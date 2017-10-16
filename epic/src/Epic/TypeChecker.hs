module Epic.TypeChecker where

import Debug.Trace

import           Control.Applicative
import           Control.Lens hiding (Context)
import           Control.Monad.Except
import           Control.Monad.State

import           Data.Char (isLower)
import           Data.Functor.Foldable
import           Data.List
import           Data.Monoid
import qualified Data.Text as T

import           Epic.Language
import           Epic.PrettyPrinter
import           Epic.Resolver

toMeta :: Functor f => Fix f -> Fix (MetaF f)
toMeta = cata (Fix . MetaBase)

substMeta :: Functor f => Int -> Fix (MetaF f) -> Fix (MetaF f) -> Fix (MetaF f)
substMeta i repl = cata $ \case
  MetaIndexF i'| i == i' -> repl
  m -> Fix m

substMetas :: Functor f => [(Int, Fix (MetaF f))] -> Fix (MetaF f)
           -> Fix (MetaF f)
substMetas ms m = foldr (\(i,t') t -> substMeta i t' t) m ms

fromMetaType :: [(Int, MetaType)] -> MetaType -> Either T.Text Type
fromMetaType mts mt = do
    let mt' = substMetas mts mt
        indices = cata collectIndices mt'
    t <- go 0 indices mt'
    return $ addUniversals indices t

  where
    collectIndices :: MetaF (TypePF tyref) [Int] -> [Int]
    collectIndices = \case
      MetaIndexF i -> [i]
      MetaBase (FunctionTypeF is is') -> is `union` is'
      MetaBase (UniversalTypeF is) -> is
      MetaBase (TypeApplicationF is is') -> is `union` is'
      _ -> []

    go :: Int -> [Int] -> MetaType -> Either T.Text Type
    go base indices = \case
      MetaIndex mi ->
        case elemIndex mi indices of
          Nothing -> Left "fromMetaType: the impossible happened"
          Just dbi -> return $ TypeVariable $ base + dbi
      TypeVariableM dbi -> return $ TypeVariable dbi
      FunctionTypeM t t' ->
        FunctionType <$> go base indices t <*> go base indices t'
      UniversalTypeM t -> UniversalType <$> go (base + 1) indices t
      PrimTypeBoolM -> return PrimTypeBool
      PrimTypeIntM -> return PrimTypeInt
      TypeConstructorM ref -> return $ TypeConstructor ref
      TypeApplicationM t t' ->
        TypeApplication <$> go base indices t <*> go base indices t'

    addUniversals :: [a] -> Type -> Type
    addUniversals [] t = t
    addUniversals (_ : xs) t = addUniversals xs (UniversalType t)

fromMetaKind :: [(Int, MetaKind)] -> MetaKind -> Kind
fromMetaKind mks mk = cata phi $ substMetas mks mk
  where
    phi :: MetaF KindF Kind -> Kind
    phi = \case
      MetaIndexF _ -> Star
      MetaBase k   -> Fix k

type TypeChecker meta final
  = StateT (Int, [(Int, meta)], [((ModuleName, T.Text), final)]) (Either T.Text)

type Context = [MetaType]

emptyContext :: Context
emptyContext = []

getVariable :: MonadError T.Text m => [a] -> Int -> m a
getVariable ctx i = case ctx ^? element i of
  Nothing -> throwError "variable index out of bound"
  Just ty -> return ty

newMetaVar :: TypeChecker (Fix (MetaF f)) (Fix f) (Int, Fix (MetaF f))
newMetaVar = do
  (i, mts) <- get
  put (i + 1, mts)
  return (i, MetaIndex i)

addMetaType :: Int -> MetaType -> TypeChecker MetaType Type ()
addMetaType = addMeta checkOccurence
  where
    checkOccurence :: Int -> MetaType -> Bool
    checkOccurence i = \case
      MetaIndex i' -> i == i'
      TypeVariableM _ -> False
      FunctionTypeM t t' -> checkOccurence i t || checkOccurence i t'
      UniversalTypeM t -> checkOccurence i t
      PrimTypeBoolM -> False
      PrimTypeIntM -> False
      TypeConstructorM _ -> False
      TypeApplicationM t t' -> checkOccurence i t || checkOccurence i t'

addMetaKind :: Int -> MetaKind -> TypeChecker MetaKind ()
addMetaKind = addMeta checkOccurence
  where
    checkOccurence :: Int -> MetaKind -> Bool
    checkOccurence i = \case
      MetaIndex i' -> i == i'
      StarM -> False
      ArrowM k k' -> checkOccurence i k || checkOccurence i k'

addMeta :: (Functor f, Pretty (Fix (MetaF f))) => (Int -> Fix (MetaF f) -> Bool)
        -> Int -> Fix (MetaF f) -> TypeChecker (Fix (MetaF f)) ()
addMeta checkOccurence i m = do
  (n, ms) <- get
  -- We want to ensure an invariant: references between elements of the list
  -- only go from right to left.
  let m' = substMetas ms m
  case m' of
    MetaIndex i' | i == i' ->
      return () -- Could happen after substitutions, we just discard those.
    _ -> do
      when (checkOccurence i m') $ throwError $
        "self-referencing ?" <> T.pack (show i) <> " = " <> prettyPrint m'
      put (n, (i, m') : ms)

findMeta :: Int -> TypeChecker meta (Maybe meta)
findMeta i = do
  (_, ms) <- get
  return $ lookup i ms

getMeta :: Int -> TypeChecker (Fix (MetaF f)) (Fix (MetaF f))
getMeta i = do
  mm <- findMeta i
  case mm of
    Nothing -> return $ MetaIndex i
    Just m  -> return m

typeOf :: Environment -> FQTerm -> Either T.Text (Term, Type)
typeOf env term = do
    ((te, mt), (_, mts)) <- runStateT (go emptyContext term) (0, [])
    (te,) <$> fromMetaType mts mt

  where
    go :: Context -> FQTerm -> TypeChecker MetaType (Term, MetaType)
    go ctx t = traceShow t $ case t of
      Variable i -> getVariable ctx i
      Reference ref -> toMeta <$> getRefType env ref

      Abstraction (Just tyIn) te -> do
        let tyIn' = toMeta tyIn
        tyOut <- go (tyIn' : ctx) te
        return $ FunctionTypeM tyIn' tyOut

      Abstraction Nothing te -> do
        (_, tyIn) <- newMetaVar
        let ctx' = tyIn : map (bumpTypeIndexFrom 0) ctx
        tyOut <- go ctx' te
        return $ FunctionTypeM tyIn tyOut

      Application te1 te2 -> do
        ty1 <- go ctx te1
        ty2 <- go ctx te2
        (i, tyU) <- newMetaVar
        unify (FunctionTypeM ty2 tyU) ty1
        getMeta i

      IfThenElse tec te1 te2 -> do
        tyc <- go ctx tec
        ty1 <- go ctx te1
        ty2 <- go ctx te2
        unify tyc PrimTypeBoolM
        (i, tyU) <- newMetaVar
        unify tyU ty1
        unify tyU ty2
        getMeta i

      PrimBool _ -> return PrimTypeBoolM
      PrimInt _ -> return PrimTypeIntM
      FixTerm ->
        -- fix : forall a. (a -> a) -> a
        return (UniversalTypeM (FunctionTypeM
                                (FunctionTypeM (TypeVariableM 0)
                                               (TypeVariableM 0))
                                (TypeVariableM 0)))

-- | Unify the two given types and adapt the constraints in the state.
-- The first given type is a subtype of the second.
unify :: MetaType -> MetaType -> TypeChecker MetaType ()
unify l r = traceShow (prettyPrint l, prettyPrint r) $ case (l, r) of
    (TypeVariableM i, TypeVariableM i') | i == i' -> return ()

    (MetaIndex i, _) -> unifyMetaIndex i r
    (_, MetaIndex i) -> unifyMetaIndex i l

    (FunctionTypeM tyIn tyOut, FunctionTypeM tyIn' tyOut') -> do
      unify tyIn' tyIn
      unify tyOut tyOut'

    (UniversalTypeM ty1, UniversalTypeM ty2) -> do
      (_, mt1) <- newMetaVar
      (_, mt2) <- newMetaVar
      let ty1' = substDeBruijnIndex 0 [(0, mt1)] ty1
          ty2' = substDeBruijnIndex 0 [(0, mt2)] ty2
      unify ty1' ty2'

    (FunctionTypeM tyIn tyOut, UniversalTypeM ty) ->
      universalHelper 1 tyIn tyOut ty

    (ty1, UniversalTypeM ty2) -> do
      (ty2', _) <- monomorphiseType 0 [] ty2
      unify ty1 ty2'

    -- FIXME: (UniversalTypeM ty1, ty2) -> remove potentially useless forall

    (PrimTypeBoolM, PrimTypeBoolM) -> return ()
    (PrimTypeIntM, PrimTypeIntM) -> return ()
    (TypeConstructorM ref, TypeConstructorM ref')
      | ref == ref' -> return ()
    (TypeApplicationM ty1 ty1', TypeApplicationM ty2 ty2') -> do
      unify ty1  ty2
      unify ty1' ty2'

    _ -> throwError $
      "can't unify type " <> prettyPrint l <> "\n\twith " <> prettyPrint r

  where
    universalHelper :: Int -> MetaType -> MetaType
                    -> MetaType -> TypeChecker MetaType ()
    universalHelper count tyIn tyOut = \case
      UniversalTypeM ty -> universalHelper (count+1) tyIn tyOut ty
      FunctionTypeM tyIn' tyOut' -> traceShow ("---", tyIn', tyOut', tyIn, tyOut) $ do
        (tyInMono, repl) <- monomorphiseType 0 [] tyIn'
        let tyOut'' = addUniversals (count - length repl) $
                        substDeBruijnIndex 0 repl tyOut'
        unify (FunctionTypeM tyIn tyOut) (FunctionTypeM tyInMono tyOut'')
      mt -> do
        (_, mts) <- get
        t <- lift $ fromMetaType mts mt
        throwError $ "can't unify universal type with " <> prettyPrint t

    -- It takes a type without the leading UniversalType constructors,
    -- replaces the type variables by meta variables and returns the newly
    -- constructed type together with the list of replacements.
    -- It does not replace type variables of other UniversalType it encounters.
    monomorphiseType :: Int -> [(Int, MetaType)] -> MetaType
                     -> TypeChecker MetaType (MetaType, [(Int, MetaType)])
    monomorphiseType threshold acc = \case
      t@(MetaIndex _) -> return (t, acc)
      t@(TypeVariableM i)
        | i >= threshold ->
            case lookup i acc of
              Nothing -> do
                (_, mvar) <- newMetaVar
                return (mvar, [(i, mvar)])
              Just mt -> return (mt, acc)
        | otherwise -> return (t, acc)
      FunctionTypeM t1 t2 -> do
        (t1', acc')  <- monomorphiseType threshold acc  t1
        (t2', acc'') <- monomorphiseType threshold acc' t2
        return (FunctionTypeM t1' t2', acc'')
      UniversalTypeM t -> do
        (t', acc') <- monomorphiseType (threshold + 1) acc t
        return (UniversalTypeM t', acc')
      PrimTypeBoolM -> return (PrimTypeBoolM, acc)
      PrimTypeIntM -> return (PrimTypeIntM, acc)
      TypeConstructorM ref -> return (TypeConstructorM ref, acc)
      TypeApplicationM t1 t2 -> do
        (t1', acc')  <- monomorphiseType threshold acc  t1
        (t2', acc'') <- monomorphiseType threshold acc' t2
        return (TypeApplicationM t1' t2', acc'')

    addUniversals :: Int -> MetaType -> MetaType
    addUniversals 0 = id
    addUniversals i = UniversalTypeM . addUniversals (i-1)

    substDeBruijnIndex :: Int -> [(Int, MetaType)] -> MetaType -> MetaType
    substDeBruijnIndex base repl = cata $ \case
      MetaBase (TypeVariableF i) ->
        case lookup (i - base) repl of
          Just mt -> mt
          Nothing ->
            let count = foldl (\c (i',_) -> if i' + base < i then c + 1 else 0)
                              0 repl
            in TypeVariableM (i - count)
      mt -> Fix mt

    unifyMetaIndex :: Int -> MetaType -> TypeChecker MetaType ()
    unifyMetaIndex i t = do
      mMT <- findMeta i
      case mMT of
        Nothing -> addMetaType i t
        Just mt -> unify mt t

-- | Increase the De Bruijn indices by 1 if they are above a threshold.
bumpTypeIndexFrom :: Int -> MetaType -> MetaType
bumpTypeIndexFrom from = \case
  TypeVariableM i | i >= from -> TypeVariableM (i + 1)
  FunctionTypeM t t' ->
    FunctionTypeM (bumpTypeIndexFrom from t) (bumpTypeIndexFrom from t')
  UniversalTypeM t -> UniversalTypeM (bumpTypeIndexFrom (from+1) t)
  TypeApplicationM t t' ->
    TypeApplicationM (bumpTypeIndexFrom from t) (bumpTypeIndexFrom from t')
  t -> t

kindCheckTypeDefinition :: Environment -> TypeDefinition Type ()
                        -> Either T.Text (TypeDefinition Type Kind)
kindCheckTypeDefinition env def = do
    (ks, (_, kcs)) <- flip runStateT (0, []) $ do
      vars <- foldM (\acc _ -> (: acc) <$> newMetaVar)
                    [] (def ^. variables)
      let ownRef = FQRef (env ^. localModuleName) (def ^. typeName)
      mapM_ (kindCheck (Just (ownRef, ownKind vars)) env
                       (map snd vars))
            (def ^. constructors . each . _2)
      foldM (\acc -> fmap (: acc) . getMeta . fst) [] vars
    return $ def & variables .~ map (fromMetaKind kcs) ks

  where
    ownKind :: [(Int, MetaKind)] -> MetaKind
    ownKind = foldr ArrowM StarM . map snd

kindOf :: Environment -> Type -> Either T.Text Kind
kindOf env typ = do
  (k, (_, ks)) <- runStateT (kindCheck Nothing env [] typ) (0,[])
  return $ fromMetaKind ks k

-- | Kind check a type in a context. The first argument is used when kind
-- checking a type definition and contains the name of the type being kind
-- checked and its own meta kind.
kindCheck:: Maybe (FQRef, MetaKind) -> Environment -> [MetaKind] -> Type
         -> TypeChecker MetaKind MetaKind
kindCheck mOwn env vars = \case
  TypeVariable i -> getVariable vars i
  FunctionType t t' -> do
    k  <- kindCheck mOwn env vars t
    k' <- kindCheck mOwn env vars t'
    unifyKind k  StarM
    unifyKind k' StarM
    return StarM
  UniversalType t -> do
    (_, ku) <- newMetaVar
    k <- kindCheck mOwn env (ku : vars) t
    unifyKind k StarM
    return StarM
  PrimTypeBool -> return StarM
  PrimTypeInt -> return StarM
  TypeConstructor ref -> case mOwn of
    Just (ownRef, ownKind) | ref == ownRef -> return ownKind
    _ -> toMeta <$> getRefKind env ref
  TypeApplication t t' -> do
    k  <- kindCheck mOwn env vars t
    k' <- kindCheck mOwn env vars t'
    (i, ku) <- newMetaVar
    unifyKind k (ArrowM k' ku)
    getMeta i

unifyKind :: MetaKind -> MetaKind -> TypeChecker MetaKind ()
unifyKind l r = case (l, r) of
    (MetaIndex i, _) -> unifyMetaIndex i r
    (_, MetaIndex i) -> unifyMetaIndex i l

    (ArrowM kIn kOut, ArrowM kIn' kOut') -> do
      unifyKind kIn  kIn'
      unifyKind kOut kOut'

    (StarM, StarM) -> return ()

    _ -> throwError $
      "can't unify kind " <> prettyPrint l <> "\n\twith " <> prettyPrint r

  where
    unifyMetaIndex :: Int -> MetaKind -> TypeChecker MetaKind ()
    unifyMetaIndex i k = do
      mMK <- findMeta i
      case mMK of
        Nothing -> addMetaKind i k
        Just mk -> unifyKind mk k

typeOfModule :: MonadError T.Text m => Maybe [T.Text]
             -> Environment -> FQModule -> m TypedModule
typeOfModule mForeignNames initEnv _mod = either throwError return $ do
    let emptyMod = _mod & types       .~ []
                        & definitions .~ []
    res <- foldM typeStep (initEnv, emptyMod) (_mod ^. types)
    snd <$> foldM termStep res (_mod ^. definitions)

  where
    typeStep :: (Environment, TypedModule) -> TypeDefinition Type ()
             -> Either T.Text (Environment, TypedModule)
    typeStep (env, typedMod) def = do
      def' <- kindCheckTypeDefinition env def
      let env' = env & localTypes %~ (def' :)
          typedMod' = case _mod ^. exports of
            Just names | def ^. typeName `notElem` names -> typedMod
            _ -> typedMod & types %~ (def' :)
      traceShow (def ^. typeName, def' ^. variables) $ return (env', typedMod')

    termStep :: (Environment, TypedModule) -> Definition FQRef (Maybe Type)
             -> Either T.Text (Environment, TypedModule)
    termStep (env, typedMod) def = do
      (pair, def') <- case def of
        TermDefinition name term mType -> do
          inferred <- typeOf env term
          typ <- case mType of
            Nothing -> return inferred
            Just t  -> do
              checkMoreSpecific t inferred
              return t

          kind <- kindOf env typ
          case kind of
            Star -> return ()
            _    -> throwError $ name <> " has kind " <> prettyPrint kind
                                      <> " but should have kind *"

          return ((name, typ), TermDefinition name term typ)

        ForeignDefinition name typ -> do
          case mForeignNames of
            Just names | name `notElem` names ->
              throwError $ "foreign value " <> name
                           <> " not available in context"
            _ -> return ()

          return ((name, typ), ForeignDefinition name typ)

      let env' = env & localBindings %~ (pair :)
          typedMod' = case _mod ^. exports of
            Just names | def ^. defName `notElem` names -> typedMod
            _ -> typedMod & definitions %~ (def' :)
      return (env', typedMod')

    checkMoreSpecific :: Type -> Type -> Either T.Text ()
    checkMoreSpecific s g =
      void $ runStateT (unify (toMeta s) (toMeta g)) (0, [])

typeCheckModules :: MonadError T.Text m => Maybe [T.Text] -> [FQModule]
                 -> m [TypedModule]
typeCheckModules mForeignNames = go [] <=< reorderModules
  where
    go :: MonadError T.Text m => [TypedModule] -> [FQModule] -> m [TypedModule]
    go acc [] = return acc
    go typedModules (_mod : modules) = do
      env <- buildEnvironment typedModules (_mod ^. moduleName)
                              (_mod ^. imports)
      typedModule <- typeOfModule mForeignNames env _mod
      go (typedModule : typedModules) modules
