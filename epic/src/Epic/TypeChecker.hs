{-# LANGUAGE TemplateHaskell #-}

module Epic.TypeChecker where

import Debug.Trace

import           Control.Applicative
import           Control.Lens hiding (Context)
import           Control.Monad.Except
import           Control.Monad.State

import           Data.Functor.Foldable
import           Data.List
import           Data.Monoid
import qualified Data.Text as T

import           Epic.Language
import           Epic.PrettyPrinter

-- FIXME: Either Int T.Text -> Int: all references' types are known

-- | Environment of the currently typechecked term. The typed modules are in the
-- reversed order of the import statements.
data Environment = Environment
  { _typedModules  :: [TypedModule]
  , _localBindings :: [(T.Text, Type)]
  }

makeLenses ''Environment

getRefType :: MonadError T.Text m => Environment -> Reference -> m Type
getRefType env ref@(NameReference name) = do
  let mLocal = lookup name $ env ^. localBindings
      mImported = findOf (typedModules . each . definitions . each)
                         ((==name) . (^.defName)) env ^? _Just . defType
  case mLocal <|> mImported of
    Nothing  -> throwError $ "can't find reference " <> ppReference ref
    Just typ -> return typ

buildEnvironment :: MonadError T.Text m => [TypedModule] -> [ModuleName]
                 -> m Environment
buildEnvironment allModules =
    fmap (\tms -> Environment tms []) . go []
  where
    go :: MonadError T.Text m => [TypedModule] -> [ModuleName]
       -> m [TypedModule]
    go acc [] = return acc
    go acc (name : names) = do
      case find ((== name) . _moduleName) allModules of
        Nothing -> throwError $ "can't find module " <> T.intercalate "." name
                                <> " when building type checking environment"
        Just _mod -> go (_mod : acc) names -- reversed order!

type TypeChecker = StateT (Int, [(Either Int T.Text, MetaType)]) (Either T.Text)

toMetaType :: Type -> MetaType
toMetaType = cata $ \case
  TypeVariableF i -> TypeVariableM i
  FunctionTypeF t t' -> FunctionTypeM t t'
  UniversalTypeF t -> UniversalTypeM t
  PrimTypeBoolF -> PrimTypeBoolM
  PrimTypeIntF -> PrimTypeIntM
  TypeConstructorF ref -> TypeConstructorM ref
  TypeApplicationF t t' -> TypeApplicationM t t'

substMetaType :: Either Int T.Text -> MetaType -> MetaType -> MetaType
substMetaType i repl = cata $ \case
  MetaIndexF i'| i == i' -> repl
  mt -> Fix mt

substMetaTypes :: [(Either Int T.Text, MetaType)] -> MetaType -> MetaType
substMetaTypes mts mt = foldr (\(i,t') t -> substMetaType i t' t) mt mts

fromMetaType :: [(Either Int T.Text, MetaType)] -> MetaType
             -> Either T.Text Type
fromMetaType mts mt = do
    let mt' = substMetaTypes mts mt
        indices = cata collectIndices mt'
    t <- go 0 indices mt'
    return $ addUniversals indices t

  where
    collectIndices :: MetaF TypeF [Int] -> [Int]
    collectIndices = \case
      MetaIndexF (Left i) -> [i]
      MetaBase (FunctionTypeF is is') -> is `union` is'
      MetaBase (UniversalTypeF is) -> is
      MetaBase (TypeApplicationF is is') -> is `union` is'
      _ -> []

    go :: Int -> [Int] -> MetaType -> Either T.Text Type
    go base indices = \case
      MetaIndex (Left mi) ->
        case elemIndex mi indices of
          Nothing -> Left "fromMetaType: the impossible happened"
          Just dbi -> return $ TypeVariable $ base + dbi
      MetaIndex (Right ref) ->
        Left $ "can't resolve type of " <> ref
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
    addUniversals (_ : xs) t = UniversalType (addUniversals xs t)

type Context = [MetaType]

emptyContext :: Context
emptyContext = []

getVariableType :: Context -> Int -> Either T.Text MetaType
getVariableType ctx i = case ctx ^? element i of
  Nothing -> Left "variable index out of bound"
  Just ty -> Right ty

newMetaVar :: TypeChecker (Int, MetaType)
newMetaVar = do
  (n, mts) <- get
  put (n + 1, mts)
  return (n, MetaIndex (Left n))

checkOccurence :: Either Int T.Text -> MetaType -> Bool
checkOccurence i = \case
  MetaIndex i' -> i == i'
  TypeVariableM _ -> False
  FunctionTypeM t t' -> checkOccurence i t || checkOccurence i t'
  UniversalTypeM t -> checkOccurence i t
  PrimTypeBoolM -> False
  PrimTypeIntM -> False
  TypeConstructorM _ -> False
  TypeApplicationM t t' -> checkOccurence i t || checkOccurence i t'

addMetaType :: Either Int T.Text -> MetaType -> TypeChecker ()
addMetaType i t = do
  (n, mts) <- get
  -- We want to ensure an invariant: references between elements of the list
  -- only go from right to left.
  let t' = substMetaTypes mts t
  case t' of
    MetaIndex i' | i == i' ->
      return () -- Could happen after substitutions, we just discard those.
    _ -> do
      when (checkOccurence i t') $ throwError $
        "self-referencing meta type ?" <> T.pack (show i)
                                       <> " = " <> ppMetaType t'
      traceShow (i, t') $ put (n, (i, t') : mts)

findMetaType :: Either Int T.Text -> TypeChecker (Maybe MetaType)
findMetaType i = do
  (_, mts) <- get
  return $ lookup i mts

getMetaType :: Either Int T.Text -> TypeChecker MetaType
getMetaType i = do
  mMT <- findMetaType i
  case mMT of
    Nothing -> return $ MetaIndex i
    Just mt -> return mt

typeOfModule :: MonadError T.Text m => Maybe [T.Text]
             -> Environment -> Module -> m TypedModule
typeOfModule mForeignNames env _mod = either throwError return $ do
    let emptyMod = _mod & types       .~ []
                        & definitions .~ []
    res <- foldM typeStep (env, emptyMod) (_mod ^. types)
    snd <$> foldM termStep res (_mod ^. definitions)

  where
    typeStep :: (Environment, TypedModule) -> TypeDefinition ()
             -> Either T.Text (Environment, TypedModule)
    typeStep (env, typedMod) def = do
      return (env, typedMod) -- FIXME


    termStep :: (Environment, TypedModule) -> Definition (Maybe Type)
             -> Either T.Text (Environment, TypedModule)
    termStep (env, typedMod) = \case
      TermDefinition name term mType -> do
        (mt, (_, mts)) <-
          runStateT (typeOf' env emptyContext term) (0, [])
        inferred <- fromMetaType mts mt

        typ <- case mType of
          Nothing -> return inferred
          Just t  -> do
            checkMoreSpecific t inferred
            return t

        let env' = env & localBindings %~ ((name, typ) :)
            typedMod' =
              typedMod & definitions %~ (TermDefinition name term typ :)
        traceShow (name, ppType typ) $ return (env', typedMod')

      ForeignDefinition name typ -> do
        case mForeignNames of
          Just names | name `notElem` names ->
            throwError $ "foreign value " <> name <> " not available in context"
          _ -> return ()

        let env' = env & localBindings %~ ((name, typ) :)
            typedMod' =
              typedMod & definitions %~ (ForeignDefinition name typ :)
        return (env', typedMod')

    checkMoreSpecific :: Type -> Type -> Either T.Text ()
    checkMoreSpecific s g =
      void $ runStateT (unify (toMetaType s) (toMetaType g)) (0, [])

typeOf :: Environment -> Term -> Either T.Text Type
typeOf env term = do
  (mt, (_, mts)) <- runStateT (typeOf' env emptyContext term) (0, [])
  traceShow (">>", ppMetaType mt) $ fromMetaType mts mt

typeOf' :: Environment -> Context -> Term -> TypeChecker MetaType
typeOf' env ctx t = traceShow t $ case t of
  Variable i -> lift $ getVariableType ctx i
  Reference ref -> toMetaType <$> getRefType env ref

  Abstraction (Just tyIn) te -> do
    let tyIn' = toMetaType tyIn
    tyOut <- typeOf' env (tyIn' : ctx) te
    return $ FunctionTypeM tyIn' tyOut

  Abstraction Nothing te -> do
    (_, tyIn) <- newMetaVar
    let ctx' = tyIn : map (bumpTypeIndexFrom 0) ctx
    tyOut <- typeOf' env ctx' te
    return $ FunctionTypeM tyIn tyOut

  Application te1 te2 -> do
    ty1 <- typeOf' env ctx te1
    ty2 <- typeOf' env ctx te2
    (i, tyU) <- newMetaVar
    unify (FunctionTypeM ty2 tyU) ty1
    getMetaType $ Left i

  IfThenElse tec te1 te2 -> do
    tyc <- typeOf' env ctx tec
    ty1 <- typeOf' env ctx te1
    ty2 <- typeOf' env ctx te2
    unify tyc PrimTypeBoolM
    (i, tyU) <- newMetaVar
    unify tyU ty1
    unify tyU ty2
    getMetaType $ Left i

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
unify :: MetaType -> MetaType -> TypeChecker ()
unify l r = traceShow (ppMetaType l, ppMetaType r) $ case (l, r) of
    (TypeVariableM i, TypeVariableM i') | i == i' -> return ()

    (MetaIndex i, _) -> unifyMetaIndex i r
    (_, MetaIndex i) -> unifyMetaIndex i l

    (FunctionTypeM tyIn tyOut, FunctionTypeM tyIn' tyOut') -> do
      unify tyIn' tyIn
      unify tyOut tyOut'

    (FunctionTypeM tyIn tyOut, UniversalTypeM ty) ->
      universalHelper 1 tyIn tyOut ty

    (UniversalTypeM ty1, UniversalTypeM ty2) -> do
      (_, mt1) <- newMetaVar
      (_, mt2) <- newMetaVar
      let ty1' = substDeBruijnIndex 0 [(0, mt1)] ty1
          ty2' = substDeBruijnIndex 0 [(0, mt2)] ty2
      unify ty1' ty2'

    (PrimTypeBoolM, PrimTypeBoolM) -> return ()
    (PrimTypeIntM, PrimTypeIntM) -> return ()
    (TypeConstructorM (NameReference n), TypeConstructorM (NameReference n'))
      | n == n' -> return ()
    (TypeApplicationM ty1 ty1', TypeApplicationM ty2 ty2') -> do
      unify ty1 ty1'
      unify ty2 ty2'

    _ -> do
      (_, mts) <- get
      l' <- lift $ fromMetaType mts l
      r' <- lift $ fromMetaType mts r
      throwError $ "can't unify type " <> ppType l' <> "\n\twith " <> ppType r'

  where
    universalHelper :: Int -> MetaType -> MetaType
                    -> MetaType -> TypeChecker ()
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
        throwError $ "can't unify universal type with " <> ppType t

    -- It takes a type without the leading UniversalType constructors,
    -- replaces the type variables by meta variables and returns the newly
    -- constructed type together with the list of replacements.
    -- It does not replace type variables of other UniversalType it encounters.
    monomorphiseType :: Int -> [(Int, MetaType)] -> MetaType
                     -> TypeChecker (MetaType, [(Int, MetaType)])
    monomorphiseType threshold acc = \case
      t@(MetaIndex _) -> return (t, acc)
      TypeVariableM i
        | i >= threshold ->
            case lookup i acc of
              Nothing -> do
                (_, mvar) <- newMetaVar
                return (mvar, [(i, mvar)])
              Just mt -> return (mt, acc)
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

    -- FIXME: simpler with a catamorphism
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

unifyMetaIndex :: Either Int T.Text -> MetaType -> TypeChecker ()
unifyMetaIndex i t = do
  mMT <- findMetaType i
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

typeCheckModules :: MonadError T.Text m => Maybe [T.Text] -> [Module]
                 -> m [TypedModule]
typeCheckModules mForeignNames = go [] <=< reorderModules [] id
  where
    go :: MonadError T.Text m => [TypedModule] -> [Module] -> m [TypedModule]
    go acc [] = return acc
    go typedModules (_mod : modules) = do
      env <- buildEnvironment typedModules (_mod ^. imports)
      typedModule <- typeOfModule mForeignNames env _mod
      go (typedModule : typedModules) modules

    -- Reorder modules so that modules only depend on modules to the left.
    reorderModules :: MonadError T.Text m => [ModuleName]
                   -> ([Module] -> [Module]) -> [Module] -> m [Module]
    reorderModules _ acc [] = return $ acc []
    reorderModules dependents acc (_mod : modules) = do
      when (anyOf (imports . traverse) (`elem` dependents) _mod) $
        throwError "circular dependency detected"

      let importNames = _mod ^. imports
          (imported, others) =
            partition ((`elem` importNames) . _moduleName) modules
      if null imported
        then reorderModules (delete (_mod ^. moduleName) dependents)
                            (acc . (_mod :)) modules
        else reorderModules (_mod ^. moduleName : dependents) acc
                            (imported ++ _mod : others)
