{-# LANGUAGE TemplateHaskell #-}

module Epic.TypeChecker where

import Debug.Trace

import           Control.Applicative
import           Control.Lens hiding (Context)
import           Control.Monad.Except
import           Control.Monad.State

import           Data.List
import           Data.Monoid
import qualified Data.Text as T

import           Epic.Language
import           Epic.PrettyPrinter

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

data MetaIndex
  = DeBruijnIndex Int
  | MetaIndex (Either Int T.Text)
  deriving (Eq, Show)

type MetaType = TypeI MetaIndex

type TypeChecker = StateT (Int, [(Either Int T.Text, MetaType)]) (Either T.Text)

toMetaType :: Type -> MetaType
toMetaType = \case
  TypeVariable i -> TypeVariable (DeBruijnIndex i)
  FunctionType t t' -> FunctionType (toMetaType t) (toMetaType t')
  UniversalType t -> UniversalType (toMetaType t)
  PrimTypeBool -> PrimTypeBool
  PrimTypeInt -> PrimTypeInt
  TypeConstructor ref -> TypeConstructor ref
  TypeApplication t t' -> TypeApplication (toMetaType t) (toMetaType t')

substMetaType :: Either Int T.Text -> MetaType -> MetaType -> MetaType
substMetaType i repl = \case
  mt@(TypeVariable (DeBruijnIndex _)) -> mt
  mt@(TypeVariable (MetaIndex i'))
    | i == i'   -> repl
    | otherwise -> mt
  FunctionType t t' ->
    FunctionType (substMetaType i repl t) (substMetaType i repl t')
  UniversalType t -> UniversalType (substMetaType i repl t)
  PrimTypeBool -> PrimTypeBool
  PrimTypeInt -> PrimTypeInt
  TypeConstructor ref -> TypeConstructor ref
  TypeApplication t t' ->
    TypeApplication (substMetaType i repl t) (substMetaType i repl t')

substMetaTypes :: [(Either Int T.Text, MetaType)] -> MetaType -> MetaType
substMetaTypes mts mt = foldr (\(i,t') t -> substMetaType i t' t) mt mts

fromMetaType :: [(Either Int T.Text, MetaType)] -> MetaType
             -> Either T.Text Type
fromMetaType mts mt = traceShow ("fromMetaType", mt, mts) $ do
    let mt' = substMetaTypes mts mt
        indices = foldl collectIndices [] mt'
    t <- go 0 indices mt'
    return $ addUniversals indices t

  where
    collectIndices :: [Int] -> MetaIndex -> [Int]
    collectIndices acc = \case
      MetaIndex (Left i) | i `elem` acc -> acc
                         | otherwise    -> i : acc
      _ -> acc

    go :: Int -> [Int] -> MetaType -> Either T.Text Type
    go base indices = \case
      TypeVariable (MetaIndex (Left mi)) ->
        case elemIndex mi indices of
          Nothing -> Left "fromMetaType: the impossible happened"
          Just dbi -> return $ TypeVariable $ base + dbi
      TypeVariable (MetaIndex (Right ref)) ->
        Left $ "can't resolve type of " <> ref
      TypeVariable (DeBruijnIndex dbi) -> return $ TypeVariable dbi
      FunctionType t t' ->
        FunctionType <$> go base indices t <*> go base indices t'
      UniversalType t -> UniversalType <$> go (base + 1) indices t
      PrimTypeBool -> return PrimTypeBool
      PrimTypeInt -> return PrimTypeInt
      TypeConstructor ref -> return $ TypeConstructor ref
      TypeApplication t t' ->
        TypeApplication <$> go base indices t <*> go base indices t'

    addUniversals :: [a] -> Type -> Type
    addUniversals [] t = t
    addUniversals (_ : xs) t = UniversalType (addUniversals xs t)

newtype Context = Context
  { _vars :: [MetaType]
  } deriving (Eq, Show)

makeLenses ''Context

emptyContext :: Context
emptyContext = Context []

getVariableType :: Context -> Int -> Either T.Text MetaType
getVariableType ctx i = case ctx ^. vars ^? element i of
  Nothing -> Left "variable index out of bound"
  Just ty -> Right ty

newMetaVar :: TypeChecker (Int, MetaType)
newMetaVar = do
  (n, mts) <- get
  put (n + 1, mts)
  return (n, TypeVariable (MetaIndex (Left n)))

checkOccurence :: Either Int T.Text -> MetaType -> Bool
checkOccurence i = \case
  TypeVariable (DeBruijnIndex _) -> False
  TypeVariable (MetaIndex i') -> i == i'
  FunctionType t t' -> checkOccurence i t || checkOccurence i t'
  UniversalType t -> checkOccurence i t
  PrimTypeBool -> False
  PrimTypeInt -> False
  TypeConstructor _ -> False
  TypeApplication t t' -> checkOccurence i t || checkOccurence i t'

addMetaType :: Either Int T.Text -> MetaType -> TypeChecker ()
addMetaType i t = do
  (n, mts) <- get
  -- We want to ensure an invariant: references between elements of the list
  -- only go from right to left.
  let t' = substMetaTypes mts t
  if t' == TypeVariable (MetaIndex i)
    then return () -- Could happen after substitutions, we just discard those.
    else do
      when (checkOccurence i t') $ throwError $ T.pack $
        "self-referencing meta type " ++ show (i, t')
      traceShow (i, t') $ put (n, (i, t') : mts)

findMetaType :: Either Int T.Text -> TypeChecker (Maybe MetaType)
findMetaType i = do
  (_, mts) <- get
  return $ lookup i mts

getMetaType :: Either Int T.Text -> TypeChecker MetaType
getMetaType i = do
  mMT <- findMetaType i
  case mMT of
    Nothing -> return $ TypeVariable $ MetaIndex i
    Just mt -> return mt

typeOfModule :: MonadError T.Text m => Maybe [T.Text]
             -> Environment -> Module -> m TypedModule
typeOfModule mForeignNames env _mod =
    either throwError return $
      snd <$> foldM step (env, _mod & definitions .~ []) (_mod ^. definitions)

  where
    step :: (Environment, TypedModule) -> Definition (Maybe Type)
         -> Either T.Text (Environment, TypedModule)
    step (env, typedMod) = \case
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
        return (env', typedMod')

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

    checkExport :: T.Text -> Bool
    checkExport name = maybe True (name `elem`) (_mod ^. exports)

typeOf :: Environment -> Term -> Either T.Text Type
typeOf env term = do
  (mt, (_, mts)) <- runStateT (typeOf' env emptyContext term) (0, [])
  traceShow (">>", mt) $ fromMetaType mts mt

typeOf' :: Environment -> Context -> Term -> TypeChecker MetaType
typeOf' env ctx t = traceShow t $ case t of
  Variable i -> lift $ getVariableType ctx i
  Reference ref -> toMetaType <$> getRefType env ref

  Abstraction (Just tyIn) te -> do
    let tyIn' = toMetaType tyIn
    tyOut <- typeOf' env (ctx & vars %~ (tyIn' :)) te
    return $ FunctionType tyIn' tyOut

  Abstraction Nothing te -> do
    (_, tyIn) <- newMetaVar
    let ctx' = ctx & vars %~ ((tyIn :) . map (onTypeIndex (+1)))
    tyOut <- typeOf' env ctx' te
    return $ FunctionType tyIn tyOut

  Application te1 te2 -> do
    ty1 <- typeOf' env ctx te1
    ty2 <- typeOf' env ctx te2
    (i, tyU) <- newMetaVar
    unify (FunctionType ty2 tyU) ty1
    getMetaType $ Left i

  IfThenElse tec te1 te2 -> do
    tyc <- typeOf' env ctx tec
    ty1 <- typeOf' env ctx te1
    ty2 <- typeOf' env ctx te2
    unify tyc PrimTypeBool
    (i, tyU) <- newMetaVar
    unify tyU ty1
    unify tyU ty2
    getMetaType $ Left i

  PrimBool _ -> return PrimTypeBool
  PrimInt _ -> return PrimTypeInt
  Fix ->
    -- fix : forall a. (a -> a) -> a
    return (UniversalType (FunctionType
                           (FunctionType (TypeVariable (DeBruijnIndex 0))
                                         (TypeVariable (DeBruijnIndex 0)))
                           (TypeVariable (DeBruijnIndex 0))))

-- FIXME: Unify kinds to reject more programs (e.g. fix  \f -> \x -> f x)

-- | Unify the two given types and adapt the constraints in the state.
-- The first given type is a subtype of the second.
unify :: MetaType -> MetaType -> TypeChecker ()
unify l r = traceShow (l, r) $ case (l, r) of
    (TypeVariable i, TypeVariable i') | i == i' -> return ()

    (TypeVariable (MetaIndex i), _) -> unifyMetaIndex i r
    (_, TypeVariable (MetaIndex i)) -> unifyMetaIndex i l

    (FunctionType tyIn tyOut, FunctionType tyIn' tyOut') -> do
      unify tyIn' tyIn
      unify tyOut tyOut'

    (FunctionType tyIn tyOut, UniversalType ty) ->
      universalHelper 1 tyIn tyOut ty

    (UniversalType ty1, UniversalType ty2) -> do
      (_, mt1) <- newMetaVar
      (_, mt2) <- newMetaVar
      let ty1' = substDeBruijnIndex 0 [(0, mt1)] ty1
          ty2' = substDeBruijnIndex 0 [(0, mt2)] ty2
      unify ty1' ty2'

    (PrimTypeBool, PrimTypeBool) -> return ()
    (PrimTypeInt, PrimTypeInt) -> return ()
    (TypeConstructor (NameReference n), TypeConstructor (NameReference n'))
      | n == n' -> return ()
    (TypeApplication ty1 ty1', TypeApplication ty2 ty2') -> do
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
      UniversalType ty -> universalHelper (count+1) tyIn tyOut ty
      FunctionType tyIn' tyOut' -> traceShow ("---", tyIn', tyOut', tyIn, tyOut) $ do
        (tyInMono, repl) <- monomorphiseType 0 [] tyIn'
        let tyOut'' = addUniversals (count - length repl) $
                        substDeBruijnIndex 0 repl tyOut'
        unify (FunctionType tyIn tyOut) (FunctionType tyInMono tyOut'')
      mt -> do
        (_, mts) <- get
        t <- lift $ fromMetaType mts mt
        throwError $ "can't unify universal type with " <> ppType t

    -- It takes a type where we already the leading UniversalType constructors,
    -- replaces the type variables by meta variables and returns the newly
    -- constructed type together with the list of replacements.
    -- It does not replace type variables of other UniversalType it encounters.
    monomorphiseType :: Int -> [(Int, MetaType)] -> MetaType
                     -> TypeChecker (MetaType, [(Int, MetaType)])
    monomorphiseType threshold acc = \case
      TypeVariable (DeBruijnIndex i)
        | i >= threshold ->
            case lookup i acc of
              Nothing -> do
                (_, mvar) <- newMetaVar
                return (mvar, [(i, mvar)])
              Just mt -> return (mt, acc)
      t@(TypeVariable _) -> return (t, acc)
      FunctionType t1 t2 -> do
        (t1', acc')  <- monomorphiseType threshold acc  t1
        (t2', acc'') <- monomorphiseType threshold acc' t2
        return (FunctionType t1' t2', acc'')
      UniversalType t -> do
        (t', acc') <- monomorphiseType (threshold + 1) acc t
        return (UniversalType t', acc')
      PrimTypeBool -> return (PrimTypeBool, acc)
      PrimTypeInt -> return (PrimTypeInt, acc)
      TypeConstructor ref -> return (TypeConstructor ref, acc)
      TypeApplication t1 t2 -> do
        (t1', acc')  <- monomorphiseType threshold acc  t1
        (t2', acc'') <- monomorphiseType threshold acc' t2
        return (TypeApplication t1' t2', acc'')

    addUniversals :: Int -> MetaType -> MetaType
    addUniversals 0 = id
    addUniversals i = UniversalType . addUniversals (i-1)

    substDeBruijnIndex :: Int -> [(Int, MetaType)] -> MetaType -> MetaType
    substDeBruijnIndex base repl = \case
      TypeVariable (DeBruijnIndex i) ->
        case lookup (i - base) repl of
          Just mt -> mt
          Nothing ->
            let count = foldl (\c (i',_) -> if i' + base < i then c + 1 else 0)
                              0 repl
            in TypeVariable (DeBruijnIndex (i - count))
      t@(TypeVariable _) -> t
      FunctionType t1 t2 -> FunctionType (substDeBruijnIndex base repl t1)
                                         (substDeBruijnIndex base repl t2)
      UniversalType t -> UniversalType $ substDeBruijnIndex (base+1) repl t
      PrimTypeBool -> PrimTypeBool
      PrimTypeInt -> PrimTypeInt
      TypeConstructor ref -> TypeConstructor ref
      TypeApplication t1 t2 -> TypeApplication (substDeBruijnIndex base repl t1)
                                               (substDeBruijnIndex base repl t2)

-- FIXME: Check for self-reference
unifyMetaIndex :: Either Int T.Text -> MetaType -> TypeChecker ()
unifyMetaIndex i t = do
  mMT <- findMetaType i
  case mMT of
    Nothing -> addMetaType i t
    Just mt -> unify mt t

-- | Apply a function on the De Bruijn index if any.
onTypeIndex :: (Int -> Int) -> MetaType -> MetaType
onTypeIndex f = \case
  TypeVariable (DeBruijnIndex i) -> TypeVariable (DeBruijnIndex (f i))
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

    buildGetRefType :: MonadError T.Text m => (Reference -> Maybe Type)
                    -> [(ModuleName, [(T.Text, Type)])] -> [ModuleName]
                    -> m (Reference -> Maybe Type)
    buildGetRefType acc _ [] = return acc
    buildGetRefType acc modTypes (name : names) = do
      case lookup name modTypes of
        Nothing -> throwError $
          "typeCheckModules: missing module " <> T.intercalate "." name
        Just types -> return $ \ref@(NameReference refName) ->
          lookup refName types <|> acc ref
