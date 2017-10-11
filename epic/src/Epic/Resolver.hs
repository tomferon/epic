{-# LANGUAGE ScopedTypeVariables #-}

module Epic.Resolver where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Except

import           Data.Char
import           Data.Functor.Foldable
import           Data.Maybe
import           Data.Monoid
import           Data.List
import qualified Data.Text as T

import           Epic.Language
import           Epic.PrettyPrinter

-- FIXME: getRefType and getRefKind should be made so they always return what is
-- requested => make FQRef directly contain the result?

data ResolverEnvironment t k = ResolverEnvironment
  { _importedModules :: [ModuleRTK FQRef t k]
  , _localModuleName :: ModuleName
  , _localBindings   :: [(T.Text, t)]
  , _localTypes      :: [TypeDefinition Type k]
  }

makeLenses ''ResolverEnvironment

buildEnvironment :: forall m t k. MonadError T.Text m => [ModuleRTK FQRef t k]
                 -> ModuleName -> [ModuleName] -> m (ResolverEnvironment t k)
buildEnvironment allModules mname =
    fmap (\tms -> ResolverEnvironment tms mname [] []) . go []
  where
    go :: [ModuleRTK FQRef t k] -> [ModuleName] -> m [ModuleRTK FQRef t k]
    go acc [] = return acc
    go acc (name : names) = do
      case find ((== name) . _moduleName) allModules of
        Nothing -> throwError $ "can't find module " <> prettyPrint name
                                <> " when building type checking environment"
        Just _mod -> go (_mod : acc) names -- reversed order!

resolveModules :: MonadError T.Text m => [Module] -> m [FQModule]
resolveModules = go [] <=< reorderModules
  where
    go :: MonadError T.Text m => [FQModule] -> [Module] -> m [FQModule]
    go acc [] = return acc
    go resolvedModules (_mod : modules) = do
      resolvedModule <- resolveModule resolvedModules _mod
      go (resolvedModule : resolvedModules) modules

resolveModule :: MonadError T.Text m => [FQModule] -> Module -> m FQModule
resolveModule resolvedModules _mod = do
  env <- buildEnvironment resolvedModules (_mod ^. moduleName) (_mod ^. imports)
  (env', types) <- resolveTypeDefinitions env _mod
  defs <- snd <$> resolveDefinitions env' _mod
  return $ Module (_mod ^. moduleName) (_mod ^. exports) (_mod ^. imports)
                  types defs

resolveTypeDefinitions :: MonadError T.Text m
                       => ResolverEnvironment (Maybe Type) ()
                       -> Module -> m ( ResolverEnvironment (Maybe Type) ()
                                      , [TypeDefinition Type ()] )
resolveTypeDefinitions env _mod = do
  let step (env, f) def = do
        (env', fqdef) <- resolveTypeDefinition env def
        return (env', f . (fqdef :))
  (env', f) <- foldM step (env, id) (_mod ^. types)
  return (env', f [])

resolveTypeDefinition :: MonadError T.Text m
                      => ResolverEnvironment (Maybe Type) ()
                      -> TypeDefinition LocalType ()
                      -> m ( ResolverEnvironment (Maybe Type) ()
                           , TypeDefinition Type () )
resolveTypeDefinition env def = do
  let ownRef = FQRef (env ^. localModuleName) (def ^. typeName)
  constrs <- forM (def ^. constructors) $ \(name, lts) -> do
    ts <- mapM (resolveType env (Just ownRef)) lts
    return (name, ts)
  let def' = TypeDefinition (def ^. typeName) (def ^. variables) constrs
      env' = env & localTypes %~ (def' :)
  return (env', def')

resolveType :: MonadError T.Text m => ResolverEnvironment (Maybe Type) ()
            -> Maybe FQRef -> LocalType -> m Type
resolveType env mOwnRef = cata $ \case
  TypeVariableF i -> return $ TypeVariable i
  FunctionTypeF t t' -> FunctionType <$> t <*> t'
  UniversalTypeF t -> UniversalType <$> t
  PrimTypeBoolF -> return PrimTypeBool
  PrimTypeIntF -> return PrimTypeInt
  TypeConstructorF ref -> fmap TypeConstructor $ case (mOwnRef, ref) of
    (Just ownRef@(FQRef _ ownName), NameReference name)
      | name == ownName -> return ownRef
    (Just ownRef@(FQRef _ ownName), FQReference ref')
      | ownRef == ref' -> return ownRef
    _ -> resolveLocalType env ref
  TypeApplicationF t t' -> TypeApplication <$> t <*> t'

resolveLocalType :: MonadError T.Text m => ResolverEnvironment (Maybe Type) ()
                 -> LocalReference -> m FQRef
resolveLocalType env = \case
  NameReference name -> do
    let mLocal = do
          typ <- find ((==name) . view typeName) (env ^. localTypes)
          return $ FQRef (env ^. localModuleName) name
        mImported = listToMaybe $ do
          _mod <- env ^. importedModules
          typ  <- _mod ^. types
          guard $ typ ^. typeName == name
          return $ FQRef (_mod ^. moduleName) name

    case mLocal <|> mImported of
      Nothing  -> throwError $ "can't find type " <> name
      Just ref -> return ref

  FQReference ref@(FQRef mname _) -> do
    when (mname `elem` env ^.. importedModules . each . moduleName) $
      throwError $ prettyPrint mname <> " is not imported"
    return ref

resolveDefinitions :: MonadError T.Text m => ResolverEnvironment (Maybe Type) ()
                   -> Module -> m ( ResolverEnvironment (Maybe Type) ()
                                  , [Definition FQRef (Maybe Type)] )
resolveDefinitions env _mod = do
  let step (env, f) def = do
        (env', fqdef) <- resolveDefinition env def
        return (env', f . (fqdef :))
  (env', f) <- foldM step (env, id) (_mod ^. definitions)
  return (env', f [])

resolveDefinition :: MonadError T.Text m => ResolverEnvironment (Maybe Type) ()
                  -> Definition LocalReference (Maybe LocalType)
                  -> m ( ResolverEnvironment (Maybe Type) ()
                       , Definition FQRef (Maybe Type) )
resolveDefinition env = \case
  TermDefinition name localTerm mLocalType -> do
    def <- TermDefinition name
      <$> resolveTerm env localTerm
      <*> sequence (fmap (resolveType env Nothing) mLocalType)
    let env' = env & localBindings %~ ((name, Nothing) :)
    return (env', def)

  ForeignDefinition name localType -> do
    def <- ForeignDefinition name <$> resolveType env Nothing localType
    let env' = env & localBindings %~ ((name, Nothing) :)
    return (env', def)

resolveTerm :: MonadError T.Text m => ResolverEnvironment (Maybe Type) ()
            -> LocalTerm -> m Term
resolveTerm env = \case
  Variable i -> return $ Variable i
  Reference ref -> Reference <$> resolveLocalReference env ref
  Abstraction mLocalType t ->
    Abstraction <$> sequence (fmap (resolveType env Nothing) mLocalType)
                <*> resolveTerm env t
  Application t t' -> Application <$> resolveTerm env t <*> resolveTerm env t'
  IfThenElse t t' t'' -> IfThenElse
    <$> resolveTerm env t <*> resolveTerm env t' <*> resolveTerm env t''
  PrimBool b -> return $ PrimBool b
  PrimInt i -> return $ PrimInt i
  FixTerm -> return FixTerm

resolveLocalReference :: MonadError T.Text m
                      => ResolverEnvironment (Maybe Type) () -> LocalReference
                      -> m FQRef
resolveLocalReference env = \case
  NameReference name -> do
    let mLocal
          | isUpper (T.head name) = listToMaybe $ do
              typ <- env ^. localTypes
              cname <- typ ^.. constructors . each . _1
              guard $ cname == name
              return $ FQRef (env ^. localModuleName) name
          | otherwise = do
              _ <- lookup name (env ^. localBindings)
              return $ FQRef (env ^. localModuleName) name

        mImported
          | isUpper (T.head name) = listToMaybe $ do
              _mod  <- env ^. importedModules
              typ   <- _mod ^. types
              cname <- typ ^.. constructors . each . _1
              guard $ cname == name
              return $ FQRef (_mod ^. moduleName) name
          | otherwise = listToMaybe $ do
              _mod <- env ^. importedModules
              def  <- _mod ^. definitions
              guard $ def ^. defName == name
              return $ FQRef (_mod ^. moduleName) name

    case mLocal <|> mImported of
      Nothing  -> throwError $ "can't find reference " <> name
      Just ref -> return ref

  FQReference ref@(FQRef mname _) -> do
    unless (mname `elem` env ^.. importedModules . each . moduleName) $
      throwError $ prettyPrint mname <> " is not imported"
    return ref

-- | Reorder modules so that modules only depend on modules to the left.
reorderModules :: MonadError T.Text m => [ModuleRTK r t k]
               -> m [ModuleRTK r t k]
reorderModules = go id []
  where
    go :: MonadError T.Text m => ([ModuleRTK r t k] -> [ModuleRTK r t k])
       -> [ModuleName] -> [ModuleRTK r t k] -> m [ModuleRTK r t k]
    go acc _ [] = return $ acc []
    go acc dependents (_mod : modules) = do
      when (anyOf (imports . traverse) (`elem` dependents) _mod) $
        throwError "circular dependency detected"

      let importNames = _mod ^. imports
          (imported, others) =
            partition ((`elem` importNames) . _moduleName) modules
      if null imported
        then go (acc . (_mod :)) (delete (_mod ^. moduleName) dependents)
                                 modules
        else go acc (_mod ^. moduleName : dependents)
                    (imported ++ _mod : others)

getRefType :: MonadError T.Text m => ResolverEnvironment Type k -> FQRef
           -> m Type
getRefType env ref@(FQRef mname name) = do
  if env ^. localModuleName == mname
    then do
      let mType = if isUpper (T.head name)
            then listToMaybe $ do
              typ <- env ^. localTypes
              (constr, paramTypes) <- typ ^. constructors
              guard $ constr == name
              return $ constructorType mname (typ ^. typeName)
                                       (typ ^. variables) paramTypes
            else lookup name (env ^. localBindings)

      case mType of
        Nothing -> throwError $ "can't resolve " <> name
        Just typ -> return typ

    else do
      let mType = if isUpper (T.head name)
            then listToMaybe $ do
              _mod <- env ^. importedModules
              guard $ _mod ^. moduleName == mname
              typ  <- _mod ^. types
              (constr, paramTypes) <- typ ^. constructors
              guard $ constr == name
              return $ constructorType mname (typ ^. typeName)
                                       (typ ^. variables) paramTypes

            else listToMaybe $ do
              _mod <- env ^. importedModules
              guard $ _mod ^. moduleName == mname
              def <- _mod ^. definitions
              guard $ def ^. defName == name
              return $ def ^. defType

      case mType of
        Nothing ->
          throwError $ "module " <> prettyPrint mname
                       <> " is not imported or it doesn't export " <> name
        Just typ -> return typ

constructorType :: ModuleName -> T.Text -> [k] -> [Type] -> Type
constructorType mname name vars paramTypes =
    let tyConstr = TypeConstructor (FQRef mname name)
        varCount = length vars
        final    = addTypeVariables varCount tyConstr
    in addUniversals varCount $ foldr FunctionType final paramTypes

  where
    addTypeVariables :: Int -> Type -> Type
    addTypeVariables 0 ty = ty
    addTypeVariables n ty =
      addTypeVariables (n-1) (TypeApplication ty (TypeVariable (n - 1)))

    addUniversals :: Int -> Type -> Type
    addUniversals 0 ty = ty
    addUniversals n ty = addUniversals (n-1) (UniversalType ty)

getRefKind :: MonadError T.Text m => ResolverEnvironment t Kind -> FQRef
           -> m Kind
getRefKind env (FQRef mname name)
  | env ^. localModuleName == mname = do
      let mTypeDef = find ((==name) . view typeName) (env ^. localTypes)
          mKind = fmap (foldr Arrow Star . view variables) mTypeDef
      case mKind of
        Nothing -> throwError $ "can't resolve " <> name
        Just kind -> return kind

  | otherwise = do
      let mKind = listToMaybe $ do
            _mod <- env ^. importedModules
            guard $ _mod ^. moduleName == mname
            typ <- _mod ^. types
            guard $ typ ^. typeName == name
            return $ foldr Arrow Star $ typ ^. variables

      case mKind of
        Nothing ->
          throwError $ "module " <> prettyPrint mname
                       <> " is not imported or it doesn't export " <> name
        Just kind -> return kind
