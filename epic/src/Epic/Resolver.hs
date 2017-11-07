module Epic.Resolver
  ( ResolverEnvironment(..)
  , importedModules, localModuleName, localBindings, localTypes
  , buildEnvironment
  , resolveModules
  , resolveModule
  , resolveTypeDefinitions
  , resolveTypeDefinition
  , resolveType
  , resolveLocalType
  , resolveDefinitions
  , resolveDefinition
  , resolveTerm
  , resolvePattern
  , resolveReference
  , resolveConstructorReference
  , reorderModules
  ) where

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

data ResolverEnvironment = ResolverEnvironment
  { _importedModules :: [FQModule]
  , _localModuleName :: ModuleName
  , _localBindings   :: [FQDefinition]
  , _localTypes      :: [TypeDefinition FQType ()]
  } deriving (Eq, Show)

makeLenses ''ResolverEnvironment

buildEnvironment :: MonadError T.Text m => [FQModule] -> ModuleName
                 -> [ModuleName] -> m ResolverEnvironment
buildEnvironment allModules mname =
    fmap (\tms -> ResolverEnvironment tms mname [] []) . go []
  where
    go :: MonadError T.Text m => [FQModule] -> [ModuleName] -> m [FQModule]
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

resolveTypeDefinitions :: MonadError T.Text m => ResolverEnvironment -> Module
                       -> m (ResolverEnvironment, [TypeDefinition FQType ()])
resolveTypeDefinitions env _mod = do
  let step (env, f) def = do
        (env', fqdef) <- resolveTypeDefinition env def
        return (env', f . (fqdef :))
  (env', f) <- foldM step (env, id) (_mod ^. types)
  return (env', f [])

resolveTypeDefinition :: MonadError T.Text m => ResolverEnvironment
                      -> TypeDefinition LocalType ()
                      -> m (ResolverEnvironment, TypeDefinition FQType ())
resolveTypeDefinition env def = do
  let ownRef = (env ^. localModuleName, def ^. typeName)
  mkConstrs <- forM (def ^. constructors) $ \(name, lts) -> do
    fs <- mapM (resolveType' env (Just ownRef)) lts
    return (name, fs)
  let def' = TypeDefinition (def ^. typeName) (def ^. variables)
                            (map (\(n,fs) -> (n, map ($ def') fs)) mkConstrs)
      env' = env & localTypes %~ (def' :)
  return (env', def')

resolveType :: MonadError T.Text m => ResolverEnvironment -> LocalType
            -> m FQType
resolveType env t = do
  f <- resolveType' env Nothing t
  -- when called with Nothing, it should not use its argument
  return $ f $ error "resolveType: the impossible happened"

resolveType' :: MonadError T.Text m => ResolverEnvironment
             -> Maybe (ModuleName, T.Text) -> LocalType
             -> m (TypeDefinition FQType () -> FQType)
resolveType' env mOwnRef = fmap (fmap FQType) . cata phi
  where
    phi :: MonadError T.Text m
        => TypePF LocalReference
                  (m (TypeDefinition FQType ()
                      -> (TypeP (Ref (TypeDefinition FQType ())))))
        -> m (TypeDefinition FQType () -- itself
              -> (TypeP (Ref (TypeDefinition FQType ()))))
    phi = \case
      TypeVariableF i -> return $ const $ TypeVariable i
      FunctionTypeF t t' ->
        (\f f' tdef -> FunctionType (f tdef) (f' tdef)) <$> t <*> t'
      UniversalTypeF t -> (\f tdef -> UniversalType (f tdef)) <$> t
      PrimTypeBoolF -> return $ const PrimTypeBool
      PrimTypeIntF -> return $ const PrimTypeInt
      TypeConstructorF ref -> do
        f <- case (mOwnRef, ref) of
          (Just (ownModuleName, ownName), NameReference name)
            | name == ownName -> return $ Ref ownModuleName ownName
          (Just (ownModuleName, ownName), FQReference mname name)
            | ownModuleName == mname && ownName == name ->
                return $ Ref ownModuleName ownName
          _ -> const <$> resolveLocalType env ref
        return $ TypeConstructor . f
      TypeApplicationF t t' ->
        (\f f' tdef -> TypeApplication (f tdef) (f' tdef)) <$> t <*> t'

resolveLocalType :: MonadError T.Text m => ResolverEnvironment
                 -> LocalReference -> m (Ref (TypeDefinition FQType ()))
resolveLocalType env = \case
  NameReference name -> do
    let mLocal = do
          typ <- find ((==name) . view typeName) (env ^. localTypes)
          return $ Ref (env ^. localModuleName) name typ
        mImported = listToMaybe $ do
          _mod <- env ^. importedModules
          typ  <- _mod ^. types
          guard $ typ ^. typeName == name
          return $ Ref (_mod ^. moduleName) name typ

    case mLocal <|> mImported of
      Nothing  -> throwError $ "can't find type " <> name
      Just ref -> return ref

  ref@(FQReference mname name) -> do
    let mType = listToMaybe $ do
          _mod <- env ^. importedModules
          guard $ _mod ^. moduleName == mname
          typ <- _mod ^. types
          guard $ typ ^. typeName == name
          return $ Ref mname name typ

    case mType of
      Nothing -> throwError $
        "can't resolve " <> prettyPrint ref
        <> ", make sure the module is imported and export this value"
      Just ref -> return ref

resolveDefinitions :: MonadError T.Text m => ResolverEnvironment -> Module
                   -> m (ResolverEnvironment, [FQDefinition])
resolveDefinitions env _mod = do
  let step (env, f) def = do
        (env', fqdef) <- resolveDefinition env def
        return (env', f . (fqdef :))
  (env', f) <- foldM step (env, id) (_mod ^. definitions)
  return (env', f [])

resolveDefinition :: MonadError T.Text m => ResolverEnvironment
                  -> LocalDefinition -> m (ResolverEnvironment, FQDefinition)
resolveDefinition env ldef = do
  def <- case ldef of
    TermDefinition name localTerm mLocalType ->
      TermDefinition name
        <$> resolveTerm env localTerm
        <*> sequence (fmap (resolveType env) mLocalType)

    ForeignDefinition name localType ->
      ForeignDefinition name <$> resolveType env localType

  let fqdef = FQDefinition def
      env' = env & localBindings %~ (fqdef :)
  return (env', fqdef)

resolveTerm :: MonadError T.Text m => ResolverEnvironment -> LocalTerm
            -> m FQTerm
resolveTerm env = go
  where
    go :: MonadError T.Text m => LocalTerm
       -> m (TermP (Ref FQDefinition) (Ref (TypeDefinition FQType ())) FQType)
    go = \case
      Variable i -> return $ Variable i
      Reference ref -> Reference <$> resolveReference env ref
      ConstructorReference ref name ->
        ConstructorReference <$> resolveConstructorReference env ref
                             <*> pure name
      Abstraction mLocalType t ->
        Abstraction <$> sequence (fmap (resolveType env) mLocalType)
                    <*> go t
      Application t t' -> Application <$> go t <*> go t'
      IfThenElse t t' t'' -> IfThenElse <$> go t <*> go t' <*> go t''
      PatternMatch t branches ->
        PatternMatch <$> go t
                     <*> mapM (\(pat, t') -> (,) <$> resolvePattern env pat
                                             <*> go t')
                              branches
      PrimBool b -> return $ PrimBool b
      PrimInt i -> return $ PrimInt i
      FixTerm -> return FixTerm
      Constructor i _ ty -> Constructor i [] <$> resolveType env ty

resolvePattern :: MonadError T.Text m => ResolverEnvironment -> LocalPattern
               -> m FQPattern
resolvePattern env = \case
  WildcardPattern -> return WildcardPattern
  VariablePattern name -> return $ VariablePattern name
  ConstructorPattern ref name patterns -> do
    ref'      <- resolveConstructorReference env ref
    patterns' <- mapM (resolvePattern env) patterns
    return $ ConstructorPattern ref' name patterns'

resolveReference :: MonadError T.Text m => ResolverEnvironment -> LocalReference
                 -> m (Ref FQDefinition)
resolveReference env lref = do
  let mRef = case lref of
        NameReference name ->
          let mLocal = do
                def <- find ((==name) . view defName . unFQDefinition)
                            (env ^. localBindings)
                return $ Ref (env ^. localModuleName) name def

              mImported = listToMaybe $ do
                _mod <- env ^. importedModules
                def  <- _mod ^. definitions
                guard $ unFQDefinition def ^. defName == name
                return $ Ref (_mod ^. moduleName) name def

          in mLocal <|> mImported

        FQReference mname name -> listToMaybe $ do
          _mod <- env ^. importedModules
          guard $ _mod ^. moduleName == mname
          def <- _mod ^. definitions
          guard $ unFQDefinition def ^. defName == name
          return $ Ref mname name def

  case mRef of
    Nothing  -> throwError $ "can't find reference " <> prettyPrint lref
    Just ref -> return ref

resolveConstructorReference :: MonadError T.Text m => ResolverEnvironment
                            -> LocalReference
                            -> m (Ref (TypeDefinition FQType ()))
resolveConstructorReference env lref = do
  let mDef = case lref of
        NameReference name ->
          let mLocal = listToMaybe $ do
                typ <- env ^. localTypes
                cname <- typ ^.. constructors . each . _1
                guard $ cname == name
                return $ Ref (env ^. localModuleName) (typ ^. typeName) typ

              mImported = listToMaybe $ do
                _mod  <- env ^. importedModules
                typ   <- _mod ^. types
                cname <- typ ^.. constructors . each . _1
                guard $ cname == name
                return $ Ref (_mod ^. moduleName) (typ ^. typeName) typ

          in mLocal <|> mImported

        FQReference mname name -> listToMaybe $ do
          _mod <- env ^. importedModules
          guard $ _mod ^. moduleName == mname
          typ <- _mod ^. types
          cname <- typ ^.. constructors . each . _1
          guard $ cname == name
          return $ Ref mname (typ ^. typeName) typ

  case mDef of
    Nothing  -> throwError $ "can't find reference " <> prettyPrint lref
    Just ref -> return ref

-- | Reorder modules so that modules only depend on modules on the left.
reorderModules :: MonadError T.Text m => [ModuleP tedef tydef]
               -> m [ModuleP tedef tydef]
reorderModules = go id []
  where
    go :: MonadError T.Text m
       => ([ModuleP tedef tydef] -> [ModuleP tedef tydef]) -> [ModuleName]
       -> [ModuleP tedef tydef] -> m [ModuleP tedef tydef]
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
