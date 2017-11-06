{-# LANGUAGE Rank2Types #-}

module Epic.TypeChecker where

import Debug.Trace

import           Control.Applicative
import           Control.Lens hiding (Context)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.Char (isLower)
import           Data.Functor.Foldable
import           Data.List
import           Data.Maybe
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
    return $ Type $ addUniversals indices t

  where
    collectIndices :: MetaF (TypePF tyref) [Int] -> [Int]
    collectIndices = \case
      MetaIndexF i -> [i]
      MetaBase (FunctionTypeF is is') -> is `union` is'
      MetaBase (UniversalTypeF is) -> is
      MetaBase (TypeApplicationF is is') -> is `union` is'
      _ -> []

    go :: Int -> [Int] -> MetaType
       -> Either T.Text (TypeP (Ref (TypeDefinition Type Kind)))
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

    addUniversals :: [a] -> TypeP (Ref (TypeDefinition Type Kind))
                  -> TypeP (Ref (TypeDefinition Type Kind))
    addUniversals [] t = t
    addUniversals (_ : xs) t = addUniversals xs (UniversalType t)

fromMetaKind :: [(Int, MetaKind)] -> MetaKind -> Kind
fromMetaKind mks mk = cata phi $ substMetas mks mk
  where
    phi :: MetaF KindF Kind -> Kind
    phi = \case
      MetaIndexF _ -> Star
      MetaBase k   -> Fix k

-- FIXME: Use Map and IntMap

data TypeCheckerState = TypeCheckerState
  { _nextMetaIndex     :: Int
  , _typeConstraints   :: [(Int, MetaType)]
  , _kindConstraints   :: [(Int, MetaKind)]
  , _typedDefinitions  :: [((ModuleName, T.Text), Definition)]
  , _kindedDefinitions :: [((ModuleName, T.Text), TypeDefinition Type Kind)]
  }

makeLenses ''TypeCheckerState

emptyTypeCheckerState :: TypeCheckerState
emptyTypeCheckerState = TypeCheckerState 0 [] [] [] []

type TypeChecker = StateT TypeCheckerState (Either T.Text)

withCache :: Eq a => Lens' TypeCheckerState [(a, b)] -> a -> TypeChecker b
          -> TypeChecker b
withCache lens k f = do
  cache <- use lens
  case lookup k cache of
    Nothing -> do
      v <- f
      lens #%= ((k, v) :)
      return v
    Just v -> return v

getVariable :: MonadError T.Text m => [a] -> Int -> m a
getVariable ctx i = case ctx ^? element i of
  Nothing -> throwError "variable index out of bound"
  Just ty -> return ty

newMetaVar :: TypeChecker (Int, Fix (MetaF f))
newMetaVar = state $ \st ->
  let i = st ^. nextMetaIndex
  in ((i, MetaIndex i), st & nextMetaIndex +~ 1)

addMetaType :: Int -> MetaType -> TypeChecker ()
addMetaType = addMeta checkOccurence typeConstraints
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

addMetaKind :: Int -> MetaKind -> TypeChecker ()
addMetaKind = addMeta checkOccurence kindConstraints
  where
    checkOccurence :: Int -> MetaKind -> Bool
    checkOccurence i = \case
      MetaIndex i' -> i == i'
      StarM -> False
      ArrowM k k' -> checkOccurence i k || checkOccurence i k'

addMeta :: (Functor f, Pretty (Fix (MetaF f))) => (Int -> Fix (MetaF f) -> Bool)
        -> Lens' TypeCheckerState [(Int, Fix (MetaF f))]
        -> Int -> Fix (MetaF f) -> TypeChecker ()
addMeta checkOccurence lens i m = do
  ms <- use lens
  -- We want to ensure an invariant: references between elements of the list
  -- only go from right to left.
  let m' = substMetas ms m
  case m' of
    MetaIndex i' | i == i' ->
      return () -- Could happen after substitutions, we just discard those.
    _ -> do
      when (checkOccurence i m') $ throwError $
        "self-referencing ?" <> T.pack (show i) <> " = " <> prettyPrint m'
      lens #%= ((i, m') :)

findMeta :: Lens' TypeCheckerState [(Int, meta)] -> Int
         -> TypeChecker (Maybe meta)
findMeta lens i = do
  ms <- use lens
  return $ lookup i ms

findMetaType :: Int -> TypeChecker (Maybe MetaType)
findMetaType = findMeta typeConstraints

findMetaKind :: Int -> TypeChecker (Maybe MetaKind)
findMetaKind = findMeta kindConstraints

getMeta :: Lens' TypeCheckerState [(Int, Fix (MetaF f))] -> Int
        -> TypeChecker (Fix (MetaF f))
getMeta lens i = do
  mm <- findMeta lens i
  case mm of
    Nothing -> return $ MetaIndex i
    Just m  -> return m

getMetaType :: Int -> TypeChecker MetaType
getMetaType = getMeta typeConstraints

getMetaKind :: Int -> TypeChecker MetaKind
getMetaKind = getMeta kindConstraints

withCleanedConstraints :: TypeChecker a -> TypeChecker a
withCleanedConstraints action = do
  st <- get
  nextMetaIndex   #= 0
  typeConstraints #= []
  kindConstraints #= []
  res <- action
  nextMetaIndex   #= st ^. nextMetaIndex
  typeConstraints #= st ^. typeConstraints
  kindConstraints #= st ^. kindConstraints
  return res

-- FIXME: Check that the type has kind *
typeOfDefinition :: FQDefinition -> TypeChecker Definition
typeOfDefinition fqdef = traceShow (unFQDefinition fqdef ^. defName) $ case fqdef of
    FQDefinition (TermDefinition name fqterm mSig) -> do
      (term, inferred) <- typeOf fqterm
      finalType <- case mSig of
        Nothing -> return inferred
        Just sig -> do
          (sig', _) <- kindOf sig
          checkMoreSpecific sig' inferred
          return sig'
      return $ Definition $ TermDefinition name term finalType

    FQDefinition (ForeignDefinition name sig) -> do
      (typ, k) <- kindOf sig
      return $ Definition $ ForeignDefinition name typ

  where
    checkMoreSpecific :: Type -> Type -> TypeChecker ()
    checkMoreSpecific (Type s) (Type g) = withCleanedConstraints $
      unify (toMeta s) (toMeta g)

typeOf :: FQTerm -> TypeChecker (Term, Type)
typeOf fqterm = withCleanedConstraints $ do
    (term, mty) <- typeCheck [] fqterm
    constraints <- use typeConstraints
    typ <- lift $ fromMetaType constraints mty
    return (term, typ)

typeCheck :: [MetaType] -> FQTerm -> TypeChecker (Term, MetaType)
typeCheck ctx fqterm = traceShow fqterm $ case fqterm of
  Variable i -> (Variable i,) <$> getVariable ctx i

  Reference (Ref mname name fqdef) -> do
    def@(Definition d) <- typeOfDefinition fqdef
    return (Reference (Ref mname name def), toMeta (unType (d ^. defType)))

  ConstructorReference (Ref mname name fqdef) cname -> do
    def <- kindCheckTypeDefinition mname fqdef
    ty  <- constructorType mname def cname
    return ( ConstructorReference (Ref mname name def) cname
           , toMeta (unType ty) )

  Abstraction (Just tyIn) te -> do
    tyIn' <- toMeta . unType . fst <$> kindOf tyIn
    (te', tyOut) <- typeCheck (tyIn' : ctx) te
    return (Abstraction Nothing te', FunctionTypeM tyIn' tyOut)

  Abstraction Nothing te -> do
    (_, tyIn) <- newMetaVar
    let ctx' = tyIn : map (bumpTypeIndexFrom 0) ctx
    (te', tyOut) <- typeCheck ctx' te
    return (Abstraction Nothing te', FunctionTypeM tyIn tyOut)

  Application te1 te2 -> do
    (te1', ty1) <- typeCheck ctx te1
    (te2', ty2) <- typeCheck ctx te2
    (i, tyU) <- newMetaVar
    unify (FunctionTypeM ty2 tyU) ty1
    mt <- getMetaType i
    return (Application te1' te2', mt)

  PatternMatch te pairs -> typeCheckPatternMatch ctx te pairs

  IfThenElse tec te1 te2 -> do
    (tec', tyc) <- typeCheck ctx tec
    (te1', ty1) <- typeCheck ctx te1
    (te2', ty2) <- typeCheck ctx te2
    unify tyc PrimTypeBoolM
    (i, tyU) <- newMetaVar
    unify tyU ty1
    unify tyU ty2
    mt <- getMetaType i
    return (IfThenElse tec' te1' te2', mt)

  PrimBool b -> return (PrimBool b, PrimTypeBoolM)
  PrimInt i -> return (PrimInt i, PrimTypeIntM)

  FixTerm -> do
    -- fix : forall a. (a -> a) -> a
    let mt = UniversalTypeM (FunctionTypeM
                            (FunctionTypeM (TypeVariableM 0)
                                           (TypeVariableM 0))
                            (TypeVariableM 0))
    return (FixTerm, mt)

  Constructor _ _ _ -> error "typeCheck: the impossible happened"

typeCheckPatternMatch :: [MetaType] -> FQTerm -> [(FQPattern, FQTerm)]
                      -> TypeChecker (Term, MetaType)
typeCheckPatternMatch ctx te pairs = do
    (te', ty) <- typeCheck ctx te
    (i, tyU)  <- newMetaVar

    (missing, mkPairs) <- foldM (go ty tyU) (MissingAll, id) pairs
    case missing of
      MissingNone -> return ()
      _ -> throwError "non-exhaustive pattern matching"

    mt <- getMetaType i
    return (PatternMatch te' (mkPairs []), mt)

  where
    go :: MetaType -> MetaType
       -> (MissingPatterns, [(Pattern, Term)] -> [(Pattern, Term)])
       -> (FQPattern, FQTerm)
       -> TypeChecker (MissingPatterns, [(Pattern, Term)] -> [(Pattern, Term)])
    go headmty branchmty (missing, f) (pat, te) = do
      (pat', patmty, extraCtx) <- typeCheckPattern pat
      unify headmty patmty

      (te', temty) <- typeCheck (extraCtx ++ ctx) te
      unify temty branchmty

      missing' <- case removeMissingPattern missing pat' of
        Nothing -> throwError "redundant pattern"
        Just m  -> return m

      return (missing', f . ((pat', te') :))

typeCheckPattern :: FQPattern -> TypeChecker (Pattern, MetaType, [MetaType])
typeCheckPattern = \case
  WildcardPattern ->  (\(_,mt) -> (WildcardPattern, mt, [])) <$> newMetaVar
  VariablePattern name ->
    (\(_,mt) -> (VariablePattern name, mt, [mt])) <$> newMetaVar
  ConstructorPattern (Ref mname name def) cname patterns -> do
    def' <- kindCheckTypeDefinition mname def

    argTypes <- case lookup cname (def' ^. constructors) of
      Nothing -> throwError $ "unknown constructor " <> cname <> " for type "
                              <> prettyPrint mname <> "." <> name
      Just ts -> return ts
    unless (length patterns == length argTypes) $
      throwError "invalid number of arguments to constructor in pattern"

    varMetaTypes <- map snd <$> mapM (\_ -> newMetaVar) (def' ^. variables)

    subs <- forM (zip argTypes patterns) $ \(argType, pat) -> do
      let argMetaType = substDeBruijnIndex 0 (zip [1..] varMetaTypes)
                                           (toMeta (unType argType))
      sub@(_, patternMetaType, _) <- typeCheckPattern pat
      unify argMetaType patternMetaType
      return sub

    let patterns' = map (\(p,_,_) -> p) subs
        extraCtx  = concatMap (\(_,_,c) -> c) subs
        ref'       = Ref mname name def'
        mt = foldr FunctionTypeM (TypeConstructorM ref') varMetaTypes

    return (ConstructorPattern ref' cname patterns', mt, extraCtx)

-- | Unify the two given types and adapt the constraints in the state.
-- The first given type is a subtype of the second.
unify :: MetaType -> MetaType -> TypeChecker ()
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
    universalHelper :: Int -> MetaType -> MetaType -> MetaType -> TypeChecker ()
    universalHelper count tyIn tyOut = \case
      UniversalTypeM ty -> universalHelper (count+1) tyIn tyOut ty
      FunctionTypeM tyIn' tyOut' -> traceShow ("---", tyIn', tyOut', tyIn, tyOut) $ do
        (tyInMono, repl) <- monomorphiseType 0 [] tyIn'
        let tyOut'' = addUniversals (count - length repl) $ traceShow ("substDeBruijnIndex", 0, repl, tyOut') $
                        substDeBruijnIndex 0 repl tyOut'
        unify (FunctionTypeM tyIn tyOut) (FunctionTypeM tyInMono tyOut'')
      mt -> do
        throwError $ "can't unify universal type with " <> prettyPrint mt

    -- It takes a type without the leading UniversalType constructors,
    -- replaces the type variables by meta variables and returns the newly
    -- constructed type together with the list of replacements.
    -- It does not replace type variables of other UniversalType it encounters.
    monomorphiseType :: Int -> [(Int, MetaType)] -> MetaType
                     -> TypeChecker (MetaType, [(Int, MetaType)])
    monomorphiseType threshold acc = \case
      t@(MetaIndex _) -> return (t, acc)
      t@(TypeVariableM i)
        | i >= threshold ->
            case lookup i acc of
              Nothing -> do
                (_, mvar) <- newMetaVar
                return (mvar, (i, mvar) : acc)
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

    unifyMetaIndex :: Int -> MetaType -> TypeChecker ()
    unifyMetaIndex i t = do
      mMT <- findMetaType i
      case mMT of
        Nothing -> addMetaType i t
        Just mt -> unify mt t

-- FIXME: what about nested forall's?
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

{-
monomorphiseType :: MetaType -> TypeChecker (MetaType, [(Int, MetaType)])
monomorphiseType threshold acc = \case
  t@(MetaIndex _) -> return (t, acc)
  t@(TypeVariableM i)
    | i >= threshold ->
        case lookup i acc of
          Nothing -> do
            (_, mvar) <- newMetaVar
            return (mvar, (i, mvar) : acc)
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
-}

data MissingPatterns
  = MissingAll
  | MissingConstructors [(T.Text, [MissingPatterns])]
  | MissingNone
  deriving (Eq, Show)

removeMissingPattern :: MissingPatterns -> Pattern -> Maybe MissingPatterns
removeMissingPattern missing pat = case (missing, pat) of
  (MissingNone, _) -> Nothing
  (_, WildcardPattern) -> Just MissingNone
  (_, VariablePattern _) -> Just MissingNone

  (MissingAll, ConstructorPattern (Ref _ _ def) _ _) ->
    let missing' = MissingConstructors $
          map (fmap (map (const MissingAll))) (def ^. constructors)
    in removeMissingPattern missing' pat

  (MissingConstructors constrs, ConstructorPattern _ cname patterns) -> do
    let (res, others) = partition ((==cname) . fst) constrs
    missings <- snd <$> listToMaybe res
    guard $ length missings == length patterns

    let step (missing', pat') (acc, changed) =
          case removeMissingPattern missing' pat' of
            Nothing -> (missing' : acc, changed)
            Just missing'' -> (missing'' : acc, True)
        (missings', changed) = foldr step ([], False) (zip missings patterns)

    if changed
      then Just $ MissingConstructors $ (cname, missings') : others
      else Nothing

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

constructorType :: ModuleName -> TypeDefinition Type Kind -> T.Text
                -> TypeChecker Type
constructorType mname def cname = case lookup cname (def ^. constructors) of
    Nothing -> throwError $ "can't find constructor " <> cname
    Just types -> do
      let tyConstr  = TypeConstructor (Ref mname (def ^. typeName) def)
      let finalType = mkFinalType tyConstr (def ^. variables) 0
      return $ Type $ addUniversals (def ^. variables) $
        foldr (FunctionType . unType) finalType types

  where
    mkFinalType :: TypeP (Ref (TypeDefinition Type Kind)) -> [a] -> Int
                -> TypeP (Ref (TypeDefinition Type Kind))
    mkFinalType acc [] _ = acc
    mkFinalType acc (_:vars) n =
      mkFinalType (TypeApplication acc (TypeVariable n)) vars (n+1)

    addUniversals :: [a] -> TypeP (Ref (TypeDefinition Type Kind))
                  -> TypeP (Ref (TypeDefinition Type Kind))
    addUniversals [] t = t
    addUniversals (_ : xs) t = addUniversals xs (UniversalType t)

kindCheckTypeDefinition :: ModuleName -> TypeDefinition FQType ()
                        -> TypeChecker (TypeDefinition Type Kind)
kindCheckTypeDefinition mname def =
    withCache kindedDefinitions (mname, def ^. typeName) go

  where
    go :: TypeChecker (TypeDefinition Type Kind)
    go = withCleanedConstraints $ do
      vars <- foldM (\acc _ -> (: acc) <$> newMetaVar) [] (def ^. variables)
      mfix $ \final -> do
        let tuple = (mname, def ^. typeName, final, ownKind vars)
        flip runReaderT (Just tuple) $ do
          let ctx = map snd vars
          def' <- forOf (constructors . each . _2 . each) def
                        (fmap (Type . fst) . kindCheck ctx . unFQType)
          mks <- lift $ foldM (\acc -> fmap (: acc) . getMetaKind . fst) [] vars
          constraints <- use kindConstraints
          return $ def' & variables .~ map (fromMetaKind constraints) mks

    ownKind :: [(Int, MetaKind)] -> MetaKind
    ownKind = foldr ArrowM StarM . map snd

kindOf :: FQType -> TypeChecker (Type, Kind)
kindOf fqtype = withCleanedConstraints $ do
  (typ, mkind) <- runReaderT (kindCheck [] (unFQType fqtype)) Nothing
  constraints  <- use kindConstraints
  let kind = fromMetaKind constraints mkind
  return (Type typ, kind)

-- | Kind check a type in a context. The first argument is used when kind
-- checking a type definition and contains the name of the type being kind
-- checked and its own meta kind.
kindCheck :: [MetaKind] -> TypeP (Ref (TypeDefinition FQType ()))
          -> ReaderT (Maybe ( ModuleName, T.Text
                            , TypeDefinition Type Kind, MetaKind ))
                     TypeChecker
                     (TypeP (Ref (TypeDefinition Type Kind)), MetaKind)
kindCheck vars = \case
  TypeVariable i -> (TypeVariable i,) <$> getVariable vars i

  FunctionType fqt fqt' -> do
    (t,  k)  <- kindCheck vars fqt
    (t', k') <- kindCheck vars fqt'
    lift $ unifyKind k  StarM
    lift $ unifyKind k' StarM
    return (FunctionType t t', StarM)

  UniversalType fqt -> do
    (_, ku) <- lift newMetaVar
    (t, k)  <- kindCheck (ku : vars) fqt
    lift $ unifyKind k StarM
    return (UniversalType t, StarM)

  PrimTypeBool -> return (PrimTypeBool, StarM)
  PrimTypeInt -> return (PrimTypeInt, StarM)

  TypeConstructor (Ref mname name fqdef) -> do
    mOwn <- ask
    case mOwn of
      Just (ownModuleName, ownName, ownDef, ownKind)
        | ownModuleName == mname && ownName == name ->
            return (TypeConstructor (Ref mname name ownDef), ownKind)
      _ -> do
        def <- lift $ kindCheckTypeDefinition mname fqdef
        let k = toMeta $ foldr Arrow Star $ def ^. variables
        return (TypeConstructor (Ref mname name def), k)

  TypeApplication fqt fqt' -> do
    (t,  k)  <- kindCheck vars fqt
    (t', k') <- kindCheck vars fqt'
    (i, ku)  <- lift newMetaVar
    lift $ unifyKind k (ArrowM k' ku)
    (TypeApplication t t',) <$> lift (getMetaKind i)

unifyKind :: MetaKind -> MetaKind -> TypeChecker ()
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
    unifyMetaIndex :: Int -> MetaKind -> TypeChecker ()
    unifyMetaIndex i k = do
      mMK <- findMetaKind i
      case mMK of
        Nothing -> addMetaKind i k
        Just mk -> unifyKind mk k

typeOfModule :: Maybe [T.Text] -> FQModule -> TypeChecker TypedModule
typeOfModule mForeignNames _mod = do
    _mod' <- forOf (types . each) _mod
                   (kindCheckTypeDefinition (_mod ^. moduleName))
    forOf (definitions . each) _mod' typeOfDefinition

typeCheckModules :: MonadError T.Text m => Maybe [T.Text] -> [FQModule]
                 -> m [TypedModule]
typeCheckModules mForeignNames mods = either throwError return $
  evalStateT (reorderModules mods >>= mapM (typeOfModule mForeignNames))
             emptyTypeCheckerState
