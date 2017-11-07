module Epic.TypeChecker.Internal where

import           Data.List
import           Data.Functor.Foldable
import qualified Data.Text as T

import           Epic.Language

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
