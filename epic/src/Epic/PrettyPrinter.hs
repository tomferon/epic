module Epic.PrettyPrinter where
--  ( ppType
--  , ppReference
--  ) where

import Debug.Trace

import           Control.Lens

import           Data.Monoid
import qualified Data.Text as T

import           Epic.Language

data Stream a = Cons a (Stream a)

prependList :: [a] -> Stream a -> Stream a
prependList [] s = s
prependList (x : xs) s = Cons x (prependList xs s)

nameStream :: () -> Stream T.Text
nameStream _ = go (map T.singleton alphabet)
  where
    go :: [T.Text] -> Stream T.Text
    go sub =
      let sub' = foldl (\acc l -> acc ++ map (T.cons l) sub) [] alphabet
      in prependList sub (go sub')

    alphabet :: [Char]
    alphabet = "abcdefghijklmnopqrstuvwxyz"

streamElem :: Int -> Stream a -> a
streamElem 0 (Cons x _) = x
streamElem n (Cons _ xs) = streamElem (n-1) xs

-- FIXME: Remove duplication in ppType and ppMetaType
-- FIXME: Use Data.Text.Lazy.Builder?
-- FIXME: Precedence of type application and arrows for parentheses

ppType :: Type -> T.Text
ppType = go False (nameStream ()) []
  where
    go :: Bool -> Stream T.Text -> [T.Text] -> Type -> T.Text
    go paren ns names = \case
      TypeVariable i -> names !! i
      FunctionType t t' ->
        let txt = go True ns names t <> " -> " <> go False ns names t'
        in if paren then "(" <> txt <> ")" else txt
      t@(UniversalType _) ->
        let txt = universals [] ns names t
        in if paren then "(" <> txt <> ")" else txt
      PrimTypeBool -> "Bool"
      PrimTypeInt -> "Int"
      TypeConstructor ref -> ppReference ref
      TypeApplication t t' ->
        let txt = go paren ns names t <> " " <> go True ns names t'
        in if paren then "(" <> txt <> ")" else txt

    universals :: [T.Text] -> Stream T.Text -> [T.Text] -> Type
               -> T.Text
    universals vars ns names = \case
      UniversalType t ->
        let Cons name ns' = ns
        in universals (name : vars) ns' (name : names) t
      t ->
        let forallNames = foldr (\name txt -> txt <> " " <> name) "" vars
        in "forall" <> forallNames <> ". " <> go False ns names t

ppMetaType :: MetaType -> T.Text
ppMetaType = go False (nameStream ()) []
  where
    go :: Bool -> Stream T.Text -> [T.Text] -> MetaType -> T.Text
    go paren ns names = \case
      MetaIndex i -> "?" <> T.pack (show i)
      TypeVariableM i -> names !! i
      FunctionTypeM t t' ->
        let txt = go True ns names t <> " -> " <> go False ns names t'
        in if paren then "(" <> txt <> ")" else txt
      t@(UniversalTypeM _) ->
        let txt = universals [] ns names t
        in if paren then "(" <> txt <> ")" else txt
      PrimTypeBoolM -> "Bool"
      PrimTypeIntM -> "Int"
      TypeConstructorM ref -> ppReference ref
      TypeApplicationM t t' ->
        let txt = go paren ns names t <> " " <> go True ns names t'
        in if paren then "(" <> txt <> ")" else txt

    universals :: [T.Text] -> Stream T.Text -> [T.Text] -> MetaType
               -> T.Text
    universals vars ns names = \case
      UniversalTypeM t ->
        let Cons name ns' = ns
        in universals (name : vars) ns' (name : names) t
      t ->
        let forallNames = foldr (\name txt -> txt <> " " <> name) "" vars
        in "forall" <> forallNames <> ". " <> go False ns names t

ppReference :: Reference -> T.Text
ppReference = \case
  NameReference name -> name

ppKind :: Kind -> T.Text
ppKind = go False
  where
    go :: Bool -> Kind -> T.Text
    go paren = \case
      Star -> "*"
      Arrow k k' ->
        let txt = go True k <> " -> " <> go False k'
        in if paren then "(" <> txt <> ")" else txt

ppMetaKind :: MetaKind -> T.Text
ppMetaKind = go False
  where
    go :: Bool -> MetaKind -> T.Text
    go paren = \case
      MetaIndex i -> "?" <> T.pack (show i)
      StarM -> "*"
      ArrowM k k' -> go True k <> " -> " <> go False k'

ppModuleName :: ModuleName -> T.Text
ppModuleName = T.intercalate "."

ppTypedModule :: TypedModule -> T.Text
ppTypedModule tmod =
    "module " <> ppModuleName (tmod ^. moduleName)
    <> (if null (tmod ^. types)
          then ""
          else "\n\n# Type definitions\n\n"
               <> T.intercalate "\n\n"
                    (map (ppTypeDef (nameStream ())) (tmod ^. types)))
    <> (if null (tmod ^. definitions)
          then ""
          else "\n\n# Definitions\n\n"
                <> T.unlines (map ppDef (tmod ^. definitions)))

  where
    ppTypeDef :: Stream T.Text -> TypeDefinition Kind -> T.Text
    ppTypeDef names td =
      let kind = foldr Arrow Star (td ^. variables)
      in "type " <> td ^. typeName <> " : " <> ppKind kind
         <> "\ntype " <> td ^. typeName
         <> fst (foldl (\(acc, Cons n ns) _ -> (acc <> " " <> n, ns))
                       ("", names) (td ^. variables))

    ppDef :: Definition Type -> T.Text
    ppDef = \case
      TermDefinition name _ typ -> name <> " : " <> ppType typ
      ForeignDefinition name typ -> "foreign " <> name <> " : " <> ppType typ
