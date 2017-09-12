module Epic.PrettyPrinter where
--  ( ppType
--  , ppReference
--  ) where

import Debug.Trace

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

ppType :: Type -> T.Text
ppType = go False (nameStream ()) [] . traceShowId
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
        let txt = go True ns names t <> " " <> go False ns names t'
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

ppReference :: Reference -> T.Text
ppReference = \case
  NameReference name -> name
