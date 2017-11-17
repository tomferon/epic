module Epic.Conversion
  ( FromEpic(..)
  , ToEpic(..)
  ) where

import Epic.Language

class ToEpic a where
  toEpic :: a -> Term

class FromEpic a  where
  fromEpicE :: Term -> Either String a
  fromEpic :: Term -> a
