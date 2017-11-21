{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Epic.Conversion
  ( FromEpic(..)
  , ToEpic(..)
  , GToEpic(..)
  , GFromEpic(..)
  ) where

import GHC.Generics

import Control.Monad
import Control.Monad.ST

import Epic.Evaluation.Internal
import Epic.Language

class GToEpic f where
  -- | FIXME: eval terms in reversed order
  gtoEpic :: f a -> ST s (Int, [EvalTerm s])

instance GToEpic U1 where
  gtoEpic U1 = return (0, [])

instance GToEpic f => GToEpic (M1 i c f) where
  gtoEpic = gtoEpic . unM1

instance ToEpic a => GToEpic (K1 i a) where
  gtoEpic = fmap (\x -> (0, [x])) . toEpic . unK1

instance (GToEpic a, GToEpic b) => GToEpic (a :*: b) where
  gtoEpic (x :*: y) = do
    (_, xs) <- gtoEpic x
    (_, ys) <- gtoEpic y
    return (0, ys ++ xs)

instance (GToEpic a, GToEpic b) => GToEpic (a :+: b) where
  gtoEpic = \case
    L1 x -> gtoEpic x
    R1 x -> do
      (i, ts) <- gtoEpic x
      return (i + 1, ts)

class ToEpic a where
  toEpic :: a -> ST s (EvalTerm s)

  default toEpic :: (Generic a, GToEpic (Rep a)) => a -> ST s (EvalTerm s)
  toEpic x = do
    (i, ets) <- gtoEpic $ from x
    ts <- mapM (makeThunk . return) ets
    return $ Constructor i ts

instance ToEpic Bool where
  toEpic = return . BaseTerm . PrimBool

instance ToEpic Int where
  toEpic = return . BaseTerm . PrimInt

instance (FromEpic a, ToEpic b) => ToEpic (a -> b) where
  toEpic f = return $ HaskellFunction $ toEpic . f <=< fromEpic

instance ToEpic ()

instance (ToEpic a, ToEpic b) => ToEpic (a, b)
instance (ToEpic a, ToEpic b, ToEpic c) => ToEpic (a, b, c)
instance (ToEpic a, ToEpic b, ToEpic c, ToEpic d) => ToEpic (a, b, c, d)
instance (ToEpic a, ToEpic b, ToEpic c, ToEpic d, ToEpic e)
  => ToEpic (a, b, c, d, e)
instance (ToEpic a, ToEpic b, ToEpic c, ToEpic d, ToEpic e, ToEpic f)
  => ToEpic (a, b, c, d, e, f)
instance (ToEpic a, ToEpic b, ToEpic c, ToEpic d, ToEpic e, ToEpic f, ToEpic g)
  => ToEpic (a, b, c, d, e, f, g)

instance (ToEpic a, ToEpic b) => ToEpic (Either a b)
instance ToEpic a => ToEpic [a]
instance ToEpic a => ToEpic (Maybe a)

class GFromEpic f where
  -- | FIXME: eval term in "correct" order
  gfromEpic :: (Int, [EvalTerm s]) -> ST s (f a)

instance GFromEpic U1 where
  gfromEpic (0, []) = return U1
  gfromEpic _ = error "can't convert U1"

instance GFromEpic f => GFromEpic (M1 i c f) where
  gfromEpic = fmap M1 . gfromEpic

instance FromEpic a => GFromEpic (K1 i a) where
  gfromEpic (_, [et]) = K1 <$> fromEpic et
  gfromEpic _ = error "can't convert K1"

instance (GFromEpic a, GFromEpic b) => GFromEpic (a :*: b) where
  gfromEpic (i, t : ts) = (:*:) <$> gfromEpic (i, [t]) <*> gfromEpic (i, ts)
  gfromEpic _ = error "can't convert :*:"

instance (GFromEpic a, GFromEpic b) => GFromEpic (a :+: b) where
  gfromEpic (0, ts) = L1 <$> gfromEpic (0,   ts)
  gfromEpic (i, ts) = R1 <$> gfromEpic (i-1, ts)

class FromEpic a  where
  fromEpic :: EvalTerm s -> ST s a

  default fromEpic :: (Generic a, GFromEpic (Rep a)) => EvalTerm s -> ST s a
  fromEpic (Constructor i ts) = do
    ets <- foldM (\acc t -> (: acc) <$> evalThunk t) [] ts
    fmap to $ gfromEpic (i, ets)
  fromEpic _ = error "can't convert non-constructor value to generics"

instance FromEpic Bool where
  fromEpic (BaseTerm (PrimBool x)) = return x
  fromEpic _ = error "can't convert Bool"

instance FromEpic Int where
  fromEpic (BaseTerm (PrimInt x)) = return x
  fromEpic _ = error "can't convert Int"

instance FromEpic ()

instance (FromEpic a, FromEpic b) => FromEpic (a, b)
instance (FromEpic a, FromEpic b, FromEpic c) => FromEpic (a, b, c)
instance (FromEpic a, FromEpic b, FromEpic c, FromEpic d)
  => FromEpic (a, b, c, d)
instance (FromEpic a, FromEpic b, FromEpic c, FromEpic d, FromEpic e)
  => FromEpic (a, b, c, d, e)
instance ( FromEpic a, FromEpic b, FromEpic c, FromEpic d, FromEpic e
         , FromEpic f ) => FromEpic (a, b, c, d, e, f)
instance ( FromEpic a, FromEpic b, FromEpic c, FromEpic d, FromEpic e
         , FromEpic f, FromEpic g ) => FromEpic (a, b, c, d, e, f, g)

instance (FromEpic a, FromEpic b) => FromEpic (Either a b)
instance FromEpic a => FromEpic [a]
instance FromEpic a => FromEpic (Maybe a)
