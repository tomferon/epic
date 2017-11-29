{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}

module Epic.Conversion
  ( FromEpic(..)
  , ToEpic(..)
  , GToEpic(..)
  , GFromEpic(..)
  ) where

import           GHC.Generics

import           Control.Monad
import           Control.Monad.ST

import qualified Data.Text as T

import           Epic.Evaluation.Internal
import           Epic.Language

data Proxy s (f :: * -> *) = Proxy

class GToEpic s f where
  -- | FIXME: doc: eval terms in reversed order
  gtoEpic :: Int -> f a -> ST s (Int, [EvalTerm s])
  constructorCount :: Proxy s f -> Int

instance GToEpic s U1 where
  gtoEpic i U1 = return (i, [])
  constructorCount _ = 1

instance GToEpic s f => GToEpic s (M1 i c f) where
  gtoEpic i = gtoEpic i . unM1

  constructorCount = constructorCount . mkProxy
    where
      mkProxy :: Proxy s (M1 i c f) -> Proxy s f
      mkProxy _ = Proxy

instance ToEpic s a => GToEpic s (K1 i a) where
  gtoEpic i = fmap (\x -> (i, [x])) . toEpic . unK1
  constructorCount _ = 1

instance (GToEpic s a, GToEpic s b) => GToEpic s (a :*: b) where
  gtoEpic i (x :*: y) = do
    (_, xs) <- gtoEpic 0 x
    (_, ys) <- gtoEpic 0 y
    return (i, ys ++ xs)
  constructorCount _ = 1

instance (GToEpic s a, GToEpic s b) => GToEpic s (a :+: b) where
  gtoEpic i x = case x of
      L1 y -> gtoEpic i y
      R1 y -> do
        proxy <- mkProxy x
        let j = constructorCount proxy
        gtoEpic (i+j) y
    where
      mkProxy :: (a :+: b) c -> ST s (Proxy s a)
      mkProxy _ = return Proxy

  constructorCount proxy =
      constructorCount (mkProxyL proxy) + constructorCount (mkProxyR proxy)
    where
      mkProxyL :: Proxy s (a :+: b) -> Proxy s a
      mkProxyL _ = Proxy

      mkProxyR :: Proxy s (a :+: b) -> Proxy s b
      mkProxyR _ = Proxy

class ToEpic s a where
  toEpic :: a -> ST s (EvalTerm s)

  default toEpic :: (Generic a, GToEpic s (Rep a)) => a -> ST s (EvalTerm s)
  toEpic x = do
    (i, ets) <- gtoEpic 0 $ from x
    ts <- mapM (makeThunk . return) ets
    return $ Constructor i ts

instance ToEpic s Bool where
  toEpic = return . BaseTerm . PrimBool

instance ToEpic s Int where
  toEpic = return . BaseTerm . PrimInt

instance ToEpic s Char where
  toEpic = return . BaseTerm . PrimChar

instance ToEpic s T.Text where
  toEpic = return . BaseTerm . PrimString

instance (FromEpic s a, ToEpic s b) => ToEpic s (a -> b) where
  toEpic f = return $ HaskellFunction $ toEpic . f <=< fromEpic <=< evalThunk

instance ToEpic s ()

instance (ToEpic s a, ToEpic s b) => ToEpic s (a, b)
instance (ToEpic s a, ToEpic s b, ToEpic s c) => ToEpic s (a, b, c)
instance (ToEpic s a, ToEpic s b, ToEpic s c, ToEpic s d)
  => ToEpic s (a, b, c, d)
instance (ToEpic s a, ToEpic s b, ToEpic s c, ToEpic s d, ToEpic s e)
  => ToEpic s (a, b, c, d, e)
instance ( ToEpic s a, ToEpic s b, ToEpic s c, ToEpic s d, ToEpic s e
         , ToEpic s f ) => ToEpic s (a, b, c, d, e, f)
instance ( ToEpic s a, ToEpic s b, ToEpic s c, ToEpic s d, ToEpic s e
         , ToEpic s f, ToEpic s g ) => ToEpic s (a, b, c, d, e, f, g)

instance (ToEpic s a, ToEpic s b) => ToEpic s (Either a b)
instance ToEpic s a => ToEpic s [a]
instance ToEpic s a => ToEpic s (Maybe a)

class GFromEpic s f where
  -- | FIXME: doc: eval term in "correct" order
  gfromEpic :: (Int, [EvalTerm s]) -> ST s (Either Int ([EvalTerm s], f a))

instance GFromEpic s U1 where
  gfromEpic (0, ets) = return $ Right (ets, U1)
  gfromEpic (i, _) = return $ Left $ i - 1
  gfromEpic _ = error "can't convert U1"

instance GFromEpic s f => GFromEpic s (M1 i c f) where
  gfromEpic = fmap (fmap (fmap M1)) . gfromEpic

instance FromEpic s a => GFromEpic s (K1 i a) where
  gfromEpic (0, et : ets) = (\x -> Right (ets, K1 x)) <$> fromEpic et
  gfromEpic (0, []) = error "can't convert K1"
  gfromEpic (i, _) = return $ Left $ i - 1

instance (GFromEpic s a, GFromEpic s b) => GFromEpic s (a :*: b) where
  gfromEpic (0, ets) = do
    res1 <- gfromEpic (0, ets)
    case res1 of
      Left i -> error "can't convert (:*:): the impossible happened"
      Right (ets', x) -> do
        res2 <- gfromEpic (0, ets')
        case res2 of
          Left i -> error "can't convert (:*:): the impossible happened"
          Right (ets'', y) -> return $ Right (ets'', x :*: y)
  gfromEpic (i, _) = return $ Left $ i - 1
  gfromEpic _ = error "can't convert (:*:)"

instance (GFromEpic s a, GFromEpic s b) => GFromEpic s (a :+: b) where
  gfromEpic (i, ets) = do
    res <- gfromEpic (i, ets)
    case res of
      Left i' -> fmap (fmap (fmap R1)) (gfromEpic (i', ets))
      Right (ets', x) -> return $ Right (ets', L1 x)

class FromEpic s a  where
  fromEpic :: EvalTerm s -> ST s a

  default fromEpic :: (Generic a, GFromEpic s (Rep a)) => EvalTerm s -> ST s a
  fromEpic (Constructor i ts) = do
    ets <- foldM (\acc t -> (: acc) <$> evalThunk t) [] ts
    res <- gfromEpic (i, ets)
    case res of
      Left i -> error $ "wrong number of constructor (" ++ show i ++ ")"
      Right (_, x) -> return $ to x
  fromEpic et = do
    str <- debugShowEvalTerm 2 et
    error $ "fromEpic (" ++ str ++ ")"
  fromEpic _ = error "can't convert non-constructor value to generics"

instance FromEpic s Bool where
  fromEpic (BaseTerm (PrimBool x)) = return x
  fromEpic _ = error "can't convert Bool"

instance FromEpic s Int where
  fromEpic (BaseTerm (PrimInt x)) = return x
  fromEpic _ = error "can't convert Int"

instance FromEpic s Char where
  fromEpic (BaseTerm (PrimChar x)) = return x
  fromEpic _ = error "can't convert Char"

instance FromEpic s T.Text where
  fromEpic (BaseTerm (PrimString x)) = return x
  fromEpic _ = error "can't convert T.Text"

instance (ToEpic s a, FromEpic s b) => FromEpic s (a -> ST s b) where
  fromEpic (HaskellFunction f) = return $ \x -> do
    t <- makeThunk $ toEpic x
    et <- f t
    fromEpic et
  fromEpic (TermWithContext (Abstraction _ t) ctx foreigns) = return $ \x -> do
    t' <- makeThunk $ toEpic x
    et <- evalWHNFCtx (t' : ctx) foreigns $ toEvalTerm t
    fromEpic et
  fromEpic _ = error "can't convert function"

instance FromEpic s ()

instance (FromEpic s a, FromEpic s b) => FromEpic s (a, b)
instance (FromEpic s a, FromEpic s b, FromEpic s c) => FromEpic s (a, b, c)
instance (FromEpic s a, FromEpic s b, FromEpic s c, FromEpic s d)
  => FromEpic s (a, b, c, d)
instance (FromEpic s a, FromEpic s b, FromEpic s c, FromEpic s d, FromEpic s e)
  => FromEpic s (a, b, c, d, e)
instance ( FromEpic s a, FromEpic s b, FromEpic s c, FromEpic s d, FromEpic s e
         , FromEpic s f ) => FromEpic s (a, b, c, d, e, f)
instance ( FromEpic s a, FromEpic s b, FromEpic s c, FromEpic s d, FromEpic s e
         , FromEpic s f, FromEpic s g ) => FromEpic s (a, b, c, d, e, f, g)

instance (FromEpic s a, FromEpic s b) => FromEpic s (Either a b)
instance FromEpic s a => FromEpic s [a]
instance FromEpic s a => FromEpic s (Maybe a)
