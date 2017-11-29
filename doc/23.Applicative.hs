{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}

module Applicative where

import           Prelude hiding (Applicative, pure, (<$>), (<*>))

import           Data.Functor.Identity
import           Data.Kind (Type)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Proxy

-- fmap  :: (a -> b) -> f a -> f b
-- fmap2 :: (a -> b -> c) -> f a -> f b -> f c

class Functor2 (f :: Type -> Type) where
    fmap2 :: (a -> b -> c) -> f a -> f b -> f c

instance Functor2 Maybe where
    fmap2 f (Just a) (Just b) = Just (f a b)
    fmap2 _ _ _ = Nothing

instance Functor2 (Either a) where
    fmap2 f (Right a) (Right b) = Right (f a b)
    fmap2 _ (Left e)  _         = Left e
    fmap2 _ _         (Left e)  = Left e

class Functor f => Applicative (f :: Type -> Type) where
    -- | ap
    (<*>) :: f (a -> b) -> f a -> f b

    pure :: a -> f a -- fmap0, liftA0

instance Applicative Maybe where
    Just f <*> Just a = Just (f a)
    _      <*> _      = Nothing

    pure = Just

instance Applicative Proxy where
    pure _ = Proxy
    Proxy <*> Proxy = Proxy

instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity a = Identity (f a)

-- instance Applicative IO where
--     pure :: a -> IO a
--     pure = ...
--     (<*>) :: IO (a -> b) -> IO a -> IO b
--     (<*>) = ...

instance Applicative [] where
    pure :: a -> [a]
    pure a = [a]

    (<*>) :: [a -> b] -> [a] -> [b]
    fs <*> xs = [f x | f <- fs, x <- xs]

-- pure f <*> a === fmap f a

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

-- NOT instance Applicative (Map k) where
--     pure :: a -> Map k a

-- (*>) :: Applicative f => f a -> f b -> f b
-- actionA *> actionB = pure (\_ b -> b) <*> actionA <*> actionB
--
-- (<*) :: Applicative f => f a -> f b -> f a
-- actionA <* actionB = pure const <*> actionA <*> actionB

-- const a = \_ -> a
-- const a _ = a

liftA0 :: Applicative f => a -> f a
liftA0 = pure

liftA1 :: Functor f => (a -> b) -> f a -> f b
liftA1 = fmap

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 g fa fb = g <$> fa <*> fb
         -- pure g <*> fa <*> fb

(*>) :: Applicative f => f a -> f b -> f b
(*>) = liftA2 (\_ b -> b)

(<*) :: Applicative f => f a -> f b -> f a
(<*) = liftA2 const
