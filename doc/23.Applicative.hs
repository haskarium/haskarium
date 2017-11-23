{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}

module Applicative where

import           Prelude hiding (Applicative)

import           Data.Functor.Identity
import           Data.Kind (Type)
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

instance Applicative [] where
    pure :: a -> [a]
    pure a = [a]

    (<*>) :: [a -> b] -> [a] -> [b]
    fs <*> xs = [f x | f <- fs, x <- xs]

-- pure f <*> a === fmap f a

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap
