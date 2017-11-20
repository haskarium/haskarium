{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}

module Functor where

import           Prelude hiding (Functor, fmap)

import           Data.Kind (Type)
import           Data.Proxy

-- Maybe :: Type -> Type

-- Either :: Type -> Type -> Type
-- Either a :: Type -> Type

-- data Proxy t = Proxy
-- Proxy :: Type -> Type

-- [] :: Type -> Type

-- Laws:
-- fmap id = id
-- fmap (f . g) = fmap f . fmap g
class Functor (f :: Type -> Type) where
    fmap :: (a -> b) -> (f a -> f b)

instance Functor Maybe where
    fmap f (Just a) = Just (f a)
    fmap _ Nothing = Nothing

divM x y = case y of 0 -> Nothing; _ -> Just (x `div` y)

example1 = fmap (show . succ) $ divM 42 1

instance Functor (Either e) where
    fmap :: (a -> b) -> Either e a -> Either e b
    fmap f (Right a) = Right (f a)
    fmap _ (Left e)  = Left e

divE x y = case y of 0 -> Left "Division by zero"; _ -> Right (x `div` y)

example2 = fmap (show . succ) $ divE 42 1

instance Functor Proxy where
    fmap :: (a -> b) -> Proxy a -> Proxy b
    fmap _ _ = Proxy

instance Functor [] where
    fmap :: (a -> b) -> [a] -> [b]
    fmap = map

divL x y = case y of 0 -> []; _ -> [x `div` y]

example3 = fmap (show . succ) $ divL 42 1
