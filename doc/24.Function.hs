{-# LANGUAGE NoImplicitPrelude #-}

module Function where

import           Prelude ((.))

class Functor f where
    fmap :: (a -> b) -> f a -> f b

-- a -> b :: Type -- функция -- тип
-- (->) a b :: Type -- префиксная форма оператора
-- (->) a :: Type -> Type -- каррирование
-- (->) :: Type -> Type -> Type -- каррирование

instance Functor ((->) r) where
    -- fmap :: (a -> b) -> f a -> f b
    -- fmap :: (a -> b) -> ((->) r) a -> ((->) r) b
    -- fmap :: (a -> b) -> (->) r a -> (->) r b
    -- fmap :: (a -> b) -> (r -> a) -> (r -> b)

    -- fmap f g = \r -> b
    --   where
    --     b = f a
    --     a = g r
    -- fmap f g = \r -> f (g r)
    -- fmap f g = f . g
    fmap = (.)

    -- Law 1.
    --    fmap id g = id g
    -- Proof:
    --    fmap id g = id . g
    --              = g
    --              = id g
    --    QED

    -- Law 2.
    --    fmap (f . g) = fmap f . fmap g
