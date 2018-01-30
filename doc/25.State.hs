{-# LANGUAGE InstanceSigs #-}

module State where

import Control.Applicative (liftA2)

-- newtype State s a = State (s -> (a, s))
-- runState :: State s a -> s -> (a, s)
-- runState (State f) = f

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
    -- fmap :: (a -> b) -> (f a -> f b)
    fmap :: (a -> b) -> (State s a -> State s b)
    fmap f (State sa) = State $ \s0 -> let (a, s1) = sa s0; b = f a in (b, s1)

instance Applicative (State s) where
    pure :: a -> State s a
    pure a = State $ \s -> (a, s)

    liftA2 :: (a -> b -> c) -> State s a -> State s b -> State s c
    liftA2 f (State sa) (State sb) = State $ \s0 ->
        let (a, s1) = sa s0
            (b, s2) = sb s1
            c = f a b
        in (c, s2)

-- newtype IO a = IO (RealWorld -> (RealWorld, a))
