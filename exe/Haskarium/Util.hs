{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Haskarium.Util
    ( distance
    , randomRS
    ) where

import           Control.Monad.State.Strict (MonadState, state)
import           Graphics.Gloss (Point)
import           System.Random (Random, StdGen, randomR)

import           Haskarium.Types (Distance)

distance :: Point -> Point -> Distance
distance (x1, y1) (x2, y2) =
    sqrt $ (x1 - x2) ^ (2 :: Int) + (y1 - y2) ^ (2 :: Int)

randomRS
    :: (Random a, MonadState StdGen m)
    => (a, a) -> m a
randomRS range = state $ \s ->
    let (a, s1) = randomR range s
    in (a, s1)
