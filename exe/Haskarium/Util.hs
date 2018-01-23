module Haskarium.Util
    ( andThen
    , distance
    ) where

import           Control.Monad.State.Strict (State, runState, state)
import           Graphics.Gloss (Point)

import           Haskarium.Types (Distance)

distance :: Point -> Point -> Distance
distance (x1, y1) (x2, y2) =
    sqrt $ (x1 - x2) ^ (2 :: Int) + (y1 - y2) ^ (2 :: Int)

andThen :: State s a -> (a -> State s b) -> State s b
andThen sa fsb = state $ \s0 ->
    let (a, s1) = runState sa s0
        sb = fsb a
        (b, s2) = runState sb s1
    in  (b, s2)
