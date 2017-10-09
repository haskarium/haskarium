module Haskarium.Util
    ( distance
    ) where

import           Graphics.Gloss (Point)

distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) =
    sqrt $ (x1 - x2) ^ (2 :: Int) + (y1 - y2) ^ (2 :: Int)
