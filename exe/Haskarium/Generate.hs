module Haskarium.Generate
    ( makeCreatures
    ) where

import           Graphics.Gloss (Point)
import           System.Random (StdGen,  randomR)

import           Haskarium.Types (Creature (..), Species (..))

makeCreatures :: (Point, Point) -> StdGen -> [Species] -> (StdGen, [Creature])
makeCreatures window g = foldr makeCreatures' (g, [])
  where
    ((minX, minY), (maxX, maxY)) = window
    makeCreatures' spec (g0, creatures) = (g5, c : creatures)
      where
        fakeSize = 10  -- TODO: add real creature sizes
        c = Creature{ position = (x, y)
                    , direction = dir
                    , turnRate = tr
                    , species = s'
                    , size = fakeSize}
        (x, g1) = randomR (minX + fakeSize / 2, maxX - fakeSize / 2) g0
        (y, g2) = randomR (minY + fakeSize / 2, maxY - fakeSize / 2) g1
        (dir, g3) = randomR (0, 2 * pi) g2
        (tr, g4) = case spec of
            Centipede{} ->
                randomR (-pi / 34, -pi / 30) g3
            _ ->
                randomR (pi / 4, pi / 2) g3
        (s', g5) = case spec of
            Centipede{} ->
                let (numSegments, gN) = randomR (5, 15) g4
                in (Centipede{segments = replicate numSegments (x, y)}, gN)
            Flea{} ->
                let (eagerness, gN) = randomR (0.0, 1.0) g4
                in (Flea{idleTime = eagerness}, gN)
            _ ->
                (spec, g4)
