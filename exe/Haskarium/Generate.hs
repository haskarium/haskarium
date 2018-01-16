{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Haskarium.Generate
    ( makeGame
    ) where

import           Control.Monad.State.Strict (state)
import           Graphics.Gloss (Point)
import           Numeric.Natural (Natural)
import           System.Random (StdGen, randomR)

import           Haskarium.Types (Angle, Ant (..), Centipede (..),
                                  Creature (..), Flea (..), Fly (..), Rnd,
                                  World (..))

type Window = (Point, Point)

makeGame :: Window -> Rnd World
makeGame window = World
    <$> makeCreaturesRnd window 0 10
    <*> makeCreaturesRnd window 0 10
    <*> makeCreaturesRnd window 0 10
    <*> makeCreaturesRnd window 0 10

makeCreaturesRnd
    :: forall species. Generate species
    => Window -> Natural -> Natural -> Rnd [Creature species]
makeCreaturesRnd window minN maxN = state $ \g ->
    makeCreatures window g minN maxN

makeCreatures
    :: forall species.
    Generate species
    => Window -> StdGen -> Natural -> Natural -> ([Creature species], StdGen)
makeCreatures window g minN maxN = foldr makeCreatures' ([], g') [1::Int .. nCreatures]
  where
    ((minX, minY), (maxX, maxY)) = window
    (nCreatures, g') = randomR (fromIntegral minN, fromIntegral maxN) g
    makeCreatures' _ (creatures, g0) = (c : creatures, g5)
      where
        -- TODO: extract to a primitive generator
        fakeSize = 10  -- TODO: add real creature sizes
        c = Creature{ position = (x, y)
                    , targetDir = dir
                    , currentDir = dir
                    , turnRate = tr
                    , species = s
                    , size = fakeSize}
        (x, g1) = randomR (minX + fakeSize / 2, maxX - fakeSize / 2) g0
        (y, g2) = randomR (minY + fakeSize / 2, maxY - fakeSize / 2) g1
        (dir, g3) = randomR (0, 2 * pi) g2
        (tr, g4) = randomR (turnRateRange @species) g3
        (s, g5) = generate g4

class Generate a where
    generate :: StdGen -> (a, StdGen)

    turnRateRange :: (Angle, Angle)
    turnRateRange = (pi / 4, pi / 2)

instance Generate Centipede where
    generate g =
        let (numSegments, g') = randomR (5, 15) g
        in  (Centipede{segments = replicate numSegments (0, 0)}, g')
    turnRateRange = (pi / 25, pi / 20)

instance Generate Flea where
    generate g =
        let (eagerness, g') = randomR (0.0, 1.0) g
        in  (Flea{idleTime = eagerness}, g')

instance Generate Ant where
    generate g = (Ant, g)

instance Generate Fly where
    generate g = (Fly, g)
