{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Haskarium.Generate
    ( makeGame
    ) where

import           Control.Monad (replicateM)
import           Graphics.Gloss (Point)
import           Numeric.Natural (Natural)

import           Haskarium.Types (Angle, Ant (..), Centipede (..),
                                  Creature (..), Flea (..), Fly (..), Rnd,
                                  World (..))
import           Haskarium.Util (andThen, randomRS)

type Window = (Point, Point)

makeGame :: Window -> Rnd World
makeGame window = World
    <$> makeCreatures window 0 10
    <*> makeCreatures window 0 10
    <*> makeCreatures window 0 10
    <*> makeCreatures window 0 10

makeCreatures
    :: forall species. Generate species
    => Window -> Natural -> Natural -> Rnd [Creature species]
makeCreatures window minN maxN =
    pNCreatures `andThen` \nCreatures ->
    replicateM nCreatures makeCreature
  where
    ((minX, minY), (maxX, maxY)) = window
    pNCreatures = randomRS (fromIntegral minN, fromIntegral maxN)
    makeCreature = Creature
        <$> pPos
        <*> pDir
        <*> pDir
        <*> pTR
        <*> generate
        <*> pure fakeSize
      where
        fakeSize = 10  -- TODO: add real creature sizes
        px = randomRS (minX + fakeSize / 2, maxX - fakeSize / 2)
        py = randomRS (minY + fakeSize / 2, maxY - fakeSize / 2)
        pPos = (,) <$> px <*> py
        pDir = randomRS (0, 2 * pi)
        pTR = randomRS (turnRateRange @species)

class Generate a where
    generate :: Rnd a

    turnRateRange :: (Angle, Angle)
    turnRateRange = (pi / 4, pi / 2)

instance Generate Centipede where
    generate = mkCentipede <$> randomRS (5, 15)
      where
        mkCentipede numSegments =
            Centipede{segments = replicate numSegments (0, 0)}

    turnRateRange = (pi / 25, pi / 20)

instance Generate Flea where
    generate = Flea <$> randomRS (0.0, 1.0)

instance Generate Ant where
    generate = pure Ant

instance Generate Fly where
    generate = pure Fly
