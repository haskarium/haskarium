{-# LANGUAGE DeriveFunctor #-}

module Haskarium.Types
    ( Angle
    , RadiansPerSecond
    , Ant (..)
    , Centipede (..)
    , Creature (..)
    , Distance
    , Flea (..)
    , Fly (..)
    , Speed
    , Time
    , World (..)
    , Rnd (..)
    ) where

import           Graphics.Gloss (Point)
import           System.Random (StdGen)

type Angle = Float
type Distance = Float
type RadiansPerSecond = Float
type Speed = Float
type Time = Float

data Creature species = Creature
    { position :: !Point
    , targetDir :: !Angle
    , currentDir :: !Angle
    , turnRate :: !RadiansPerSecond
    , species :: !species
    , size :: !Distance
    }

data Ant = Ant

newtype Centipede = Centipede{segments :: [Point]}

newtype Flea = Flea{idleTime :: Time}

data Fly = Fly

data World = World
    { ants :: ![Creature Ant]
    , centipedes :: ![Creature Centipede]
    , fleas :: ![Creature Flea]
    , flies :: ![Creature Fly]
    , randomGen :: !StdGen
    }

newtype Rnd a = Rnd
    { runRnd :: StdGen -> (StdGen, a)
    }
    deriving (Functor)

instance Applicative Rnd where
    pure x = Rnd $ \g ->
        (g, x)

    u <*> v = Rnd $ \g ->
        let
            (gU, f)  = runRnd u g
            (gV, x) = runRnd v gU
        in
            (gV, f x)
