{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Haskarium.Types
    ( Angle
    , Ant (..)
    , Centipede (..)
    , Creature (..)
    , Distance
    , Flea (..)
    , Fly (..)
    , RadiansPerSecond
    , Sim
    , Speed
    , Time
    , World (..)
    , Located (..)
    , LandCreature(..)
    , landCreatures
    ) where

import           Control.Monad.RWS.Strict (RWS)
import           Graphics.Gloss           (Point)
import           System.Random            (StdGen)

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
    }

type Sim a = RWS World () StdGen a

class Located a where
  getPoints :: a -> [Point]

instance Located (Creature Ant) where
  getPoints Creature{position} = [position]

instance Located (Creature Flea) where
  getPoints Creature{position} = [position]

instance Located (Creature Centipede) where
  getPoints Creature{position, species} = position : segments species

data LandCreature
    = forall species
    . Located (Creature species)
    => LandCreature (Creature species)

landCreatures :: World -> [LandCreature]
landCreatures World{ants, centipedes, fleas} = mconcat
  [ map LandCreature ants
  , map LandCreature centipedes
  , map LandCreature fleas
  ]
