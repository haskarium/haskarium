module Haskarium.Types
    ( Angle
    , RadiansPerSecond
    , Ant (..)
    , Centipede (..)
    , Creature (..)
    , Distance
    , Flea (..)
    , Fly (..)
    , SpeciesType (..)
    , Speed
    , Time
    , World (..)
    , IsSpecies (..)
    ) where

import           Graphics.Gloss (Point)

type Angle = Float
type Distance = Float
type RadiansPerSecond = Float
type Speed = Float
type Time = Float

data Creature species = Creature
    { position  :: !Point
    , direction :: !Angle
    , turnRate  :: !RadiansPerSecond
    , species   :: !species
    , size      :: !Distance
    }

data SpeciesType = SAnt | SCentipede | SFlea | SFly

class IsSpecies species where
    speciesType :: species -> SpeciesType

data Ant = Ant

instance IsSpecies Ant where
    speciesType _ = SAnt

newtype Centipede = Centipede{segments :: [Point]}

instance IsSpecies Centipede where
    speciesType _ = SCentipede

newtype Flea = Flea{idleTime :: Time}

instance IsSpecies Flea where
    speciesType _ = SFlea

data Fly = Fly

instance IsSpecies Fly where
    speciesType _ = SFly

data World = World
    { ants :: [Creature Ant]
    , centipedes :: [Creature Centipede]
    , fleas :: [Creature Flea]
    , flies :: [Creature Fly]
    }
