{-# LANGUAGE LambdaCase #-}

module Haskarium.Types
    ( Angle
    , RadiansPerSecond
    , Creature (..)
    , Distance
    , Species (..)
    , SpeciesType (..)
    , Speed
    , Time
    , World
    , speciesType
    ) where

import           Graphics.Gloss (Point)

type Angle = Float
type Distance = Float
type RadiansPerSecond = Float
type Speed = Float
type Time = Float

data Creature = Creature
    { position  :: !Point
    , direction :: !Angle
    , turnRate  :: !RadiansPerSecond
    , species   :: !Species
    , size      :: !Distance
    }

data Species
    = Ant | Flea{idleTime :: !Time} | Fly | Centipede{segments :: ![Point]}

data SpeciesType = SAnt | SCentipede | SFlea | SFly

speciesType :: Species -> SpeciesType
speciesType = \case
    Ant{}       -> SAnt
    Centipede{} -> SCentipede
    Flea{}      -> SFlea
    Fly{}       -> SFly

type World = [Creature]
