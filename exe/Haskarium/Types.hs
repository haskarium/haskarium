{-# LANGUAGE LambdaCase #-}

module Haskarium.Types
    ( Creature (..)
    , Species (..)
    , SpeciesType (..)
    , World
    , speciesType
    ) where

import           Graphics.Gloss (Point)

type Angle = Float
type RadiansPerSecond = Float

data Creature = Creature
    { position  :: !Point
    , direction :: !Angle
    , turnRate  :: !RadiansPerSecond
    , species   :: !Species
    , size      :: !Float
    }

data Species
    = Ant | Flea{idleTime :: !Float} | Fly | Centipede{segments :: ![Point]}

data SpeciesType = SAnt | SCentipede | SFlea | SFly

speciesType :: Species -> SpeciesType
speciesType = \case
    Ant{}       -> SAnt
    Centipede{} -> SCentipede
    Flea{}      -> SFlea
    Fly{}       -> SFly

type World = [Creature]
