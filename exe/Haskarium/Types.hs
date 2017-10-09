module Haskarium.Types
    ( Creature (..)
    , Species (..)
    , World
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

type World = [Creature]
