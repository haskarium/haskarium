{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Haskarium.Draw
    ( draw
    ) where

import           Data.Monoid ((<>))
import           Graphics.Gloss (Picture, blue, circle, circleSolid, color,
                                 green, orange, polygon, red, rotate, translate)
import           Graphics.Gloss.Geometry.Angle (radToDeg)

import           Haskarium.Const (centipedeSegmentRadius)
import           Haskarium.Types (Ant, Centipede (..), Creature (..), Flea, Fly,
                                  World (..))

class Drawable a where
    draw :: a -> Picture

instance Drawable (Creature Centipede) where
    draw Creature{position, species = Centipede segments} =
        mconcat $ map draw' (position : segments)
      where
        draw' (x, y) =
          translate x y .
          color orange $
          circleSolid centipedeSegmentRadius

instance Drawable (Creature Ant) where
    draw =
        drawCreature $ color red $ triangleBody <> translate (-5) 0 (circle 5)

instance Drawable (Creature Flea) where
    draw =
        drawCreature $ color blue $ triangleBody <> translate (-5) 0 (circle 5)

instance Drawable (Creature Fly) where
    draw =
        drawCreature $
        color green $
        triangleBody <> translate 5 5 (circle 5) <> translate 5 (-5) (circle 5)

drawCreature :: Picture -> Creature s -> Picture
drawCreature body Creature{position = (x, y), currentDir = direction} =
    translate x y $ rotate (- radToDeg direction) body

triangleBody :: Picture
triangleBody = polygon
    [ ( 5,  0)
    , (-5, -5)
    , (-5,  5)
    ]

instance Drawable World where
    draw World{ants, centipedes, fleas, flies} = mconcat $
        map draw ants <>
        map draw centipedes <>
        map draw fleas <>
        map draw flies
