{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Haskarium.Draw
    ( draw
    ) where

import           Data.Monoid ((<>))
import           Graphics.Gloss (Picture, blue, circle, circleSolid, color,
                                 green, orange, polygon, red, rotate, translate,
                                 scale)
import           Graphics.Gloss.Geometry.Angle (radToDeg)

import           Haskarium.Types (Ant, Centipede (..), Creature (..), Flea, Fly,
                                  World (..))

class Drawable a where
    draw :: a -> Picture

instance Drawable (Creature Centipede) where
    draw Creature{position, size, species = Centipede segments} =
        foldMap draw' (position : segments)
      where
        draw' (x, y) =
            translate x y .
            color orange $
            circleSolid size

instance Drawable (Creature Ant) where
    draw =
        drawCreature $ color red $ triangleBody <> translate (-0.5) 0 (circle 0.5)

instance Drawable (Creature Flea) where
    draw =
        drawCreature $ color blue $ triangleBody <> translate (-0.5) 0 (circle 0.5)

instance Drawable (Creature Fly) where
    draw =
        drawCreature $
        color green $
        triangleBody <> translate 0.5 0.5 (circle 0.5) <> translate 0.5 (-0.5) (circle 0.5)

drawCreature :: Picture -> Creature s -> Picture
drawCreature body Creature{position = (x, y), currentDir, size} =
    translate x y $ scale size size $ rotate (- radToDeg currentDir) body

triangleBody :: Picture
triangleBody = polygon
    [ ( 0.5,  0)
    , (-0.5, -0.5)
    , (-0.5,  0.5)
    ]

instance Drawable World where
    draw World{ants, centipedes, fleas, flies} =
        foldMap draw ants <>
        foldMap draw centipedes <>
        foldMap draw fleas <>
        foldMap draw flies
