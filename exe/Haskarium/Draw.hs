{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Haskarium.Draw
    ( draw
    ) where

import           Data.Monoid ((<>))
import           Graphics.Gloss (Picture, blank, blue, circle, circleSolid,
                                 color, green, orange, polygon, red, rotate,
                                 translate)
import           Graphics.Gloss.Geometry.Angle (radToDeg)

import           Haskarium.Const (centipedeSegmentRadius)
import           Haskarium.Types (Ant, Centipede (..), Creature (..), Flea, Fly,
                                  IsSpecies, SpeciesType (..), World (..),
                                  speciesType)

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
    draw = drawCreature

instance Drawable (Creature Flea) where
    draw = drawCreature

instance Drawable (Creature Fly) where
    draw = drawCreature

drawCreature :: IsSpecies s => Creature s -> Picture
drawCreature Creature{position = (x, y), direction, species} =
    translate x y .
    rotate (- radToDeg direction) .
    figure $
    speciesType species

figure :: SpeciesType -> Picture
figure = \case
    SAnt ->
        color red $ triangleBody <> translate (-5) 0 (circle 5)
    SFlea ->
        color blue $ triangleBody <> translate (-5) 0 (circle 5)
    SFly ->
        color green $
        triangleBody <> translate 5 5 (circle 5) <> translate 5 (-5) (circle 5)
    _ ->
        blank
  where
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
