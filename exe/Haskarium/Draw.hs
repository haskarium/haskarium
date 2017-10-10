{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Haskarium.Draw
    ( draw
    ) where

import           Graphics.Gloss (Picture, blank, blue, circle, circleSolid,
                                 color, green, orange, pictures, polygon, red,
                                 rotate, translate)
import           Graphics.Gloss.Geometry.Angle (radToDeg)

import           Haskarium.Const (centipedeSegmentRadius)
import           Haskarium.Types (Creature (..), Species (..), SpeciesType (..),
                                  World, speciesType)

drawCreature :: Creature -> Picture
drawCreature Creature{position, species = Centipede segments} =
    pictures $ map draw' (position : segments)
  where
    draw' (x, y) =
      translate x y .
      color orange $
      circleSolid centipedeSegmentRadius
drawCreature Creature{position = (x, y), direction, species} =
    translate x y .
    rotate (- radToDeg direction) .
    figure $
    speciesType species

figure :: SpeciesType -> Picture
figure = \case
    SAnt ->
        color red $
        pictures
            [ triangleBody
            , translate (-5) 0 $ circle 5
            ]
    SFlea ->
        color blue $
        pictures
            [ triangleBody
            , translate (-5) 0 $ circle 5
            ]
    SFly ->
        color green $
        pictures
            [ triangleBody
            , translate 5   5  $ circle 5
            , translate 5 (-5) $ circle 5
            ]
    _ ->
        blank
  where
    triangleBody = polygon
        [ ( 5,  0)
        , (-5, -5)
        , (-5,  5)
        ]

draw :: World -> Picture
draw = pictures . map drawCreature
