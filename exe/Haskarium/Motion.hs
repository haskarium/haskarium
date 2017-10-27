{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Haskarium.Motion
    ( updateCreature
    , Interactive (..)
    ) where

import           Graphics.Gloss (Point)
import           Graphics.Gloss.Geometry.Angle (normalizeAngle)
import           Graphics.Gloss.Geometry.Line (intersectSegHorzLine,
                                               intersectSegVertLine)
import           Graphics.Gloss.Interface.Pure.Game (Event)

import           Haskarium.Const
import           Haskarium.Types (Angle, Ant, Centipede (..), Creature (..),
                                  Distance, Flea (..), Fly, IsSpecies,
                                  SpeciesType (SFlea), Speed, Time, World (..),
                                  speciesType)
import           Haskarium.Util (distance)

updateCreature :: Interactive (Creature s) => Time -> Creature s -> Creature s
updateCreature dt creature@Creature{turnRate} =
    creatureTurn (onTick dt creature) ddir
  where
    ddir = turnRate * dt

class Interactive a where
    onTick :: Time -> a -> a
    onTick _ = id

    onEvent :: Event -> a -> a
    onEvent _ = id

instance Interactive World where
    onTick dt World{ants, centipedes, fleas, flies} = World
        { ants        = map (updateCreature dt) ants
        , centipedes  = map (updateCreature dt) centipedes
        , fleas       = map (updateCreature dt) fleas
        , flies       = map (updateCreature dt) flies
        }

instance Interactive (Creature Ant) where
    onTick = run 20

instance Interactive (Creature Fly) where
    onTick = run 200

instance Interactive (Creature Flea) where
    onTick dt creature@Creature{species = Flea{idleTime}} =
        if idleTime < fleaMaxIdleTime then
            creature{species = Flea{idleTime = idleTime + dt}}
        else
            (creatureMovedCheckCollisions creature fleaJumpDistance)
                {species = Flea{idleTime = idleTime + dt - fleaMaxIdleTime}}
      where
        fleaJumpDistance = 100

run :: IsSpecies s => Speed -> Time -> Creature s -> Creature s
run speed dt creature = creatureMovedCheckCollisions creature dist
  where
    dist = dt * speed

instance Interactive (Creature Centipede) where
    onTick dt creature@Creature{species} =
        runHead{species = Centipede{segments=newSegments}}
      where
        Centipede{segments} = species

        runHead = run 6 dt creature

        newSegments = runChain (position runHead) segments

        runChain _ [] = []
        runChain p@(px, py) (c@(cx, cy) : next) =
            let
            dist = distance p c
            c' =
                if dist < maxNeck then
                    c
                else let
                    dist' = maxNeck
                    squeezeFactor = dist / dist'
                    cx' = px - (px - cx) / squeezeFactor
                    cy' = py - (py - cy) / squeezeFactor
                    in (cx', cy')
            in c' : runChain c' next

        maxNeck = 1.5 * centipedeSegmentRadius

creatureTurn :: Creature s -> Angle -> Creature s
creatureTurn creature@Creature{direction} ddir =
    creature{direction = direction + ddir}

creatureMovedCheckCollisions
    :: IsSpecies s => Creature s -> Distance -> Creature s
creatureMovedCheckCollisions creature@Creature{position, species} dist
    | dist <= 0 = creature
    | otherwise =
        case maybeCollision of
            Nothing         -> creatureMoved creature dist
            Just collision' -> creatureMovedWithCollisions collision'
          where
            maybeCollision = checkCollisions creature dist
            creatureMovedWithCollisions (collision, new_dir) =
                case speciesType species of
                    SFlea -> creatureMoved creature distToCol
                    _     ->
                        creatureMovedCheckCollisions
                            (creatureMoved creature distToCol)
                                {direction = new_dir}
                            (dist - distToCol)
              where
                distToCol = distance position collision

creatureMoved :: Creature s -> Distance -> Creature s
creatureMoved creature@Creature{position = (x, y), direction} dist =
    creature{position = pointMoved (x, y) dist direction}

pointMoved :: Point -> Distance -> Angle -> Point
pointMoved (x, y) dist direction = (x + dx, y + dy)
  where
    dx = dist * cos direction
    dy = dist * sin direction

checkCollisions :: Creature s -> Distance -> Maybe (Point, Angle)
checkCollisions Creature{position = p0, direction, size} dist =
    checkDirs pU pD pL pR
  where
    checkDirs (Just pu) _ _ _ | isMovedUp    = Just (pu, -direction)
    checkDirs _ (Just pd) _ _ | isMovedDown  = Just (pd, -direction)
    checkDirs _ _ (Just pl) _ | isMovedLeft  = Just (pl, pi - direction)
    checkDirs _ _ _ (Just pr) | isMovedRight = Just (pr, pi - direction)
    checkDirs _ _ _ _                        = Nothing
    normDir = normalizeAngle direction
    isMovedUp = normDir < pi
    isMovedDown = normDir > pi
    isMovedLeft = normDir > pi/2 && normDir < 3 * pi / 2
    isMovedRight = normDir < pi/2 || normDir > 3 * pi / 2
    p1 = pointMoved p0 dist direction
    pU = intersectSegHorzLine p0 p1 ((fromIntegral height / 2) - size / 2)
    pD = intersectSegHorzLine p0 p1 ((- fromIntegral height / 2) + size / 2)
    pL = intersectSegVertLine p0 p1 ((- fromIntegral width / 2) + size / 2)
    pR = intersectSegVertLine p0 p1 ((fromIntegral width / 2) - size / 2)
