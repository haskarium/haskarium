{-# LANGUAGE NamedFieldPuns #-}

module Haskarium.Motion
    ( updateCreature
    ) where

import           Graphics.Gloss.Geometry.Angle (normalizeAngle)
import           Graphics.Gloss.Geometry.Line (intersectSegHorzLine,
                                               intersectSegVertLine)
import           Graphics.Gloss.Interface.Pure.Game (Point)

import           Haskarium.Const
import           Haskarium.Types (Creature (..), Species (..))
import           Haskarium.Util (distance)

updateCreature :: Float -> Creature -> Creature
updateCreature dt creature@Creature{turnRate, species} =
    creatureTurn updateCreature' ddir
  where
    updateCreature' = case species of
        Ant            -> run 20
        Fly            -> run 200
        Flea{idleTime} -> jump idleTime 100
        Centipede{}    -> updateCentipede
    ddir = turnRate * dt
    run speed = creatureMovedCheckCollisions creature dist
      where
        dist = dt * speed
    jump idleTime dist =
        if idleTime < fleaMaxIdleTime then
            creature{species = Flea{idleTime = idleTime + dt}}
        else
            (creatureMovedCheckCollisions creature dist)
                {species = Flea{idleTime = idleTime + dt - fleaMaxIdleTime}}
    updateCentipede = runHead { species = Centipede{segments=newSegments} }
      where
        Centipede{segments} = species

        runHead = run 6

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

creatureTurn :: Creature -> Float -> Creature
creatureTurn creature@Creature{direction} ddir =
    creature{direction = direction + ddir}

creatureMovedCheckCollisions :: Creature -> Float -> Creature
creatureMovedCheckCollisions creature@Creature{position, species} dist
    | dist <= 0 = creature
    | otherwise =
        case maybeCollision of
            Nothing         -> creatureMoved creature dist
            Just collision' -> creatureMovedWithCollisions collision'
          where
            maybeCollision = checkCollisions creature dist
            creatureMovedWithCollisions (collision, new_dir) =
                case species of
                    Flea{} -> creatureMoved creature distToCol
                    _      -> creatureMovedCheckCollisions
                                  (creatureMoved creature distToCol)
                                      {direction = new_dir}
                                  (dist - distToCol)
              where
                distToCol = distance position collision

creatureMoved :: Creature -> Float -> Creature
creatureMoved creature@Creature{position = (x, y), direction} dist =
    creature{position = pointMoved (x, y) dist direction}

pointMoved :: Point -> Float -> Float-> Point
pointMoved (x, y) dist direction = (x + dx, y + dy)
  where
    dx = dist * cos direction
    dy = dist * sin direction

checkCollisions :: Creature -> Float -> Maybe (Point, Float)
checkCollisions Creature{position = p0, direction, size} dist =
    checkDirs pU pD pL pR
  where
    checkDirs (Just pu) _ _ _ | isMovedUp    = Just (pu, -direction)
    checkDirs _ (Just pd) _ _ | isMovedDown  = Just (pd, -direction)
    checkDirs _ _ (Just pl) _ | isMovedLeft  = Just (pl, pi - direction)
    checkDirs _ _ _ (Just pr) | isMovedRight = Just (pr, pi - direction)
    checkDirs _ _ _ _         = Nothing
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
