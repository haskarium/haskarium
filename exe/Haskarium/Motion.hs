{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Haskarium.Motion
    ( Interactive (..)
    ) where

import           Control.Monad ((>=>))
import           Graphics.Gloss (Point)
import           Graphics.Gloss.Geometry.Angle (normalizeAngle)
import           Graphics.Gloss.Geometry.Line (intersectSegHorzLine,
                                               intersectSegVertLine)
import           Graphics.Gloss.Interface.Pure.Game (Event)

import           Haskarium.Const
import           Haskarium.Types (Angle, Ant, Centipede(..), Creature (..),
                                  Distance, Flea(..), Fly, LandCreature(..),
                                  Sim, Speed, Time, World (..), getPoints,
                                  landCreatures)
import           Haskarium.Util (distance, randomRS, worldOf)

class Interactive a where
    onTick :: Time -> a -> Sim a
    onTick _ = pure

    onEvent :: Event -> a -> Sim a
    onEvent _ = pure

instance Interactive World where
    onTick dt World{ants, centipedes, fleas, flies} = World
        <$> traverse (onTick dt) ants
        <*> traverse (onTick dt) centipedes
        <*> traverse (onTick dt) fleas
        <*> traverse (onTick dt) flies

instance Interactive (Creature Ant) where
    onTick dt = run 20 dt True >=> creatureTurn dt

instance Interactive (Creature Fly) where
    onTick dt = run 200 dt False >=> creatureTurn dt

instance Interactive (Creature Flea) where
    onTick dt creature@Creature{species = Flea{idleTime}} = do
        newCreature <-
            if idleTime < fleaMaxIdleTime then
                pure creature{species = Flea{idleTime = idleTime + dt}}
            else do
                moved <- creatureMovedCheckCollisions creature False
                                                      fleaJumpDistance
                pure moved
                    { species = Flea
                        { idleTime = idleTime + dt - fleaMaxIdleTime
                        }
                    }
        creatureTurn dt newCreature
      where
        fleaJumpDistance = 100

run :: Speed -> Time -> Bool -> Creature s -> Sim (Creature s)
run speed dt isLand creature = creatureMovedCheckCollisions creature isLand dist
  where
    dist = dt * speed

instance Interactive (Creature Centipede) where
    onTick dt creature@Creature{species} = do
        newHead <- run 10 dt True creature
        let newCreature = newHead{species = Centipede{segments=newSegments newHead}}
        creatureTurn dt newCreature
      where
        Centipede{segments} = species

        newSegments newHead = runChain (position newHead) segments

        runChain _ [] = []
        runChain p@(px, py) (c@(cx, cy) : next) = let
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

creatureTurn :: Time -> Creature s -> Sim (Creature s)
creatureTurn dt creature = update <$> getTargetDir
  where
    getTargetDir
        | reachedTargetDir = randomRS (0, 2 * pi)
        | otherwise = pure targetDir
    update targetDir' =
        creature{targetDir = targetDir', currentDir = newCurrentDir}
    reachedTargetDir =
           (currentDir  <= targetDir && targetDir <= currentDir')
        || (currentDir' <= targetDir && targetDir <= currentDir )
    Creature{targetDir, currentDir, turnRate} = creature
    newCurrentDir
        | currentDir' < 0       = currentDir' + 2 * pi
        | currentDir' > 2 * pi  = currentDir' - 2 * pi
        | otherwise             = currentDir'
    currentDir' = currentDir + trSign * turnRate * dt
    trSign =
        if (-pi < delta && delta < 0) || delta > pi then
            -1
        else
            1
    delta = targetDir - currentDir

tryToAvoid :: Creature s -> Distance -> [Point] -> (Distance, Angle)
tryToAvoid creature@Creature{currentDir, size} dist obstacles =
    avoidance nearestCreature
  where
    nearestCreature = findNearest creature viewDistance obstacles
    viewDistance = 6 * size
    avoidance (Just (rd, ra)) =
        ( newDist rd, normalizeAngle (currentDir - newAngleDiff ra rd))
    avoidance _               = (dist, currentDir)
    newDist d = if d > 3 * size then dist else 0
    newAngleDiff a d = if d > 3 * size then pi / 2 - a else 0


findNearest:: Creature s -> Float -> [Point] -> Maybe (Distance, Angle)
findNearest Creature{position = (x0, y0), currentDir} dist0 cs0 =
    findNearest' cs0 dist0 Nothing
  where
    findNearest' [] _ result = result
    findNearest' ((x1, y1):xs) dist result
      | rd > 0 && rd < dist && inViewAngle = findNearest' xs rd (Just (rd, ra))
      | otherwise            = findNearest' xs dist result
      where
        rd = distance (x0, y0) (x1, y1)
        ra = normalizeAngle $ atan2 (ax * by - bx * ay) (ax * bx + ay * by)
        ax = cos currentDir
        ay = sin currentDir
        bx = x1 - x0
        by = y1 - y0
        viewAngle = pi / 2
        inViewAngle = (viewAngle / 2 > ra) || ((2 * pi - viewAngle / 2) < ra)

creatureMovedCheckCollisions :: Creature s -> Bool -> Distance ->
                                Sim (Creature s)
creatureMovedCheckCollisions creature isLand dist = do
    cs <- worldOf landCreatures :: Sim [LandCreature]

    let obstacles = if isLand then
                        mconcat [ getPoints c | LandCreature c <- cs ]
                    else
                        []

    let (newDist, newDir) = tryToAvoid creature dist obstacles
    let creature' = creature{currentDir = newDir}

    pure creature{position = advance creature' newDist}

pointMoved :: Point -> Distance -> Angle -> Point
pointMoved (x, y) dist direction = (x + dx, y + dy)
  where
    dx = dist * cos direction
    dy = dist * sin direction

advance :: Creature s -> Distance -> Point
advance Creature{position = p0, currentDir, size} dist =
    checkDirs pU pD pL pR
  where
    checkDirs (Just pu) _ _ _ | isMovedUp    = pu
    checkDirs _ (Just pd) _ _ | isMovedDown  = pd
    checkDirs _ _ (Just pl) _ | isMovedLeft  = pl
    checkDirs _ _ _ (Just pr) | isMovedRight = pr
    checkDirs _ _ _ _                        = p1
    normDir = normalizeAngle currentDir
    isMovedUp = normDir < pi
    isMovedDown = normDir > pi
    isMovedLeft = normDir > pi/2 && normDir < 3 * pi / 2
    isMovedRight = normDir < pi/2 || normDir > 3 * pi / 2
    p1 = pointMoved p0 dist currentDir
    pU = intersectSegHorzLine p0 p1 ((fromIntegral height / 2) - size / 2)
    pD = intersectSegHorzLine p0 p1 ((- fromIntegral height / 2) + size / 2)
    pL = intersectSegVertLine p0 p1 ((- fromIntegral width / 2) + size / 2)
    pR = intersectSegVertLine p0 p1 ((fromIntegral width / 2) - size / 2)
