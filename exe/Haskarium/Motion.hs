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
    onTick dt = run 20 dt >=> creatureTurn dt

instance Interactive (Creature Fly) where
    onTick dt = run 200 dt >=> creatureTurn dt

instance Interactive (Creature Flea) where
    onTick dt creature@Creature{species = Flea{idleTime}} = do
        newCreature <-
            if idleTime < fleaMaxIdleTime then
                pure creature{species = Flea{idleTime = idleTime + dt}}
            else do
                moved <- creatureMovedCheckCollisions creature fleaJumpDistance
                pure moved
                    { species = Flea
                        { idleTime = idleTime + dt - fleaMaxIdleTime
                        }
                    }
        creatureTurn dt newCreature
      where
        fleaJumpDistance = 100

run :: Speed -> Time -> Creature s -> Sim (Creature s)
run speed dt creature = creatureMovedCheckCollisions creature dist
  where
    dist = dt * speed

instance Interactive (Creature Centipede) where
    onTick dt creature@Creature{species} = do
        newHead <- run 10 dt creature
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

creatureMovedCheckCollisions :: Creature s -> Distance -> Sim (Creature s)
creatureMovedCheckCollisions creature dist
    | dist <= 0 = pure creature
    | otherwise = do
        cs <- worldOf landCreatures :: Sim [LandCreature]

        -- TODO: look out
        let _avoid = mconcat [ getPoints c | LandCreature c <- cs ]
        let checkedDist = dist

        pure creature{position = advance creature checkedDist}

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
