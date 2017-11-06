{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Haskarium.Motion
    ( Interactive (..)
    ) where

import           Graphics.Gloss (Point)
import           Graphics.Gloss.Geometry.Angle (normalizeAngle)
import           Graphics.Gloss.Geometry.Line (intersectSegHorzLine,
                                               intersectSegVertLine)
import           Graphics.Gloss.Interface.Pure.Game (Event)
import           System.Random (StdGen, randomR)

import           Haskarium.Const
import           Haskarium.Types (Angle, Ant, Centipede (..), Creature (..),
                                  Distance, Flea (..), Fly, Speed, Time,
                                  World (..))
import           Haskarium.Util (distance)

updateCreature :: Interactive (Creature s) => Time
                                           -> Creature s
                                           -> (StdGen, [Creature s])
                                           -> (StdGen, [Creature s])
updateCreature dt creature (g, creatures) = (g', c : creatures)
    where
      (g', c) = creatureTurn (onTick dt creature) dt g

class Interactive a where
    onTick :: Time -> a -> a
    onTick _ = id

    onEvent :: Event -> a -> a
    onEvent _ = id

instance Interactive World where
    onTick dt w@World{ants, centipedes, fleas, flies, randomGen = g0} =
      World { ants        = ants'
            , centipedes  = centipedes'
            , fleas       = fleas'
            , flies       = flies'
            , randomGen   = g4
            }
      where
        (g1, ants')       = foldr (updateCreature dt) (g0,[]) ants
        (g2, centipedes') = foldr (updateCreature dt) (g1,[]) centipedes
        (g3, fleas')      = foldr (updateCreature dt) (g2,[]) fleas
        (g4, flies')      = foldr (updateCreature dt) (g3,[]) flies


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

run :: Speed -> Time -> Creature s -> Creature s
run speed dt creature = creatureMovedCheckCollisions creature dist
  where
    dist = dt * speed

instance Interactive (Creature Centipede) where
    onTick dt creature@Creature{species} =
        runHead{species = Centipede{segments=newSegments}}
      where
        Centipede{segments} = species

        runHead = run 10 dt creature

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

creatureTurn :: Creature s -> Time -> StdGen -> (StdGen, Creature s)
creatureTurn creature dt g =
    (g', creature{targetDir = newTdir, currentDir = newCdir})
      where
        Creature { targetDir = tdir
                 , currentDir = cdir
                 , turnRate = tr
                 } = creature
        (newTdir, g') =
            if   (cdir <= tdir && cdir' >= tdir)
              || (cdir >= tdir && cdir' <= tdir)
            then
              randomR (0, 2 * pi) g
            else
              (tdir, g)
        newCdir
          | cdir' < 0       = cdir' + 2 * pi
          | cdir' > 2 * pi  = cdir' - 2 * pi
          | otherwise       = cdir'
        cdir' = cdir + trSign * tr * dt
        trSign =
          if (delta > -pi && delta < 0) || delta > pi
          then -1
          else 1
        delta = tdir - cdir

creatureMovedCheckCollisions :: Creature s -> Distance -> Creature s
creatureMovedCheckCollisions creature dist
    | dist <= 0 = creature
    | otherwise = creature{position = advance creature dist}

pointMoved :: Point -> Distance -> Angle -> Point
pointMoved (x, y) dist direction = (x + dx, y + dy)
  where
    dx = dist * cos direction
    dy = dist * sin direction

advance :: Creature s -> Distance -> Point
advance Creature{position = p0, currentDir = direction, size} dist =
    checkDirs pU pD pL pR
  where
    checkDirs (Just pu) _ _ _ | isMovedUp    = pu
    checkDirs _ (Just pd) _ _ | isMovedDown  = pd
    checkDirs _ _ (Just pl) _ | isMovedLeft  = pl
    checkDirs _ _ _ (Just pr) | isMovedRight = pr
    checkDirs _ _ _ _                        = p1
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
