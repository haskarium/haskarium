{-# LANGUAGE NamedFieldPuns #-}

import           Graphics.Gloss (Display (InWindow), play, white)
import           Graphics.Gloss.Interface.Pure.Game (Event)
import           System.Random (StdGen, newStdGen, randomR)

import           Haskarium.Const (height, width)
import           Haskarium.Draw (draw)
import           Haskarium.Motion (updateCreature)
import           Haskarium.Types (Creature (..), Species (..), World)

main :: IO ()
main = do
    g <- newStdGen
    let (_g', startWorld) =
            makeCreatures
                (fromIntegral width / 2, fromIntegral height / 2)
                g
                [Fly, Flea{idleTime = 0}, Ant, Centipede{segments=[]}]
    play display white refreshRate startWorld draw onEvent onTick
  where
    display = InWindow "haskarium" (width, height) (0, 0)
    refreshRate = 60

makeCreatures :: (Float, Float) -> StdGen -> [Species] -> (StdGen, [Creature])
makeCreatures window g = foldr makeCreatures' (g, [])
  where
    (maxX, maxY) = window
    makeCreatures' species (g0, creatures) = (g5, c : creatures)
      where
        fake_size = 10  -- TODO: add real creature sizes
        c = Creature{ position = (x, y)
                    , direction = dir
                    , turnRate = tr
                    , species = s'
                    , size = fake_size}
        (x, g1) = randomR (-maxX + fake_size / 2, maxX - fake_size / 2) g0
        (y, g2) = randomR (-maxY + fake_size / 2, maxY - fake_size / 2) g1
        (dir, g3) = randomR (0, 2 * pi) g2
        (tr, g4) = case species of
            Centipede{} ->
                randomR (-pi / 34, -pi / 30) g3
            _ ->
                randomR (pi / 4, pi / 2) g3
        (s', g5) = case species of
            Centipede{} ->
                let (numSegments, gN) = randomR (5, 15) g4
                in (Centipede{segments = replicate numSegments (x, y)}, gN)
            Flea{} ->
                let (eagerness, gN) = randomR (0.0, 1.0) g4
                in (Flea{idleTime = eagerness}, gN)
            _ ->
                (species, g4)

onEvent :: Event -> World -> World
onEvent _ = id

onTick :: Float -> World -> World
onTick = map . updateCreature
