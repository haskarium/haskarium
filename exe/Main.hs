{-# LANGUAGE NamedFieldPuns #-}

import           Graphics.Gloss (Display (InWindow), play, white)
import           System.Random (newStdGen)

import           Haskarium.Const (height, width)
import           Haskarium.Draw (draw)
import           Haskarium.Generate (makeCreatures)
import           Haskarium.Motion (onEvent, onTick)
import           Haskarium.Types (World (..))

main :: IO ()
main = do
    g0 <- newStdGen
    let (g1, ants)        = makeCreatures window g0 10
    let (g2, centipedes)  = makeCreatures window g1 10
    let (g3, fleas)       = makeCreatures window g2 10
    let (g4, flies)       = makeCreatures window g3 10
    let startWorld = World{ants, centipedes, fleas, flies, randomGen = g4}
    play display white refreshRate startWorld draw onEvent onTick
  where
    window =
        ( (- fromIntegral width / 2, - fromIntegral height / 2)
        , (  fromIntegral width / 2,   fromIntegral height / 2)
        )
    display = InWindow "haskarium" (width, height) (0, 0)
    refreshRate = 60
