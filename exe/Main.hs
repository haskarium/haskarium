{-# LANGUAGE NamedFieldPuns #-}

import           Graphics.Gloss (Display (InWindow), play, white)
import           Graphics.Gloss.Interface.Pure.Game (Event)
import           System.Random (newStdGen)

import           Haskarium.Const (height, width)
import           Haskarium.Draw (draw)
import           Haskarium.Generate (makeCreatures)
import           Haskarium.Motion (updateCreature)
import           Haskarium.Types (World (..))

main :: IO ()
main = do
    g0 <- newStdGen
    let (g1, ants)        = makeCreatures window g0 1
    let (g2, centipedes)  = makeCreatures window g1 1
    let (g3, fleas)       = makeCreatures window g2 1
    let (_g4, flies)      = makeCreatures window g3 1
    let startWorld = World{ants, centipedes, fleas, flies}
    play display white refreshRate startWorld draw onEvent onTick
  where
    window =
        ( (- fromIntegral width / 2, - fromIntegral height / 2)
        , (  fromIntegral width / 2,   fromIntegral height / 2)
        )
    display = InWindow "haskarium" (width, height) (0, 0)
    refreshRate = 60

onEvent :: Event -> World -> World
onEvent _ = id

onTick :: Float -> World -> World
onTick dt World{ants, centipedes, fleas, flies} = World
    { ants        = map (updateCreature dt) ants
    , centipedes  = map (updateCreature dt) centipedes
    , fleas       = map (updateCreature dt) fleas
    , flies       = map (updateCreature dt) flies
    }
