import           Graphics.Gloss (Display (InWindow), play, white)
import           Graphics.Gloss.Interface.Pure.Game (Event)
import           System.Random (newStdGen)

import           Haskarium.Const (height, width)
import           Haskarium.Draw (draw)
import           Haskarium.Generate (makeCreatures)
import           Haskarium.Motion (updateCreature)
import           Haskarium.Types (SpeciesType (..), Time, World)

main :: IO ()
main = do
    g <- newStdGen
    let (_g', startWorld) =
            makeCreatures window g [SFly, SFlea, SAnt, SCentipede]
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

onTick :: Time -> World -> World
onTick = map . updateCreature
