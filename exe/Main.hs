import           Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = play display white refreshRate initial draw onEvent onTick
  where
    display = InWindow "haskarium" (800, 600) (0, 0)
    refreshRate = 60

type World = Float

initial :: World
initial = 0

draw :: World -> Picture
draw time = text (show (floor time :: Int) ++ " seconds")

onEvent :: Event -> World -> World
onEvent _ world = world

onTick :: Float -> World -> World
onTick dt time = time + dt
