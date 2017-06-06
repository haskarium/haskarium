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
draw _ = pictures
  [
    translate (100) (100) (Color picColor1 picShape1),
    translate (-100) (-100) (Color picColor2 picShape2)
  ]
  where
    picColor1 = red
    picShape1 = circleSolid (125)
    picColor2 = yellow
    picShape2 = triangle (125)

triangle :: Float -> Picture
triangle size = polygon
  [
    (0, size),
    (size, size / 2),
    (0, size / 2)
  ]

onEvent :: Event -> World -> World
onEvent _ world = world

onTick :: Float -> World -> World
onTick dt time = time + dt
