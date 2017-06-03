import           Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = play display green refreshRate initial draw onEvent onTick
  where
    display = InWindow "haskarium" (800, 600) (0, 0)
    refreshRate = 60

type World = Float

initial :: World
initial = 0

draw :: World -> Picture
draw time = pictures
    [ translate (-300) 100 $ text (show (floor time :: Int) ++ " seconds")
    , color red $ circleSolid 50
    , rotate (time * 50) $ color yellow $ triangle 150 150 150
    ]

onEvent :: Event -> World -> World
onEvent _ world = world

onTick :: Float -> World -> World
onTick dt time = time + dt

triangle :: Float -> Float -> Float -> Picture
triangle a b c = polygon
    [ (0, 0)
    , (a, 0)
    , (c * cosA, c * sinA)
    ]
  where
    cosA = (a*a + c*c - b*b) / (2*a*c)
    sinA = sin (acos cosA)
