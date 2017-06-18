import           Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main =
    play display white refreshRate startWorld draw onEvent onTick
  where
    display = InWindow "haskarium" (800, 600) (0, 0)
    refreshRate = 60
    startWorld =
        [ Ant{position = (0, 0), direction = (-pi) / 4}
        , Ant{position = (-30, -30), direction = 3 * pi / 4}
        , Ant{position = (20, 40), direction = pi / 3}
        ]

type Angle = Float

data Ant = Ant{position :: !Point, direction :: !Angle}

type World = [Ant]

drawAnt :: Ant -> Picture
drawAnt Ant{position = (x, y)} =
    translate x y $ color red $ rectangleSolid 10 10

draw :: World -> Picture
draw world = pictures [drawAnt ant | ant <- world]

onEvent :: Event -> World -> World
onEvent _ world = world

onTick :: Float -> World -> World
onTick dt world = [move dt ant | ant <- world]

move :: Float -> Ant -> Ant
move dt Ant{position = (x, y), direction = dir} = Ant
    {position = (x + dx, y + dy), direction = dir + dt}
  where
    dx = speed * dt * cos dir
    dy = speed * dt * sin dir
    speed = 20
