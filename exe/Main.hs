import           Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main =
    play display white refreshRate startWorld draw onEvent onTick
  where
    display = InWindow "haskarium" (800, 600) (0, 0)
    refreshRate = 60
    startWorld =
        [ Ant{position = (0, 0), direction = (-pi) / 4, turnRate = pi / 8}
        , Ant { position = (-30, -30)
              , direction = 3 * pi / 4
              , turnRate = (-pi) / 3
              }
        , Ant{position = (20, 40), direction = pi / 3, turnRate = pi / 4}
        ]

type Angle = Float
type RadiansPerSecond = Float

data Ant = Ant
    {position :: !Point, direction :: !Angle, turnRate :: !RadiansPerSecond}

type World = [Ant]

drawAnt :: Ant -> Picture
drawAnt Ant{position = (x, y)} =
    translate x y $ color red $ rectangleSolid 10 10

draw :: World -> Picture
draw world = pictures $ map drawAnt ants
  where
    ants = world

onEvent :: Event -> World -> World
onEvent _ world = world

onTick :: Float -> World -> World
onTick dt world = map move' ants
  where
    ants = world
    move' ant = move dt ant

move :: Float -> Ant -> Ant
move dt ant = ant{position = (x + dx, y + dy), direction = dir + tr * dt}
  where
    Ant{position = (x, y), direction = dir, turnRate = tr} = ant
    dx = speed * dt * cos dir
    dy = speed * dt * sin dir
    speed = 20
