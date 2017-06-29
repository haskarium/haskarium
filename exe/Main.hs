import          Graphics.Gloss.Interface.Pure.Game
import          System.Random

import          Debug.Trace

main :: IO ()
main = do
    g1 <- newStdGen
    g2 <- newStdGen
    g3 <- newStdGen
    let startWorld = [ Creature
                           { position = (0, 0)
                           , targetDir = pi / 4
                           , currentDir = pi / 4
                           , turnRate = pi / 4
                           , randomGen = g1
                           , species = Ant
                           }
                     , Creature
                           { position = (-30, -30)
                           , targetDir = 3 * pi / 4
                           , currentDir = 3 * pi / 4
                           , turnRate = pi / 3
                           , randomGen = g2
                           , species = Fly
                           }
                     , Creature
                           { position = (20, 40)
                           , targetDir = 3 * pi / 2
                           , currentDir = 3 * pi / 2
                           , turnRate = pi / 4
                           , randomGen = g3
                           , species = Ant
                           }
                     ]
    play display white refreshRate startWorld draw onEvent onTick
  where
    display = InWindow "haskarium" (800, 600) (0, 0)
    refreshRate = 60

type Angle = Float
type RadiansPerSecond = Float

data Creature = Creature
    { position   :: !Point
    , targetDir  :: !Angle
    , currentDir :: !Angle
    , turnRate   :: !RadiansPerSecond
    , randomGen  :: !StdGen
    , species    :: !Species
    }

data Species = Ant | Fly

type World = [Creature]

radiansToDegrees :: Float -> Float
radiansToDegrees rAngle = rAngle * 180 / pi

drawCreature :: Creature -> Picture
drawCreature Creature{position = (x, y), currentDir = z, species = s} =
    translate x y $ rotate (- radiansToDegrees z) figure
  where
    figure = case s of
        Ant -> antFigure
        Fly -> flyFigure
    antFigure =
        color red $
        pictures
            [ polygon
                [ ( 5,  0)
                , (-5, -5)
                , (-5,  5)
                ]
            , translate (-5) 0 $ circle 5
            ]
    flyFigure =
        color green $
        pictures
            [ polygon
                [ ( 5,  0)
                , (-5, -5)
                , (-5,  5)
                ]
            , translate 5  5 $ circle 5
            , translate 5 (-5) $ circle 5
            ]

draw :: World -> Picture
draw creatures = pictures $ map drawCreature creatures

onEvent :: Event -> World -> World
onEvent _ world = world

onTick :: Float -> World -> World
onTick dt creatures = map move' creatures
  where
    move' creature = move dt creature

move :: Float -> Creature -> Creature
move dt creature = creature
    { position = (x + dx, y + dy)
    , targetDir = newTdir
    , currentDir = newCdir
    , randomGen = g'
    }
  where
    Creature { position = (x, y)
             , targetDir = tdir
             , currentDir = cdir
             , turnRate = tr
             , randomGen = g
             , species = s
             } = creature
    (newTdir, g') = trace dbg $ if (cdir <= tdir && cdir' >= tdir) || (cdir >= tdir && cdir' <= tdir)
                                then randomR (0, 2 * pi) g
                                else (tdir, g)
    newCdir
      | cdir' < 0       = cdir' + 2 * pi
      | cdir' > 2 * pi  = cdir' - 2 * pi
      | otherwise       = cdir'
    cdir' = cdir + trSign * tr * dt
    trSign = if (delta > -pi && delta < 0) || delta > pi then -1 else 1
        where delta = tdir - cdir
    dx = speed * dt * cos cdir
    dy = speed * dt * sin cdir
    speed = case s of
        Ant -> 30
        Fly -> 100
    dbg = unwords $ map show [tdir, cdir, abs (tdir - cdir'), trSign]
