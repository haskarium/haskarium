import           Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main =
    play display white refreshRate startWorld draw onEvent onTick
  where
    display = InWindow "haskarium" (800, 600) (0, 0)
    refreshRate = 60
    startWorld =
        [ Creature
              { position = (0, 0)
              , direction = - pi / 4
              , turnRate = pi / 8
              , species = Ant
              }
        , Creature
              { position = (-30, -30)
              , direction = 3 * pi / 4
              , turnRate = - pi / 3
              , species = Fly
              }
        , Creature
              { position = (20, 40)
              , direction = pi / 3
              , turnRate = pi / 4
              , species = Ant
              }
        ]

type Angle = Float
type RadiansPerSecond = Float

data Creature = Creature
    { position  :: !Point
    , direction :: !Angle
    , turnRate  :: !RadiansPerSecond
    , species   :: !Species
    }

data Species = Ant | Fly

type World = [Creature]

radiansToDegrees :: Float -> Float
radiansToDegrees rAngle = rAngle * 180 / pi

drawCreature :: Creature -> Picture
drawCreature Creature{position = (x, y), direction = z, species = s} =
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
    {position = (x + dx, y + dy), direction = dir + tr * dt}
  where
    Creature{position = (x, y), direction = dir, turnRate = tr, species = s} =
        creature
    dx = speed * dt * cos dir
    dy = speed * dt * sin dir
    speed = case s of
        Ant -> 20
        Fly -> 100
