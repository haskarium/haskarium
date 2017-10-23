{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

import          Graphics.Gloss.Interface.Pure.Game
import          System.Random

main :: IO ()
main = do
    g <- newStdGen
    let startWorld = World{randomGen = g, creatures = cs}
    play display white refreshRate startWorld draw onEvent onTick
  where
    cs =
        [ Creature
            { position = (0, 0)
             , targetDir = pi / 4
             , currentDir = pi / 4
             , turnRate = pi / 4
             , species = Ant
             }
        , Creature
             { position = (-30, -30)
             , targetDir = 3 * pi / 4
             , currentDir = 3 * pi / 4
             , turnRate = pi / 2
             , species = Fly
             }
        , Creature
             { position = (20, 40)
             , targetDir = 3 * pi / 2
             , currentDir = 3 * pi / 2
             , turnRate = pi / 4
             , species = Flea{idleTime = 0}
             }
        ]
    display = InWindow "haskarium" (800, 600) (0, 0)
    refreshRate = 60

type Angle = Float
type RadiansPerSecond = Float

data Creature = Creature
    { position   :: !Point
    , targetDir  :: !Angle
    , currentDir :: !Angle
    , turnRate   :: !RadiansPerSecond
    , species    :: !Species
    }

data Species = Ant | Flea{idleTime :: !Float} | Fly

data World = World
    { randomGen :: !StdGen
    , creatures :: ![Creature]
    }

radiansToDegrees :: Float -> Float
radiansToDegrees rAngle = rAngle * 180 / pi

drawCreature :: Creature -> Picture
drawCreature Creature{position = (x, y), currentDir, species} =
    translate x y $
    rotate (- radiansToDegrees currentDir) $
    figure species

figure :: Species -> Picture
figure = \case
    Ant{} ->
        color red $
        pictures
            [ triangleBody
            , translate (-5) 0 $ circle 5
            ]
    Flea{} ->
        color blue $
        pictures
            [ triangleBody
            , translate (-5) 0 $ circle 5
            ]
    Fly{} ->
        color green $
        pictures
            [ triangleBody
            , translate 5   5  $ circle 5
            , translate 5 (-5) $ circle 5
            ]
  where
    triangleBody = polygon
        [ ( 5,  0)
        , (-5, -5)
        , (-5,  5)
        ]

draw :: World -> Picture
draw World{creatures} = pictures $ map drawCreature creatures

onEvent :: Event -> World -> World
onEvent _ world = world

onTick :: Float -> World -> World
onTick dt w@World{creatures} = updateCreatures dt w{creatures = []} $ reverse creatures

updateCreatures :: Float -> World -> [Creature] -> World
updateCreatures _ world [] = world
updateCreatures dt world (c:cs) = updateCreatures dt (updateCreature dt c world) cs

updateCreature :: Float -> Creature -> World -> World
updateCreature dt c@Creature{species} World{randomGen = g, creatures = cs} =
    World{randomGen = g', creatures = c' : cs}
  where
    (g', c') = turn g dt $ move dt c
    move = case species of
        Ant            -> run 30
        Fly            -> run 200
        Flea{idleTime} -> jump idleTime 100

turn :: StdGen -> Float -> Creature -> (StdGen, Creature)
turn g dt creature = (g', creature{targetDir = newTdir, currentDir = newCdir})
  where
    Creature { targetDir = tdir
             , currentDir = cdir
             , turnRate = tr
             } = creature
    (newTdir, g') = if (cdir <= tdir && cdir' >= tdir) || (cdir >= tdir && cdir' <= tdir)
                    then randomR (0, 2 * pi) g
                    else (tdir, g)
    newCdir
      | cdir' < 0       = cdir' + 2 * pi
      | cdir' > 2 * pi  = cdir' - 2 * pi
      | otherwise       = cdir'
    cdir' = cdir + trSign * tr * dt
    trSign = if (delta > -pi && delta < 0) || delta > pi then -1 else 1
    delta = tdir - cdir

run :: Float -> Float -> Creature -> Creature
run speed dt c@Creature{position = (x, y), currentDir = dir} = 
    c{position = (x + dx, y + dy)}
  where
    dx = speed * dt * cos dir
    dy = speed * dt * sin dir

jump :: Float -> Float -> Float -> Creature -> Creature
jump idleTime distance dt c@Creature{position = (x, y), currentDir = dir} =
    if idleTime < fleaMaxIdleTime then
        c{species = Flea{idleTime = idleTime + dt}}
    else
      let
        dx = distance * cos dir
        dy = distance * sin dir
      in
        c { position = (x + dx, y + dy)
          , species = Flea{idleTime = idleTime + dt - fleaMaxIdleTime}}

fleaMaxIdleTime :: Float
fleaMaxIdleTime = 2

