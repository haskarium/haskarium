{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

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
              , species = Flea{idleTime = 0}
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

data Species = Ant | Flea{idleTime :: !Float} | Fly

type World = [Creature]

radiansToDegrees :: Float -> Float
radiansToDegrees rAngle = rAngle * 180 / pi

drawCreature :: Creature -> Picture
drawCreature Creature{position = (x, y), direction, species} =
    translate x y $
    rotate (- radiansToDegrees direction) $
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
draw creatures = pictures $ map drawCreature creatures

onEvent :: Event -> World -> World
onEvent _ world = world

onTick :: Float -> World -> World
onTick dt creatures = map update' creatures
  where
    update' creature = updateCreature dt creature

updateCreature :: Float -> Creature -> Creature
updateCreature dt creature = case species of
    Ant            -> run 20
    Fly            -> run 200
    Flea{idleTime} -> jump idleTime 100
  where
    Creature{position = (x, y), direction, turnRate, species} =
        creature
    run speed = creatureMovedTurned dx dy
      where
        dx = speed * dt * cos direction
        dy = speed * dt * sin direction
    creatureMovedTurned dx dy = creature
        { position = (x + dx, y + dy)
        , direction = direction + turnRate * dt
        }
    jump idleTime distance =
        if idleTime < fleaMaxIdleTime then
            (creatureMovedTurned 0 0){species = Flea{idleTime = idleTime + dt}}
        else let
            dx = distance * cos direction
            dy = distance * sin direction
            in
            (creatureMovedTurned dx dy)
                {species = Flea{idleTime = idleTime + dt - fleaMaxIdleTime}}

fleaMaxIdleTime :: Float
fleaMaxIdleTime = 2
