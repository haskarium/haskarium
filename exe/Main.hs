{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

import           Graphics.Gloss.Interface.Pure.Game
import           System.Random                      (StdGen, newStdGen, randomR)
import           Graphics.Gloss.Geometry.Line       ( intersectSegHorzLine
                                                    , intersectSegVertLine)
import           Graphics.Gloss.Geometry.Angle      (radToDeg, normalizeAngle)

width :: Int
width = 800

height :: Int
height = 600

main :: IO ()
main = do
    g <- newStdGen
    let (_g', startWorld) =
            makeCreatures
                g
                (fromIntegral width / 2, fromIntegral height / 2)
                [Fly, Flea{idleTime = 0}, Ant, Centipede{segments=[]}]
    play display white refreshRate startWorld draw onEvent onTick
  where
    display = InWindow "haskarium" (width, height) (0, 0)
    refreshRate = 60

type Angle = Float
type RadiansPerSecond = Float

data Creature = Creature
    { position  :: !Point
    , direction :: !Angle
    , turnRate  :: !RadiansPerSecond
    , species   :: !Species
    , size      :: !Float
    }

data Species = Ant | Flea{idleTime :: !Float} | Fly | Centipede {segments :: [Point]}

type World = [Creature]

makeCreatures :: StdGen -> (Float, Float) -> [Species] -> (StdGen, [Creature])
makeCreatures g window species = makeCreatures' [] g species
  where
    (maxX, maxY) = window
    makeCreatures' creatures g0 [] = (g0, creatures)
    makeCreatures' creatures g0 (s : ss) = makeCreatures' (c : creatures) g5 ss
      where
        fake_size = 10  -- TODO: add real creature sizes
        c = Creature{ position = (x, y)
                    , direction = dir
                    , turnRate = tr
                    , species = s'
                    , size = fake_size}
        (x, g1) = randomR (-maxX + fake_size / 2, maxX - fake_size / 2) g0
        (y, g2) = randomR (-maxY + fake_size / 2, maxY - fake_size / 2) g1
        (dir, g3) = randomR (0, 2 * pi) g2
        (tr, g4) = case s of
            Centipede{} ->
                randomR (-pi / 34, -pi / 30) g3
            _ ->
                randomR (pi / 4, pi / 2) g3
        (s', g5) = case s of
            Centipede{} ->
                let (numSegments, gN) = randomR (5, 15) g4
                in (Centipede{segments = replicate numSegments (x, y)}, gN)
            Flea{} ->
                let (eagerness, gN) = randomR (0.0, 1.0) g4
                in (Flea{idleTime = eagerness}, gN)
            _ ->
                (s, g4)

drawCreature :: Creature -> Picture
drawCreature Creature{position, species = Centipede segments} =
    pictures $ map draw' (position : segments)
  where
    draw' (x, y) =
      translate x y $
      color orange $
      circleSolid centipedeSegmentRadius
drawCreature Creature{position = (x, y), direction, species} =
    translate x y $
    rotate (- radToDeg direction) $
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
    _ ->
        blank
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
updateCreature dt creature@Creature{turnRate, species} =
    creatureTurn updateCreature' ddir
  where
    updateCreature' = case species of
        Ant            -> run 20
        Fly            -> run 200
        Flea{idleTime} -> jump idleTime 100
        Centipede{}    -> updateCentipede
    ddir = turnRate * dt
    run speed = creatureMovedCheckCollisions creature dist
      where
        dist = dt * speed
    jump idleTime dist =
        if idleTime < fleaMaxIdleTime then
            creature{species = Flea{idleTime = idleTime + dt}}
        else
            (creatureMovedCheckCollisions creature dist)
                {species = Flea{idleTime = idleTime + dt - fleaMaxIdleTime}}
    updateCentipede = runHead { species = Centipede{segments=newSegments} }
      where
        Centipede{segments} = species

        runHead = run 6

        newSegments = runChain (position runHead) segments

        runChain _ [] = []
        runChain p@(px, py) (c@(cx, cy) : next) =
            let
            dist = distance p c
            c' =
                if dist < maxNeck then
                    c
                else let
                    dist' = maxNeck
                    squeezeFactor = dist / dist'
                    cx' = px - (px - cx) / squeezeFactor
                    cy' = py - (py - cy) / squeezeFactor
                    in (cx', cy')
            in c' : runChain c' next

        maxNeck = 1.5 * centipedeSegmentRadius

fleaMaxIdleTime :: Float
fleaMaxIdleTime = 2

distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) =
    sqrt $ (x1 - x2) ^ (2 :: Int) + (y1 - y2) ^ (2 :: Int)

centipedeSegmentRadius :: Float
centipedeSegmentRadius = 7

creatureTurn :: Creature -> Float -> Creature
creatureTurn creature@Creature{direction} ddir =
    creature{direction = direction + ddir}

creatureMovedCheckCollisions :: Creature -> Float -> Creature
creatureMovedCheckCollisions creature@Creature{position, species} dist
    | dist <= 0 = creature
    | otherwise =
        case maybeCollision of
            Nothing         -> creatureMoved creature dist
            Just collision' -> creatureMovedWithCollisions collision'
          where
            maybeCollision = checkCollisions creature dist
            creatureMovedWithCollisions (collision, new_dir) =
                case species of
                    Flea{} -> creatureMoved creature distToCol
                    _      -> creatureMovedCheckCollisions
                                  (creatureMoved creature distToCol)
                                      {direction = new_dir}
                                  (dist - distToCol)
              where
                distToCol = distance position collision

creatureMoved :: Creature -> Float -> Creature
creatureMoved creature@Creature{position = (x, y), direction} dist =
    creature{position = pointMoved (x, y) dist direction}

pointMoved :: Point -> Float -> Float-> Point
pointMoved (x, y) dist direction = (x + dx, y + dy)
  where
    dx = dist * cos direction
    dy = dist * sin direction

checkCollisions :: Creature -> Float -> Maybe (Point, Float)
checkCollisions Creature{position = p0, direction, size} dist =
    checkDirs pU pD pL pR
  where
    checkDirs (Just pu) _ _ _ | isMovedUp    = Just (pu, -direction)
    checkDirs _ (Just pd) _ _ | isMovedDown  = Just (pd, -direction)
    checkDirs _ _ (Just pl) _ | isMovedLeft  = Just (pl, pi - direction)
    checkDirs _ _ _ (Just pr) | isMovedRight = Just (pr, pi - direction)
    checkDirs _ _ _ _                        = Nothing
    normDir = normalizeAngle direction
    isMovedUp = normDir < pi
    isMovedDown = normDir > pi
    isMovedLeft = normDir > pi/2 && normDir < 3 * pi / 2
    isMovedRight = normDir < pi/2 || normDir > 3 * pi / 2
    p1 = pointMoved p0 dist direction
    pU = intersectSegHorzLine p0 p1 ((fromIntegral height / 2) - size / 2)
    pD = intersectSegHorzLine p0 p1 ((- fromIntegral height / 2) + size / 2)
    pL = intersectSegVertLine p0 p1 ((- fromIntegral width / 2) + size / 2)
    pR = intersectSegVertLine p0 p1 ((fromIntegral width / 2) - size / 2)
