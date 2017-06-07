import           Data.Time
import           Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = do
    zt <- getZonedTime
    let time =
            realToFrac $
            timeOfDayToTime $ localTimeOfDay $ zonedTimeToLocalTime zt
    play display white refreshRate time draw onEvent onTick
  where
    display = InWindow "haskarium" (600, 1000) (0, 0)
    refreshRate = 60

type World = Float -- seconds from Midnight

draw :: World -> Picture
draw time = pictures [clock, pendulum]
  where
    clock = translate 0 250 $ pictures [numbers, hands]
    seconds = fromIntegral (round time :: Int)
    minutes = seconds / 60
    hours = minutes / 60
    hands = pictures
        [ rotate (6  * seconds) $ line [(0, 0), (0, 200)]
        , rotate (6  * minutes) $ line [(0, 0), (0, 150)]
        , rotate (30 * hours  ) $ line [(0, 0), (0, 100)]
        ]
    numbers = pictures
        [ rotate a $
          translate dx 200 $ scale 0.2 0.2 $ text $ show n
        | n <- [1 .. 12 :: Int]
        , let a = fromIntegral (30 * n)
        , let dx = if n < 10 then -7.5 else -15
        ]
    pendulum = rotate alpha $ pictures
        [ line [(0, 0), (0, -lineLength)]
        , translate 0 (-(lineLength + circleRadius)) $ circleSolid circleRadius
        ]
    alpha = 57.3 * alpha0 * cos (omega * time)
    alpha0 = pi / 18 -- 10 deg
    omega = sqrt (g / length)
    -- need to normalize somehow, let 1 pixel = 1 mm
    length = (lineLength + circleRadius * 2) / 1000
    lineLength = 400
    circleRadius = 20
    g = 9.8

onEvent :: Event -> World -> World
onEvent _ world = world

onTick :: Float -> World -> World
onTick dt time = time + dt
