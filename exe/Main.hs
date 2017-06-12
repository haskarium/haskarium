-- import  Prelude                             hiding (($))
import  Data.Time
import  Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = do
    zt <- getZonedTime
    let time = realToFrac $ timeOfDayToTime $ localTimeOfDay $ zonedTimeToLocalTime zt
    play display green refreshRate time draw onEvent onTick
  where
    display = InWindow "haskarium" (800, 800) (0, 0)
    refreshRate = 60

type World = Float

draw :: World -> Picture
draw time = translate 0 100 $ pictures [numbers, hands, body, pendulum]
  where
    body = pictures
        [ circle 230
        , rectangleWire 480 480
        ]
    pendulum = rotate (10 * cos (time * pi) ) $
        translate 0 (-240) $
        pictures
        [ line [(0, 0), (0, -180)]
        , translate 0 (-200) $ circle 20
        ]
    seconds = fromIntegral (round time :: Int)
    minutes = seconds / 60
    hours = minutes / 60
    hands = pictures
        [ rotate (6 * seconds) $ line [(0, 0), (0, 200)]
        , rotate (6 * minutes) $ line [(0, 0), (0, 150)]
        , rotate (30 * hours) $ line [(0, 0), (0, 100)]
        ]
    numbers = pictures
        [ rotate a $
          translate dx 200 $ scale 0.2 0.2 $ text $ show n
        | n <- [1 .. 12 :: Int]
        , let a = fromIntegral (30 * n)
        , let dx = if n < 10 then -7.5 else -15
        ]

onEvent :: Event -> World -> World
onEvent _ world = world

onTick :: Float -> World -> World
onTick dt time = time + dt

-- ($) f x
-- ($) :: (a -> b) -> a -> b
-- f $ x = f x
-- infixr 0 $
