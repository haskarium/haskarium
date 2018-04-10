{-# LANGUAGE RecordWildCards #-}

import           Control.Monad.RWS.Strict (runRWS)
import           Control.Monad.State.Strict (runState)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Graphics.Gloss (Display (InWindow), play, white)
import           Options.Applicative
import           System.Random (StdGen, newStdGen)

import           Haskarium.Draw (draw)
import           Haskarium.Generate (makeGame)
import           Haskarium.Motion (onEvent, onTick)
import           Haskarium.Types (Sim, World)

main :: IO ()
main = do
    SimOptions{..} <- execParser $ info (opts <**> helper) mempty
    let window =
            ( (- fromIntegral optWidth / 2, - fromIntegral optHeight / 2)
            , (  fromIntegral optWidth / 2,   fromIntegral optHeight / 2)
            )
        display = InWindow "haskarium" (optWidth, optHeight) (0, 0)

    randomSeed <- newStdGen
    let startGame =
            runState (makeGame window) (fromMaybe randomSeed optSeed)

    play
        display
        white
        optRefresh
        startGame
        (draw . fst)
        (runSim onEvent)
        (runSim onTick)

data SimOptions = SimOptions
    { optWidth   :: Int
    , optHeight  :: Int
    , optRefresh :: Int
    , optSeed    :: Maybe StdGen
    }

opts :: Parser SimOptions
opts = SimOptions
    <$> option auto (short 'w' <> long "width" <> value 800)
    <*> option auto (short 'h' <> long "height" <> value 600)
    <*> option auto (short 'f' <> long "fps" <> value 60)
    <*> optional (option auto $ short 's' <> long "seed")

runSim
    :: (event -> World -> Sim World)
    -> event
    -> (World, StdGen)
    -> (World, StdGen)
runSim sim event (world, rndState) =
    let (world', rndState', ()) =
            runRWS (sim event world) world rndState
    in (world', rndState')
