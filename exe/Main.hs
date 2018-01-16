{-# LANGUAGE RecordWildCards #-}

import           Control.Monad.State.Strict (runState)
import           Graphics.Gloss (Display (InWindow), play, white)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import           Options.Applicative
import           System.Random (StdGen, newStdGen)

import           Haskarium.Draw (draw)
import           Haskarium.Generate (makeGame)
import           Haskarium.Motion (onEvent, onTick)
import           Haskarium.Types (Rnd)

main :: IO ()
main = do
    SimOptions{..} <- execParser $ info (opts <**> helper) mempty
    let window =
            ( (- fromIntegral optWidth / 2, - fromIntegral optHeight / 2)
            , (  fromIntegral optWidth / 2,   fromIntegral optHeight / 2)
            )
        display = InWindow "haskarium" (optWidth, optHeight) (0, 0)

    randomSeed <- newStdGen
    let startWorld = runState (makeGame window) (fromMaybe randomSeed optSeed)

    play
        display
        white
        optRefresh
        startWorld
        (draw . fst)
        (runStateEndo onEvent)
        (runStateEndo onTick)

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
    <*> optional (option auto (short 's' <> long "seed"))

runStateEndo :: (a -> b -> Rnd b) -> a -> (b, StdGen) -> (b, StdGen)
runStateEndo f a (b, s) = runState (f a b) s
