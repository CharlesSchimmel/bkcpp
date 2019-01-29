module Main where

import Lib
import UI
import Handlers
import Types
import Options
import KodiRPC.Types.Base
import KodiRPC.Calls

import           Brick
import           Brick.AttrMap
import           Brick.BChan
import           Brick.Types           as BT
import           Prelude as P
import qualified Graphics.Vty          as V
import           Control.Concurrent
import           Control.Monad
import           Options.Applicative
import           Data.Semigroup ((<>))


main :: IO ()
main = conf >>= main'
  where conf = execParser $ info (argParse <**> helper) ( fullDesc <> progDesc "Test" <> header "Blah")

main' :: Config -> IO ()
main' conf = ping (kInstance conf) >>= maybe notGood allGood

allGood :: KodiInstance -> IO ()
allGood ki = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan $ P.Left Tick
    threadDelay 1000000
  forkIO $ forever $ P.Right <$> notification ki >>= writeBChan chan
  initK <- initKState ki
  void $ customMain (V.mkVty V.defaultConfig) (Just chan) app initK

notGood :: IO ()
notGood = void $ putStrLn couldNotConnect
  where couldNotConnect = "Could not connect. Settings correct? Kodi running? Network control enabled?"

justListenToNotifs :: IO ()
justListenToNotifs = forever $ print =<< notification test

justListenToNotifs' :: KodiInstance -> IO ()
justListenToNotifs' ki = forever $ print =<< notification ki
