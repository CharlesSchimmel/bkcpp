{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import UI
import Handlers
import Types
import Options
import KodiRPC.Methods.Playlist as Playlist (add)
import KodiRPC.Methods.Player as Player (asYTPluginPath, matchYouTubeId, openPath)
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
import           Data.Maybe
import           Data.Foldable
import           Data.Text as T


main :: IO ()
main = conf >>= main'
  where conf = execParser $ info (argParse <**> helper) ( fullDesc <> header "Brick Kodi Client Plus Plus" <> progDesc "A TUI client for the Kodi Media Center.")

main' :: Options -> IO ()
main' opts = ping (kInstance . config $ opts) >>= maybe notGood (const . switchboard $ opts)

switchboard :: Options -> IO ()
switchboard opts = do
    traverse_ kaller $ cast opts >>= caster
    allGood ki
    where ki = kInstance . config $ opts
          kaller = kall ki

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

caster :: Cast -> Maybe Method
caster cast = method
    where castMethod = if queue cast then castAdd else castPlay
          file = castFormat (toCast cast)
          castAdd = Playlist.add 0
          castPlay = Player.openPath
          method = castMethod <$> file

castFormat :: Castable -> Maybe Text
castFormat (Youtube y)  = (fmap asYTPluginPath) . matchYouTubeId . T.pack $ y
castFormat (CastFile f) = Just . T.pack $ f
