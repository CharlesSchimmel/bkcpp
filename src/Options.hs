{-# LANGUAGE OverloadedStrings #-}

module Options where

import Types
import KodiRPC.Types.Base

import Options.Applicative
import Data.Semigroup ((<>))

-- argParse :: Parser Config
argParse = Options <$> configParse
  <*> (optional $ strOption
   ( long "youtube"
   <> short 'y'
   <> metavar "yt"
   <> help "YouTube url to cast"
   ))

configParse = Config <$> kodiInstanceParse

kodiInstanceParse = KodiInstance
  <$> strOption
    (  long "ip-address"
    <> short 'a'
    <> metavar "ip"
    <> help "IP address of running Kodi Media Center"
    )
  <*> option auto
    (  long "port"
    <> short 'p'
    <> showDefault
    <> metavar "port"
    <> value 8080
    <> help "Port"
    )
  <*> option auto
    (  long "username"
    <> value ""
    <> metavar "user"
    <> help "Username"
    )
  <*> option auto
    (  long "password"
    <> value ""
    <> metavar "pass"
    <> help "Password"
    )
