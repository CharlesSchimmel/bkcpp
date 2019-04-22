{-# LANGUAGE OverloadedStrings #-}

module Options where

import Types
import KodiRPC.Types.Base

import Options.Applicative
import Data.Semigroup ((<>))

-- argParse :: Parser Config
argParse = Options <$> configParse
  <*> parseCast

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

parseCastYt = Youtube <$> strOption
   ( long "youtube"
   <> short 'y'
   <> metavar "url"
   <> help "YouTube url to cast"
   )

parseCastFile = CastFile <$> strOption
  (  long "cast-file"
  <> short 'f'
  <> metavar "file"
  <> help "File to cast"
  )

queueOption = switch 
  ( short 'q' 
  <> long "queue"
  <> help "Add casted media to current playlist"
  )

parseCastMedia = parseCastYt <|> parseCastFile

parseCast = optional $ Cast <$> parseCastMedia <*> queueOption

-- bkcpp  -yt urlhere -q
