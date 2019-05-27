{-# LANGUAGE OverloadedStrings #-}

module Options where

import Types
import KodiRPC.Types.Base

import Options.Applicative
import Data.Semigroup ((<>))

-- argParse :: Parser Config
argParse = Options
  <$> configParse
  <*> parseCast
  <*> parseOneShot

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

parseQueue = switch 
  (  short 'q' 
  <> long "queue"
  <> help "Add casted media to current playlist"
  )

parseOneShot = switch
  (  short '1'
  <> long "one-shot"
  <> help "Perform a command and exit; do not open interface"
  <> showDefault
  )


parseCastMedia = parseCastYt <|> parseCastFile

parseCast = optional $ Cast <$> parseCastMedia <*> parseQueue

-- bkcpp  -yt urlhere -q
