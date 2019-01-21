{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import           Debug.Trace
import           Lens.Micro.Platform
import           Data.Aeson
import           Data.Maybe
import           KodiRPC.Types.Base
import           GHC.Generics

type Name = ()

type Kall = Method -> IO (Either RpcException Value)

-- type KAction = Action -> IO (Either RpcException Value)
-- State container 

data KState = KState
  { _k          :: KodiInstance
  , _window     :: String
  , _player     :: Maybe Player
  , _volume     :: Volume
  }
  deriving (Show)

data PlayerState = Playing | Paused
  deriving (Show, Read)

data Media =
  Media
    { _title    :: String
    , _file     :: String
    , _details  :: MediaDetails
    , _duration :: Int
    } deriving (Show)

data MediaDetails =
  Audio
  { _album         :: String
  , _artist        :: [String]
  , _albumArtist   :: [String]
  , _displayArtist :: String
  }
  |
  Movie
  |
  Episode
  { _episode       :: Int
  , _showtitle     :: String
  , _season        :: Int
  }
  deriving (Show)

data Player = Player
  { _speed         :: Float
  , _playerId      :: Int
  , _timeElapsed   :: Time
  , _timeRemaining :: Time
  , _media         :: Media
  }
  deriving (Show)

data Volume = Volume
  { _volPercent :: Int
  , _muted :: Bool
  }

instance Show Volume where
  show (Volume p m)
    | m = "--"
    | p < 100 = " " ++ padInt p
    | otherwise = padInt p

instance FromJSON Volume where
  parseJSON = withObject "Volume " $ \o -> Volume
    <$> o.:"volume"
    <*> o.:"muted"

clearPlayer (Player _ _ _ _ e) = Player 0 0 mempty mempty e
tdb_ x = trace (show x) x

data Time = Time
  { _hours         :: Hours
  , _minutes       :: Minutes
  , _seconds       :: Seconds
  , _milliseconds  :: Milliseconds
  }
  deriving (Eq, Ord)

instance FromJSON Time where
  parseJSON = withObject "Time" $ \o -> Time
    <$> o.:"hours"
    <*> o.:"minutes"
    <*> o.:"seconds"
    <*> o.:"milliseconds"

instance Monoid Time where
  mempty = Time 0 0 0 0
  mappend _ b = b

padInt x = if x < 10 then "0" ++ show x else show x

instance Show Time where
  show (Time 0 a b _) = padInt a ++ ":" ++ padInt b
  show (Time a b c _) = padInt a ++ ":" ++ padInt b ++ ":" ++ padInt c

data TimeProgress = TimeProgress
  { _elapsed :: Time
  , _total   :: Time
  } deriving (Eq, Ord, Show)

instance Monoid TimeProgress where
  mempty = TimeProgress mempty mempty
  mappend mempty b = b
  mappend b mempty = b
  mappend (TimeProgress a b) (TimeProgress c d)
    | b == d = TimeProgress c d
    | otherwise = TimeProgress c d

instance FromJSON TimeProgress where
  parseJSON = withObject "TimeProgress" $ \o -> TimeProgress
    <$> o.:"time"
    <*> o.:"totaltime"

type Hours        = Int
type Minutes      = Int
type Seconds      = Int
type Milliseconds = Int

data Tick = Tick
  deriving ( Show )

type BChanEvent = Either Tick (Maybe Notif)

addSec :: Time -> Time
addSec (Time h n s m) = Time h' n'' s'' m
  where s' = s + 1
        n' = if s' > 59 then n + 1 else n
        h' = if n' > 59 then h + 1 else h
        s'' = if s' > 59 then 0 else s'
        n'' = if n' > 59 then 0 else n'

makeLenses ''Media
makeLenses ''MediaDetails
makeLenses ''Player
makeLenses ''KState

test :: KodiInstance
test = KodiInstance "localhost" 8080 "" ""

