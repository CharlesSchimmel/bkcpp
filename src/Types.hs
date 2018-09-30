{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Lens.Micro.Platform
import           Data.Aeson
import           KodiRPC.Types

type Name = ()

-- State container 
data KState = KState
  { _window     :: String
  , _player     :: Player
  , _title      :: String
  , _nowPlaying :: Maybe NowPlaying
  , _lastRecvd  :: Value
  }
  deriving (Show)

data PlayerState = Playing | Paused
  deriving (Show, Read)

data NowPlaying =
  Song
  { _songTitle   :: String
  , _artist      :: [String]
  , _album       :: Maybe String
  , _albumArtist :: Maybe String
  }
  | Video
  { _videoTitle  :: String
  }
  deriving (Show)

data Player = Player
  { _speed         :: Float
  , _playerId      :: Float
  , _timeElapsed   :: Time
  , _timeRemaining :: Time
  , _volume        :: Volume
  }
  deriving (Show)

data Volume = Volume
  { _volPercent :: Int
    , _muted :: Bool
  }

instance Show Volume where
  show (Volume p m) = if m then "--" else if p < 100 then " " ++ padInt p else padInt p

clearPlayer (Player _ _ _ _ e) = Player 0 0 mempty mempty e

data Time = Time
  { _hours         :: Hours
  , _minutes       :: Minutes
  , _seconds       :: Seconds
  , _milliseconds  :: Milliseconds
  }
  deriving (Eq, Ord)

instance Monoid Time where
  mempty = Time 0 0 0 0
  mappend _ b = b

padInt x = if x < 10 then "0" ++ show x else show x

instance Show Time where
  show (Time 0 a b _) = padInt a ++ ":" ++ padInt b
  show (Time a b c _) = padInt a ++ ":" ++ padInt b ++ ":" ++ padInt c

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



makeLenses ''NowPlaying
makeLenses ''Player
makeLenses ''KState

type UI = KState

isPlaying k = k^.player^.speed > 0

isPlayingStr plyr = if isPlaying plyr then "playing" else "paused"
