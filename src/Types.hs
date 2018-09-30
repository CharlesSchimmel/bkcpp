{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Lens.Micro.Platform
import           Data.Aeson

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
  , _volume        :: Int
  }
  deriving (Show)


type Time = [Int]

makeLenses ''NowPlaying
makeLenses ''Player
makeLenses ''KState

type UI = KState

isPlaying k = k^.player^.speed > 0

isPlayingStr plyr = if isPlaying plyr then "playing" else "paused"
