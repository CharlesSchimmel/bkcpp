{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import           KodiRPC.Types.Base

import Prelude as P
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe
import           Data.Text
import           Debug.Trace
import           GHC.Generics
import           Lens.Micro.Platform

type Name = ()
type KApp = ReaderT KState IO

-- new app type:
data Stuff = Stuff 
  {
  } deriving ( Show )

type KResult a = Either RpcException a

data ViewState = Playlist | YouTubeCast
  deriving ( Show ) 

-- AppState
data KState = KState
  { _k          :: KodiInstance
  , _window     :: String
  , _player     :: Maybe Player
  , _volume     :: Volume
  , _viewState  :: ViewState
  }
  deriving (Show)

data PlayerState = Playing | Paused
  deriving (Show, Read)

data Media =
  Media
    { _title    :: String
    , _file     :: String
    , _duration :: Int
    , _details  :: MediaDetails
    } deriving (Show)

data MediaDetails =
  Audio
  { _album         :: String
  , _artist        :: [String]
  , _albumArtist   :: [String]
  , _displayArtist :: String
  }
  |
  Episode
  { _episode       :: Int
  , _showtitle     :: String
  , _season        :: Int
  }
  |
  Movie
  deriving (Show)

instance FromJSON MediaDetails where
  parseJSON = withObject "MediaDetails" $ \o -> do
    i <- o.:"item"
    mediaType <- i.:"type" :: Parser String
    let audio = Audio <$> i.:"album" <*> i.:"artist" <*> i.:"albumartist" <*> i.:"displayartist"
        episo = Episode <$> i.:"episode" <*> i.:"showtitle" <*> i.:"season"
    case mediaType of
      "song"    -> Audio <$> i.:"album" <*> i.:"artist" <*> i.:"albumartist" <*> i.:"displayartist"
      "episode" -> Episode <$> i.:"episode" <*> i.:"showtitle" <*> i.:"season"
      _         -> pure Movie

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
  parseJSON = withObject "Volume " $ \o -> do
    vol <- o.:"volume" :: Parser Double
    let vol' = floor vol
    Volume vol' <$> o.:"muted"

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
  show (Time 0 a b _) = show a ++ ":" ++ padInt b
  show (Time a b c _) = show a ++ ":" ++ padInt b ++ ":" ++ padInt c

data TimeProgress = TimeProgress
  { _elapsed :: Time
  , _total   :: Time
  } deriving (Eq, Ord, Show)

instance Monoid TimeProgress where
  mempty = TimeProgress mempty mempty
  mappend mempty b = b

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

newtype Config = Config
  { kInstance :: KodiInstance
  } deriving (Show)

data OneShot = OneShot { format :: Maybe String
                       } deriving (Show)
data Options = Options
  { config  :: Config
  , cast    :: Maybe Cast
  , oneShot :: Maybe OneShot
  } deriving (Show)

data Cast = Cast { toCast :: Castable
                 , queue  :: Bool
                 } deriving ( Show )

data Castable = Youtube String | CastFile String
  deriving ( Show )

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
