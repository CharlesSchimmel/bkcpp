{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module FormatParser where

import Prelude as P
import Text.Megaparsec as M
import Text.Megaparsec.Char
import System.Environment
import Control.Applicative           ((<|>), many)
import Control.Monad                 (void, join)
import Data.Char                     (isLetter, isDigit)
import Data.Maybe
import Data.Void
import Data.Text
import Debug.Trace
import Safe
import           Lens.Micro.Platform -- ((%~), (&), (^.), (.~), set, over, view)

import Types

type Parser = Parsec Void Text

separatorSub :: Parser FToken
separatorSub = do
  _ <- char '-'
  wrapped <- M.manyTill asciiChar (char '%')
  let withDefault = whenEmpty wrapped
  return $ Separator withDefault
    where whenEmpty x = if P.null x then " - " else x

pctSub :: Parser FToken
pctSub = do
  wrapped <- M.manyTill letterChar (char '%')
  let matchWrapped = readFTokens wrapped
  return matchWrapped

pctParser :: Parser FToken
pctParser = do
  _ <- char '%'
  separatorSub <|> pctSub

-- Transport Status: (play/pause/stopped)
type FToken = FormatTokens String
data FormatTokens a = Plain a
                    | Separator a
                    | Title
                    | Subtitle
                    | TimeRemaining
                    | TimeElapsed
                    | Vol
                    deriving (Show, Read)

readFTokens :: String -> FToken
readFTokens "title"    = Title
readFTokens "subtitle" = Subtitle
readFTokens "timer"    = TimeRemaining
readFTokens "timee"    = TimeElapsed
readFTokens "vol"      = Vol
readFTokens ""         = Plain "%" -- %%
readFTokens a          = Plain a

plainFToken :: Parser FToken
plainFToken = (Plain . pure) <$> asciiChar

formatLine :: Parser [FToken]
formatLine = reducePlainTokens <$> (M.many $ pctParser <|> plainFToken)

reducePlainTokens :: [FToken] -> [FToken]
reducePlainTokens = P.foldr rdxr []
    where rdxr (Plain current) (Plain hed:ts) = Plain (current ++ hed):ts
          rdxr x y = x:y

generateLine :: [FToken] -> KState -> String
generateLine tokens ks = join fnLst
  where tokFns = P.map lensForToken tokens
        fnLst = P.map (\fn -> fn ks) tokFns

lensForToken :: FToken -> KState -> String
lensForToken (Plain a)     = const a
lensForToken Vol           = (\v -> if _muted v then "MUT" else show $ _volPercent v) <$> view volume
lensForToken TimeRemaining = nop show                                                 <$> preview (player._Just.timeRemaining)
lensForToken TimeElapsed   = nop show                                                 <$> preview (player._Just.timeElapsed)
lensForToken Title         = fromMaybe ""                                             <$> preview (player._Just.media.title)
lensForToken Subtitle      = nop mediaSubtitle                                        <$> preview (player._Just.media.details)
lensForToken (Separator a) = nop (mediaSeparator a)                                   <$> preview (player._Just.media.details)

nop = maybe ""

mediaSubtitle :: MediaDetails -> String
mediaSubtitle Audio{..}   = fromMaybe "" $ headMay _artist
mediaSubtitle Episode{..} = _showtitle
mediaSubtitle _           = ""

mediaSeparator :: String -> MediaDetails -> String
mediaSeparator sep details
  | P.null . mediaSubtitle $ details = ""
  | otherwise = sep

defaultFormatStr :: Text
defaultFormatStr = "%title%%-%%subtitle% [%timee%/%timer%]"
defaultFormat = fromMaybe nothingFormat $ parseMaybe formatLine defaultFormatStr

nothingFormat = [ Title
                , Separator " - "
                , Subtitle
                , Plain "\n"
                , Plain "volume: "
                , TimeElapsed
                , Plain "/"
                , TimeRemaining
                ]

