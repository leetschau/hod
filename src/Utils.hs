{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Utils
    ( deQuote
    , deQuoteT
    , roundLocalTime
    ) where

import qualified Data.Text as T
import Data.Time

deQuote :: String -> String
deQuote ('"': body) | last body == '"' = init body
deQuote ('\'': body) | last body == '\'' = init body
deQuote str = str

deQuoteT :: T.Text -> T.Text
deQuoteT str = T.pack $ deQuote $ T.unpack str

-- |Remove milliseconds (decimal part of the Second) from a LocalTime value
roundLocalTime :: LocalTime -> LocalTime
roundLocalTime date =
    LocalTime { localDay = curDay
              , localTimeOfDay = curTime { todSec = curSec } }
      where
        curDay = localDay date
        curTime = localTimeOfDay date
        curSec = (fromIntegral . round . todSec) curTime
