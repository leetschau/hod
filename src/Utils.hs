{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Utils
    ( deQuote
    , deQuoteT
    , roundLocalTime
    , showTextList
    , getFilesWithExt
    ) where

import Data.List (isSuffixOf)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..))
import System.Directory (listDirectory)
import System.FilePath.Posix (joinPath)
import qualified Data.Text as T

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

-- | Convert a Text list into a hunman-friendly format
-- ["你好", "世界"] => "[你好; 世界]"
showTextList :: [T.Text] -> T.Text
showTextList words = mconcat [ "["
                             , T.intercalate "; " words
                             , "]"]

-- | Get files with extension 'ext' in 'dir'
-- Example: getFilesWithExt "/tmp" ".tgz"
getFilesWithExt :: FilePath -> String -> IO [FilePath]
getFilesWithExt dir ext = do
    allFiles <- listDirectory dir
    let targets = filter (isSuffixOf $ ext) allFiles
    return $ map (\file -> joinPath [dir, file]) targets
