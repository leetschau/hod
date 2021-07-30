{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Config
    ( loadConfig
    , saveConfig
    , AppConfig (..)
    , UserConfig (..)
    ) where

import Data.Aeson
import Data.List
import Data.Time
import GHC.Generics
import System.Directory
import System.FilePath.Posix
import qualified Data.ByteString.Lazy as B

data UserConfig = UserConfig
    { appHome :: FilePath
    , defaultNotebook :: String
    , editor :: String
    , viewer :: String
    , defaultListLength :: Int
    , browser :: String
    } deriving Generic

instance Show UserConfig where
    show conf = unlines [ "appHome: " ++ appHome conf
                        , "defaultNotebook: " ++ defaultNotebook conf
                        , "editor: " ++ editor conf
                        , "viewer: " ++ viewer conf
                        , "defaultListLength: " ++ (show $ defaultListLength conf)
                        , "browser: " ++ browser conf ]

instance FromJSON UserConfig
instance ToJSON UserConfig

data AppConfig = AppConfig
    { userConf :: UserConfig
    , noteRepo :: FilePath
    , recordFile :: FilePath
    , tempNote :: FilePath
    , previewFile :: FilePath }


-- |Get normal path from shorthand format 'abbrPath'
-- Convert ~/target/folder to /home/<username>/target/folder
normalPath :: FilePath -> IO FilePath
normalPath abbrPath = do
    homePath <- getHomeDirectory
    let fullPath =
            case abbrPath of
                '~' : '/' : tailPath -> joinPath [homePath, tailPath]
                otherwise -> abbrPath
    return fullPath


confPath =  "~/.config/hod/config.json"

defaultUserConf = UserConfig
    { appHome =  "~/.donno"
    , defaultNotebook = "/Diary/2021"
    , editor = "nvim"
    , viewer = "nvim -R"
    , defaultListLength = 5
    , browser = "firefox" }


readConfig :: IO B.ByteString
readConfig = do
    configPath <- normalPath confPath
    confExists <- doesFileExist configPath
    case confExists of
        True -> B.readFile configPath
        False -> do
                     createDirectoryIfMissing True $ takeDirectory configPath
                     defAppHome <- normalPath $ appHome defaultUserConf
                     let defUserConfStr =
                             encode $ defaultUserConf { appHome = defAppHome }
                     B.writeFile configPath defUserConfStr
                     return defUserConfStr

parseConfig :: B.ByteString -> Maybe AppConfig
parseConfig configStr = do
    case decode configStr :: Maybe UserConfig of
        (Just uconf) ->
            Just AppConfig
                { userConf = uconf
                , noteRepo = joinPath [appHome uconf, "repo"]
                , recordFile = joinPath [appHome uconf, "reclist"]
                , tempNote = "/tmp/newnote.md"
                , previewFile =  "/tmp/preview.html" }
        _ -> Nothing


loadConfig :: IO AppConfig
loadConfig = do
    confStr <- readConfig
    case parseConfig confStr of
        Just appConf -> return appConf
        Nothing -> fail "Parse config file failed. Fix it and try again"


-- | Save 'config' to JSON file
saveConfig :: UserConfig -> IO ()
saveConfig newConf = do
    newAppHome <- normalPath $ appHome newConf
    let conf = encode $ newConf { appHome = newAppHome }
    configPath <- normalPath confPath
    B.writeFile configPath conf