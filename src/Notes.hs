{-# LANGUAGE OverloadedStrings #-}
module Notes
    ( parse
    ) where

import Data.List
import Data.List.Split
import Data.Time
import Data.Time.Calendar
import Data.Time.Clock
import System.Directory
import System.FilePath.Posix
import Text.Pandoc.Shared
import qualified Data.Text as T
import qualified Data.Text.IO as TI


data Note =
    Note
        { title :: T.Text
        , tagList :: [T.Text]
        , notebook :: T.Text
        , created :: LocalTime
        , updated :: LocalTime
        , content :: T.Text
        , filePath :: String
        }

instance Show Note where
    show note = unwords [show $ updated note
                       , show $ notebook note
                       , show $ title note
                       , show $ created note
                       , show $ tagList note ]

noteRepo = "/home/leo/.donno/repo"

parse ["l", num] = listNotes num
parse ("s": words) = do
    notes <-simpleSearch words
    putStrLn $ unlines $ map (\x -> show x) notes


parseNote :: FilePath -> IO Note
parseNote notePath = do
    fileContent <- TI.readFile notePath
    let (titleLine : tagLine : notebookLine :
         creLine : updLine : _ : _ : bodyLines) =
            T.lines fileContent
    return Note {title = T.drop 7 titleLine
        , tagList = map (\x -> T.strip x) $ T.splitOn ";" $ T.drop 6 tagLine
        , notebook = T.drop 10 notebookLine
        , created = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S"
            $ T.unpack $ T.drop 9 creLine
        , updated = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S"
            $ T.unpack $ T.drop 9 updLine
        , content = T.unlines bodyLines
        , filePath = notePath }


loadNotes :: FilePath -> IO [Note]
loadNotes repoPath = do
    allFiles <- listDirectory repoPath
    let mdFiles = filter (\filename -> isSuffixOf ".md" filename) allFiles
    sequence $ map (\notePath -> parseNote $ joinPath [noteRepo, notePath]) mdFiles


filterByWords :: [String] -> [Note] -> [Note]
filterByWords words notes =
    let wordInNote word note =
            (T.isInfixOf word (title note))
            || (T.isInfixOf word (content note))
            || (any (\tag -> T.isInfixOf word tag) (tagList note))
    in foldl (\noteList word -> filter (wordInNote word) noteList)
             notes
             (map (\x -> T.pack x) words)


simpleSearch :: [String] -> IO [Note]
simpleSearch words = (filterByWords words) <$> (loadNotes noteRepo)


listNotes num = putStrLn ("List " ++ num ++ " notes:")
