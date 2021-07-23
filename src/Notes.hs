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
import TextShow
import Text.Pandoc.Shared
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


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

instance TextShow Note where
    showb note = fromText $ mconcat [(tshow $ localDay $ updated note)
                         , " "
                         , (notebook note)
                         , ": "
                         , (title note)
                         , " "
                         , (tshow $ localDay $ created note)
                         , " "
                         , (tshow $ tagList note)]

noteRepo = "/home/leo/.donno/repo"

parse ["l", num] = listNotes num
parse ("s": words) = do
    notes <-simpleSearch words
    TIO.putStrLn $ T.unlines $ map (\x -> showt x) notes


parseNote :: FilePath -> IO Note
parseNote notePath = do
    fileContent <- TIO.readFile notePath
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


filterByWords :: [T.Text] -> [Note] -> [Note]
filterByWords words notes =
    let wordInNote word note =
            (T.isInfixOf word (title note))
            || (T.isInfixOf word (content note))
            || (any (\tag -> T.isInfixOf word tag) (tagList note))
    in foldl (\noteList word -> filter (wordInNote word) noteList)
             notes
             words


simpleSearch :: [T.Text] -> IO [Note]
simpleSearch words = (filterByWords words) <$> (loadNotes noteRepo)


listNotes num = TIO.putStrLn ("List " <> num <> " notes:")
