{-# LANGUAGE OverloadedStrings #-}
module Notes
    ( parse
    ) where

import Data.List
import Data.Time
import System.Directory
import System.FilePath.Posix
import System.Process
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
        , filePath :: T.Text
        } deriving (Ord, Eq)

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
recordPath = "/home/leo/.donno/reclist"
defaultListLen = 5

parse :: [T.Text] -> IO ()
parse ["e"] = editNote 1
parse ["e", num] = editNote dispNum
    where dispNum = read $ T.unpack num :: Int
parse ["l"] = listNotes defaultListLen
parse ["l", num] = listNotes dispNum
    where dispNum = read $ T.unpack num :: Int
parse ("s": words) = simpleSearch words >>= saveAndDisplayList
parse ["v"] = viewNote 1
parse ["v", num] = viewNote dispNum
    where dispNum = read $ T.unpack num :: Int


parseNote :: FilePath -> IO Note
parseNote notePath = do
    fileContent <- TIO.readFile notePath
    let titleLine : tagLine : notebookLine :
            creLine : updLine : _ : _ : bodyLines =
            T.lines fileContent
    return Note {title = T.drop 7 titleLine
        , tagList = map T.strip $ T.splitOn ";" $ T.drop 6 tagLine
        , notebook = T.drop 10 notebookLine
        , created = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S"
            $ T.unpack $ T.drop 9 creLine
        , updated = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S"
            $ T.unpack $ T.drop 9 updLine
        , content = T.unlines bodyLines
        , filePath = T.pack notePath }


loadSortedNotes :: FilePath -> IO [Note]
loadSortedNotes repoPath = do
    mdFiles <- (filter $ isSuffixOf ".md") <$> listDirectory repoPath
    notes <- sequence $ map (\notePath ->
                                parseNote $ joinPath [repoPath, notePath])
                            mdFiles
    return $ reverse $ sortOn updated notes


saveAndDisplayList :: [Note] -> IO ()
saveAndDisplayList [] = TIO.putStrLn ""
saveAndDisplayList notes = do
    TIO.writeFile recordPath $ T.unlines $ map filePath notes
    TIO.putStrLn
        $ T.unlines
        $ "No. Updated, Notebook, Title, Created, Tags" :
            map (\(idx, note) -> tshow idx <> ". " <> showt note) noteEnums
        where noteEnums = zip [1 .. ] notes


simpleSearch :: [T.Text] -> IO [Note]
simpleSearch words = filterByWords words <$> loadSortedNotes noteRepo
    where wordInNote :: T.Text -> Note -> Bool
          wordInNote word note = (T.isInfixOf word $ title note)
              || (T.isInfixOf word $ content note)
              || (any (T.isInfixOf word) $ tagList note)
          filterByWords :: [T.Text] -> [Note] -> [Note]
          filterByWords words notes =
              foldl (\noteList word -> filter (wordInNote word) noteList)
                    notes
                    words


listNotes :: Int -> IO ()
listNotes num = take num <$> loadSortedNotes noteRepo >>= saveAndDisplayList


viewNote :: Int -> IO ()
viewNote num = do
    fileContent <- TIO.readFile recordPath
    let notePath = (T.lines fileContent) !! (num - 1)
    callProcess "nvim" ["-R", T.unpack notePath]


editNote :: Int -> IO ()
editNote num = do
    fileContent <- TIO.readFile recordPath
    let notePath = (T.lines fileContent) !! (num - 1)
    callProcess "nvim" [T.unpack notePath]

