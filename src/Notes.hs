{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Notes
    ( parse
    ) where

import Data.List
import Data.Time
import Data.Version (showVersion)
import NeatInterpolation (trimming)
import Paths_hod (version)
import System.Directory
import System.FilePath.Posix
import System.Process
import TextShow
import Text.Pandoc.Shared
import Utils
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


usage = [trimming|
    Usage:
    a: add a new note
    l [N]: list the most recent [N] notes
    e [N]: edit the <N>th note, 1st by default
    v [N]: view the <N>th note, 1st by default
    s [-a]: search notes (-a for advanced mode)
    b [message]: backup note repo
    conf <get/set>: get/set config
|]

data Note = Note
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
tmpNotePath = "/tmp/newnote.md"
defaultNotebook = "/Diary/2021"


parse :: [T.Text] -> IO ()
parse ["a"] = addNote
parse ["add"] = addNote
parse ["e"] = editNote 1
parse ["e", num] = editNote dispNum
    where dispNum = read $ T.unpack num :: Int
parse ["l"] = listNotes defaultListLen
parse ["l", num] = listNotes dispNum
    where dispNum = read $ T.unpack num :: Int
parse ("s": "-a": words) = advancedSearch words >>= saveAndDisplayList
parse ("s": words) = simpleSearch words >>= saveAndDisplayList
parse ["v"] = viewNote 1
parse ["v", num] = viewNote dispNum
    where dispNum = read $ T.unpack num :: Int
parse ["ver"] = parse ["version"]
parse ["version"] = putStrLn $ showVersion version
parse _ = TIO.putStrLn usage


parseNote :: FilePath -> IO Note
parseNote notePath = do
    fileContent <- TIO.readFile notePath
    let titleLine : tagLine : notebookLine :
            creLine : updLine : _ : _ : bodyLines =
            T.lines fileContent
    return Note { title = T.drop 7 titleLine
        , tagList = map T.strip $ T.splitOn ";" $ T.drop 6 tagLine
        , notebook = T.drop 10 notebookLine
        , created = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S"
            $ T.unpack $ T.drop 9 creLine
        , updated = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S"
            $ T.unpack $ T.drop 9 updLine
        , content = T.unlines bodyLines
        , filePath = T.pack notePath }


addNote :: IO ()
addNote = do
    rawTime <- formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" <$> getZonedTime
    let curTime = deQuoteT $ tshow rawTime
    let templ = [trimming|
        Title: 
        Tags: 
        Notebook: $defaultNotebook
        Created: $curTime
        Updated: $curTime

        ------

    |]
    TIO.writeFile tmpNotePath templ
    callProcess "nvim" [tmpNotePath]
    timestamp <- formatTime defaultTimeLocale "%y%m%d%H%M%S" <$> getZonedTime
    let noteName = "note" <> timestamp <> ".md"
    newNote <- parseNote tmpNotePath
    case title newNote of
        "" -> putStrLn "Cancelled for empty title!"
        _ -> copyFile tmpNotePath $ joinPath [noteRepo, noteName]


data SearchItem
    = Title T.Text
    | Tag T.Text
    | Notebook T.Text
    | Created LocalTime
    | Updated LocalTime
    | Content T.Text

data SearchFlag
    = TextFlag Bool Bool
    | DateFlag Bool

data SearchTerm = SearchTerm
    { body :: SearchItem
    , flag :: SearchFlag }

advancedSearch :: [T.Text] -> IO [Note]
advancedSearch words = matchWords (map parseTerm words) <$> loadSortedNotes noteRepo
  where
    parseTerm :: T.Text -> Maybe SearchTerm
    parseTerm arg =
        case (bodyM, optionM) of
            (Just theBody, Just theOption) ->
                Just SearchTerm { body = theBody, flag = theOption }
            _ -> Nothing
      where
        searchBody :: [T.Text] -> Maybe SearchItem
        searchBody ("ti": stem: _) = Just $ Title stem
        searchBody ("ta": stem: _) = Just $ Tag stem
        searchBody ("nb": stem: _) = Just $ Notebook stem
        searchBody ("cr": stem: _) = Just $ Created $
            parseTimeOrError True defaultTimeLocale "%Y-%m-%d" $ T.unpack stem
        searchBody ("up": stem: _) = Just $ Updated $
            parseTimeOrError True defaultTimeLocale "%Y-%m-%d" $ T.unpack stem
        searchBody ("co": stem: _) = Just $ Content stem
        searchBody _ = Nothing

        searchOption :: [T.Text] -> Maybe SearchFlag
        searchOption ["cr", _] = Just $ DateFlag False
        searchOption ["up", _] = Just $ DateFlag False
        searchOption [_, _] = Just $ TextFlag True False

        searchOption [_, _, "i"] = Just $ TextFlag True False
        searchOption [_, _, "W"] = Just $ TextFlag True False
        searchOption [_, _, "iW"] = Just $ TextFlag True False
        searchOption [_, _, "Wi"] = Just $ TextFlag True False

        searchOption [_, _, "I"] = Just $ TextFlag False False
        searchOption [_, _, "IW"] = Just $ TextFlag False False
        searchOption [_, _, "WI"] = Just $ TextFlag False False

        searchOption [_, _, "w"] = Just $ TextFlag True True
        searchOption [_, _, "iw"] = Just $ TextFlag True True
        searchOption [_, _, "wi"] = Just $ TextFlag True True

        searchOption [_, _, "Iw"] = Just $ TextFlag False True
        searchOption [_, _, "wI"] = Just $ TextFlag False True

        searchOption ["cr", _, "b"] = Just $ DateFlag True
        searchOption ["cr", _, "B"] = Just $ DateFlag False
        searchOption ["up", _, "b"] = Just $ DateFlag True
        searchOption ["up", _, "B"] = Just $ DateFlag False
        searchOption _ = Nothing

        termList = T.splitOn ":" arg

        bodyM = searchBody termList
        optionM = searchOption termList

    matchTerm :: Maybe SearchTerm -> Note -> Bool
    matchTerm Nothing note = False

    matchTerm (Just SearchTerm
                  { body = Title term
                  , flag = TextFlag ignoreCase wholeWord })
              note =
        if wholeWord then elem token titleWords
                     else T.isInfixOf token titleLine
          where
            [token, titleLine] = if ignoreCase then map T.toLower [term, title note]
                                               else [term, title note]
            titleWords = T.splitOn " " titleLine

    matchTerm (Just SearchTerm
                  { body = Tag term
                  , flag = TextFlag ignoreCase wholeWord })
              note =
        if wholeWord then elem token tags
                     else any (T.isInfixOf token) tags
          where
            (token : tags) = if ignoreCase then map T.toLower (term : tagList note)
                                           else (term : tagList note)

    matchTerm (Just SearchTerm
                  { body = Notebook term
                  , flag = TextFlag ignoreCase wholeWord })
              note =
        if wholeWord then elem token nbLevels
                     else T.isInfixOf token nbLine
          where
            [token, nbLine] = if ignoreCase then map T.toLower [term, notebook note]
                                            else [term, notebook note]
            nbLevels = T.splitOn "/" nbLine

    matchTerm (Just SearchTerm
                  { body = Created term
                  , flag = DateFlag beforeDate })
              note =
        if beforeDate then created note < term
                      else created note >= term

    matchTerm (Just SearchTerm
                  { body = Updated term
                  , flag = DateFlag beforeDate })
              note =
        if beforeDate then updated note < term
                      else updated note >= term

    matchTerm (Just SearchTerm {body = Content term
                  , flag = TextFlag ignoreCase wholeWord })
              note =
        if wholeWord then elem token bodyWords
                     else T.isInfixOf token body
          where
            [token, body] = if ignoreCase then map T.toLower [term, content note]
                                          else [term, content note]
            bodyWords = T.splitOn " " body

    matchTerm _ note = False

    matchWords :: [Maybe SearchTerm] -> [Note] -> [Note]
    matchWords terms notes =
        foldl (\noteList word -> filter (matchTerm word) noteList)
              notes
              terms


editNote :: Int -> IO ()
editNote num = do
    fileContent <- TIO.readFile recordPath
    let notePath = (T.lines fileContent) !! (num - 1)
    callProcess "nvim" [T.unpack notePath]


listNotes :: Int -> IO ()
listNotes num = take num <$> loadSortedNotes noteRepo >>= saveAndDisplayList


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
  where
    wordInNote :: T.Text -> Note -> Bool
    wordInNote word note = (T.isInfixOf word $ title note)
        || (T.isInfixOf word $ content note)
        || (any (T.isInfixOf word) $ tagList note)
    filterByWords :: [T.Text] -> [Note] -> [Note]
    filterByWords words notes =
        foldl (\noteList word -> filter (wordInNote word) noteList)
              notes
              words


viewNote :: Int -> IO ()
viewNote num = do
    fileContent <- TIO.readFile recordPath
    let notePath = (T.lines fileContent) !! (num - 1)
    callProcess "nvim" ["-R", T.unpack notePath]


