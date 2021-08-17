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
import System.Environment
import System.FilePath.Posix
import System.Process
import TextShow
import Text.Pandoc.Shared
import Config
import Utils
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

evConfEnv = "XDG_CONFIG_HOME"

usage = [trimming|
    Usage:
    a: add a new note
    l [N]: list the most recent [N] notes
    e [N]: edit the <N>th note, 1st by default
    v [N]: view the <N>th note, 1st by default
    pv [N]: like view command while view rendered text in browser, 1st by default
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
    , filePath :: String
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
                         , (showTextList $ tagList note)]


parse :: [T.Text] -> IO ()
parse ["a"] = addNote
parse ["add"] = addNote
parse ["b"] = backupDryRun
parse ["backup"] = backupDryRun
parse ["b", message] = backup message
parse ["backup", message] = backup message
parse ("conf" : "get" : key) = getConfig key
parse ["conf", "set", key, value] = setConfig (T.unpack key) (T.unpack value)
parse ["e"] = editNote 1
parse ["e", num] = editNote dispNum
    where dispNum = read $ T.unpack num :: Int
parse ["l"] = do
    noteNum <- defaultListLength . userConf <$> loadConfig
    listNotes noteNum
parse ["l", num] = listNotes dispNum
    where dispNum = read $ T.unpack num :: Int
parse ("s": "-a": words) = advancedSearch words >>= saveAndDisplayList
parse ("s": words) = simpleSearch words >>= saveAndDisplayList
parse ["v"] = viewNote 1
parse ["v", num] = viewNote dispNum
    where dispNum = read $ T.unpack num :: Int
parse ["pv"] = previewNote 1
parse ["pv", num] = previewNote dispNum
    where dispNum = read $ T.unpack num :: Int
parse ["ver"] = parse ["version"]
parse ["version"] = putStrLn $ showVersion version
parse _ = TIO.putStrLn usage


-- |Read a 'Note' from the file at 'notePath'
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
        , filePath = notePath }


addNote :: IO ()
addNote = do
    defNotebook <- T.pack . defaultNotebook . userConf <$> loadConfig
    rawTime <- formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" <$> getZonedTime
    let curTime = deQuoteT $ tshow rawTime
    let templ = [trimming|
        Title: 
        Tags: 
        Notebook: $defNotebook
        Created: $curTime
        Updated: $curTime

        ------

    |]
    tmpNotePath <- tempNote <$> loadConfig
    TIO.writeFile tmpNotePath templ
    theEditor <- editor . userConf <$> loadConfig
    callProcess theEditor [tmpNotePath]
    timestamp <- formatTime defaultTimeLocale "%y%m%d%H%M%S" <$> getZonedTime
    let noteName = "note" <> timestamp <> ".md"
    newNote <- parseNote tmpNotePath
    repoPath <- noteRepo <$> loadConfig
    case title newNote of
        "" -> putStrLn "Cancelled for empty title!"
        _ -> copyFile tmpNotePath $ joinPath [repoPath, noteName]


backupDryRun :: IO ()
backupDryRun = do
    gitRoot <- noteRepo <$> loadConfig
    setCurrentDirectory gitRoot
    callProcess "git" ["status"]


backup :: T.Text -> IO ()
backup message = do
    gitRoot <- noteRepo <$> loadConfig
    setCurrentDirectory gitRoot
    callProcess "git" ["add", "-A"]
    callProcess "git" ["commit", "-m", T.unpack message]
    callProcess "git" ["push", "origin", "master"]


-- |Save a 'note' to the 'path'
saveNote :: Note -> String -> IO ()
saveNote note path = do
    let noteStr = T.unlines [ "Title: " <> title note
                            , "Tags: " <> (T.intercalate "; " $ tagList note)
                            , "Notebook: " <> notebook note
                            , "Created: " <> (tshow $ created note)
                            , "Updated: " <> (tshow $ updated note)
                            , "\n------"
                            , content note ]
    TIO.writeFile path noteStr


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
advancedSearch words = do
    repoPath <- noteRepo <$> loadConfig
    matchWords (map parseTerm words) <$> loadSortedNotes repoPath
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
    recordPath <- recordFile <$> loadConfig
    fileContent <- TIO.readFile recordPath
    let notePath = (T.lines fileContent) !! (num - 1)
    theEditor <- editor . userConf <$> loadConfig
    evConf <- evConfPath . userConf <$> loadConfig
    setEnv evConfEnv evConf
    callProcess theEditor [T.unpack notePath]
    unsetEnv evConfEnv
    note <- parseNote $ T.unpack notePath
    curTime <- zonedTimeToLocalTime <$> getZonedTime
    saveNote (note { updated = roundLocalTime curTime })
             (filePath note)


listNotes :: Int -> IO ()
listNotes num = do
    repoPath <- noteRepo <$> loadConfig
    take num <$> loadSortedNotes repoPath >>= saveAndDisplayList


-- |Load all markdown file from the 'repoPath' and ordered with updated time
loadSortedNotes :: FilePath -> IO [Note]
loadSortedNotes repoPath = do
    mdFiles <- (filter $ isSuffixOf ".md") <$> listDirectory repoPath
    notes <- sequence $ map (\notePath ->
                                parseNote $ joinPath [repoPath, notePath])
                            mdFiles
    return $ reverse $ sortOn updated notes


-- |Save 'notes' (a list of Note created by search or list command)
-- to record file and print result to stdout
saveAndDisplayList :: [Note] -> IO ()
saveAndDisplayList [] = TIO.putStrLn ""
saveAndDisplayList notes = do
    recordPath <- recordFile <$> loadConfig
    writeFile recordPath $ unlines $ map filePath notes
    TIO.putStrLn
        $ T.unlines
        $ "No. Updated, Notebook, Title, Created, Tags" :
            map (\(idx, note) -> tshow idx <> ". " <> showt note) noteEnums
        where noteEnums = zip [1 .. ] notes


simpleSearch :: [T.Text] -> IO [Note]
simpleSearch words = do
    repoPath <- noteRepo <$> loadConfig
    filterByWords words <$> loadSortedNotes repoPath
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
    recordPath <- recordFile <$> loadConfig
    fileContent <- TIO.readFile recordPath
    let notePath = (T.lines fileContent) !! (num - 1)
    theViewer <- viewer . userConf <$> loadConfig
    evConf <- evConfPath . userConf <$> loadConfig
    setEnv evConfEnv evConf
    case words theViewer of
      [ viewerExe ] -> callProcess viewerExe [T.unpack notePath]
      ( viewerExe : args ) -> callProcess viewerExe (args ++ [T.unpack notePath])


previewNote :: Int -> IO ()
previewNote num = do
    recordPath <- recordFile <$> loadConfig
    fileContent <- TIO.readFile recordPath
    let notePath = (T.lines fileContent) !! (num - 1)
    previewPath <- previewFile <$> loadConfig
    callProcess "pandoc" [ "--standalone"
                         , "--mathjax"
                         , "--toc"
                         , "--filter"
                         , "mermaid-filter"
                         , "--output"
                         , previewPath
                         , T.unpack notePath ]
    browserName <- browser . userConf <$> loadConfig
    callProcess browserName [previewPath]


getConfig :: [T.Text] -> IO ()
getConfig key = do
    userConfig <- userConf <$> loadConfig
    case key of
        [] -> print userConfig
        [ "appHome" ] -> putStrLn $ appHome userConfig
        [ "defaultNotebook" ] -> putStrLn $ defaultNotebook userConfig
        [ "editor" ] -> putStrLn $ editor userConfig
        [ "viewer" ] -> putStrLn $ viewer userConfig
        [ "evConfPath" ] -> putStrLn $ evConfPath userConfig
        [ "defaultListLength" ] -> print $ defaultListLength userConfig
        [ "browser" ] -> putStrLn $ browser userConfig
        _ -> putStrLn "Invalid key name"


setConfig :: String -> String -> IO ()
setConfig key value = do
    userConfig <- userConf <$> loadConfig
    let newConf = case key of
            "appHome" -> userConfig { appHome = value }
            "defaultNotebook" -> userConfig { defaultNotebook = value }
            "editor" -> userConfig { editor = value }
            "viewer" -> userConfig { viewer = value }
            "defaultListLength" -> userConfig { defaultListLength = (read value :: Int) }
            "browser" -> userConfig { browser = value }
            _ -> userConfig
    saveConfig newConf


