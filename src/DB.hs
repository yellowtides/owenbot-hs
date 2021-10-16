{-# LANGUAGE OverloadedStrings #-}

module DB
    ( initGlobalDatabase
    , initGuildSpecificDatabase
    , readDB
    , writeDB
    , appendDB
    , readListDB
    , writeListDB
    , readHashMapDB
    , writeHashMapDB
    , DBTable(..)
    ) where

import Control.Applicative ((<|>))
import Control.Exception (IOException, handle)
import Control.Monad (unless, void)
import Data.Bifunctor (bimap)
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HM
import Data.List (intercalate, transpose)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics
import System.Directory
import System.IO (stderr)
import Text.Parsec.Text hiding (GenParser)
import Text.ParserCombinators.Parsec hiding ((<|>))

import Discord.Types

-- | Directory to store KeyValues in: ~/.local/share/owen on UNIX,
-- %APPDATA% on Windows. Controlled by XDG_DATA environment variable.
getDBDir :: IO FilePath
getDBDir = getXdgDirectory XdgData "owen"

-- | DbTable represents the two possible database types: global and
-- guild-specific. The table name (filename without .csv) is encoded too.
data DBTable = GlobalDB String | GuildDB GuildId String
    deriving (Show, Eq)

-- | @initialiseGlobalDatabase@ creates the default database directory. See
-- @getDbDir@ for the path. It will also generate the following default file:
--   @quotes.csv@ containing an empty hashmap
--
-- If the directory already exists, no files will be overwritten or generated.
initGlobalDatabase :: IO ()
initGlobalDatabase = do
    dbDir               <- getDBDir
    globalDatabaseExist <- doesDirectoryExist dbDir
    unless globalDatabaseExist $ do
        createDirectoryIfMissing True dbDir
        -- quotes are global
        writeHashMapDB (GlobalDB "registeredQuotes") HM.empty
        writeListDB (GlobalDB "status") ["online", "playing", "initialised!"]

-- | @initGuildSpecificDatabase gid@ creates a subdirectory within the global
-- database directory (created during first global init) that is named @gid@.
-- See @getDbDir@ for the parent path.
-- It will also generate the following default files:
--   @reactLim.csv@ containing 5
--   @hallOfFame.csv@ containing an empty list
--
-- If the directory already exists, no files will be overwritten or generated.
initGuildSpecificDatabase :: GuildId -> IO ()
initGuildSpecificDatabase gid = do
    let gidStr = show gid
    dbDir              <- getDBDir
    guildDatabaseExist <- doesDirectoryExist $ dbDir <> "/" <> gidStr
    unless guildDatabaseExist $ do
        -- Assumes that initGlobalDirectory was called at startup. If the
        -- directory is destroyed during runtime, the following will error
        -- (rightfully so).
        createDirectoryIfMissing True $ dbDir <> "/" <> gidStr
        writeDB (GuildDB gid "reactLimit")  [["5"]]
        writeDB (GuildDB gid "hallOfFame")  []
        writeDB (GuildDB gid "fameChannel") []
        writeDB (GuildDB gid "mcServer")    []
        writeDB (GuildDB gid "idAssign")    []

-- | Locks a file for thread-safety
lockFile :: FilePath -> IO ()
lockFile file = return ()  -- TODO: Replace stub with working impl.

-- | Checks if a file is locked
isFileLocked :: FilePath -> IO Bool
isFileLocked file = return False  -- TODO: Replace stub with working impl.

readDB :: DBTable -> IO [[T.Text]]
readDB table = do
    dbDir <- getDBDir
    let fp = case table of
            GlobalDB name    -> dbDir <> "/" <> name <> ".csv"
            GuildDB gid name -> dbDir <> "/" <> show gid <> "/" <> name <> ".csv"
    readCSV fp

writeDB :: DBTable -> [[T.Text]] -> IO ()
writeDB table contents = do
    dbDir <- getDBDir
    let fp = case table of
            GlobalDB name    -> dbDir <> "/" <> name <> ".csv"
            GuildDB gid name -> dbDir <> "/" <> show gid <> "/" <> name <> ".csv"
    writeCSV fp contents

appendDB :: DBTable -> [[T.Text]] -> IO ()
appendDB table contents = do
    old <- readDB table
    writeDB table $ old <> contents

readListDB :: DBTable -> IO [T.Text]
readListDB table = do
    contents <- transpose <$> readDB table
    if null contents then pure [] else pure $ head contents

writeListDB :: DBTable -> [T.Text] -> IO ()
writeListDB table = writeDB table . fmap (: [])

writeHashMapDB :: DBTable -> HM.HashMap T.Text T.Text -> IO ()
writeHashMapDB table hmap = do
    let textMap   = HM.toList hmap
    let textArray = map (\(key, value) -> [key, value]) textMap
    writeDB table textArray

readHashMapDB :: DBTable -> IO (HM.HashMap T.Text T.Text)
readHashMapDB table = do
    contents <- readDB table
    let compatibleLines = filter (\line -> length line == 2) contents
    let listMap         = map (\[key, value] -> (key, value)) compatibleLines
    return $ HM.fromList listMap

-- ===== CSV Parser =====

csvFile :: GenParser Char st [[String]]
csvFile = endBy line eol

line :: GenParser Char st [String]
line = sepBy cell (char ',')

cell :: GenParser Char st String
cell = quotedCell <|> many (noneOf ",\n\r")

quotedCell :: GenParser Char st String
quotedCell = do
    char '"'
    content <- many quotedChar
    char '"'
    return content

quotedChar :: GenParser Char st Char
quotedChar = noneOf "\"" <|> try (string "\"\"" >> return '"')

escapeQuotes :: T.Text -> T.Text
escapeQuotes x = "\"" <> T.replace "\"" "\"\"" x <> "\""

eol :: GenParser Char st String
eol =
    try (string "\n\r")
        <|> try (string "\r\n")
        <|> string "\n"
        <|> string "\r"
        <?> "end of line"

errStrLn :: String -> IO ()
errStrLn = TIO.hPutStrLn stderr . T.pack

-- =========== CSV IO Primitives =============

-- | Reads CSV as 2-D T.Text list. If doesn't exist, creates new
-- file with empty contents and returns []
readCSV :: FilePath -> IO [[T.Text]]
readCSV path = do
    contents <-
        handle
                (\e -> do
                    errStrLn $ "[DB] Error reading " <> path
                    errStrLn $ "[DB] " <> show (e :: IOException)
                    pure "" -- it needs a default value
                )
            $ readFile path
    case parse csvFile path contents of
        Left  e      -> print e >> pure []
        Right result -> pure $ (T.pack <$>) <$> result

-- | Write CSV from 2-D T.Text list
writeCSV :: FilePath -> [[T.Text]] -> IO ()
writeCSV path contents =
    handle
            (\e -> do
                errStrLn $ "[DB] Error writing to " <> path
                void $ errStrLn $ show (e :: IOException)
            )
        $ TIO.writeFile path
        $ T.unlines
        $ fmap (T.intercalate "," . fmap escapeQuotes) contents

-- | Appends the given tabular `Text` data to the CSV present at the given path. If no
-- such CSV exists, a new one is created.
appendCSV :: FilePath -> [[T.Text]] -> IO ()
appendCSV path contents = do
    oldData <- readCSV path
    writeCSV path $ oldData <> contents
