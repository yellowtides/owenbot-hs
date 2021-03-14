{-# LANGUAGE OverloadedStrings #-}

module CSV ( configDir
           , readCSV
           , readSingleColCSV
           , writeCSV
           , writeSingleColCSV
           , addToCSV
           , writeHashMapToCSV
           ) where

import           Control.Applicative        ( (<|>) )
import           Data.Functor               ( (<&>) )
import           Data.List                  ( transpose )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.Directory           ( createDirectoryIfMissing )
import           System.IO                  ( stderr )
import           Text.ParserCombinators.Parsec hiding ( (<|>))
import           Text.Parsec.Text hiding    ( GenParser )
import           Control.Exception          ( handle
                                            , IOException )

import Data.Bifunctor ( bimap )

import qualified Data.HashMap.Strict as HM

configDir :: FilePath
configDir = ".owen/"

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

eol :: GenParser Char st String
eol = try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

-- | Reads CSV as 2-D T.Text list. If doesn't exist, creates new
-- file with empty contents and returns []
readCSV :: FilePath -> IO [[T.Text]]
readCSV path = do
    contents <- handle
        (\e -> do
            let err = show (e :: IOException)
            TIO.hPutStrLn stderr $ T.pack ("[Warn] Error reading" <> configDir <> path)
            TIO.hPutStrLn stderr (T.pack err)
            pure "")
        $ readFile (configDir <> path)
    case parse csvFile path contents of
        Left e       -> print e >> pure []
        Right result -> pure $ (T.pack <$>) <$> result

readSingleColCSV :: FilePath -> IO [T.Text]
readSingleColCSV path = do
    contents <- transpose <$> readCSV path
    if null contents
        then pure []
        else pure $ head contents

-- | Write CSV from 2-D T.Text list
writeCSV :: FilePath -> [[T.Text]] -> IO ()
writeCSV path contents = do
    createDirectoryIfMissing True configDir
    handle
        (\e -> do
            let err = show (e :: IOException)
            TIO.hPutStrLn stderr $ T.pack ("[Warn] Error writing to" <> configDir <> path)
            TIO.hPutStrLn stderr (T.pack err)
            pure ())
        $ TIO.writeFile (configDir <> path)
        $ T.unlines
        $ fmap (T.intercalate ",")
        $ fmap (fmap $ \x -> "\"" <> (T.replace "\"" "\"\"" x) <> "\"") contents

writeHashMapToCSV :: FilePath -> HM.HashMap T.Text T.Text -> IO ()
writeHashMapToCSV path hmap = do
    let textMap   = HM.toList hmap
    let textArray = map (\(key, value) -> [key, value]) textMap
    writeCSV path textArray

-- | Appends the given tabular `Text` data to the CSV present at the given path. If no
-- such CSV exists, a new one is created.
addToCSV :: FilePath -> [[T.Text]] -> IO ()
addToCSV path contents = do
    oldData <- readCSV path
    writeCSV path $ oldData <> contents

writeSingleColCSV :: FilePath -> [T.Text] -> IO ()
writeSingleColCSV path = (writeCSV path) . fmap (\x -> [x])
