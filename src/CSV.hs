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
import           Control.Exception          ( handle
                                            , IOException
                                            )
import           Data.Bifunctor             ( bimap )
import           Data.Functor               ( (<&>) )
import qualified Data.HashMap.Strict as HM
import           Data.List                  ( transpose )
import           Data.Maybe                 ( fromMaybe )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.Directory           ( createDirectoryIfMissing
                                            , getXdgDirectory
                                            , XdgDirectory( XdgConfig )
                                            )

import           System.IO                  ( stderr )
import           Text.ParserCombinators.Parsec hiding ( (<|>))
import           Text.Parsec.Text hiding    ( GenParser )

configDir :: IO FilePath
configDir = getXdgDirectory XdgConfig "owen/"

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
escapeQuotes x =  "\"" <> T.replace "\"" "\"\"" x <> "\""

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
    base <- configDir
    contents <- handle
        (\e -> do
            let err = show (e :: IOException)
            TIO.hPutStrLn stderr $ T.pack ("[Warn] Error reading " <> base <> path)
            TIO.hPutStrLn stderr (T.pack err)
            pure "")
        $ readFile (base <> path)
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
    base <- configDir
    createDirectoryIfMissing True base
    handle
        (\e -> do
            let err = show (e :: IOException)
            TIO.hPutStrLn stderr $ T.pack ("[Warn] Error writing to " <> base <> path)
            TIO.hPutStrLn stderr (T.pack err)
            pure ())
        $ TIO.writeFile (base <> path)
        $ T.unlines
        $ fmap (T.intercalate "," . fmap escapeQuotes) contents

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
writeSingleColCSV path = writeCSV path . fmap (:[])
