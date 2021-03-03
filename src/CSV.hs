{-# LANGUAGE OverloadedStrings #-}

module CSV ( readCSV
           , readSingleColCSV
           , writeCSV
           , writeSingleColCSV
           ) where

import           Data.Functor               ( (<&>) )
import           Data.List                  ( transpose )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Text.ParserCombinators.Parsec
import           Text.Parsec.Text hiding    ( GenParser )

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
    contents <- readFile path
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
writeCSV path contents = TIO.writeFile path
    $ T.unlines
    $ fmap (T.intercalate ",")
    $ fmap (fmap $ \x -> "\"" <> (T.replace "\"" "\"\"" x) <> "\"") contents

writeSingleColCSV :: FilePath -> [T.Text] -> IO ()
writeSingleColCSV path = (writeCSV path) . fmap (\x -> [x])