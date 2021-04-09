{-# LANGUAGE OverloadedStrings #-}

module KeyValue
    ( readDB
    , lookupDB
    , prependDB
    , writeDB
    , fetch
    , store
    ) where

import Control.Monad           ( liftM )

import Data.Maybe                 ( fromMaybe
                                  , isNothing )
import Data.ByteString.Lazy as BS ( readFile
                                  , writeFile )
import Data.Csv                   ( encode
                                  , decode
                                  , HasHeader (NoHeader)
                                  )
import Data.Text as T
import Data.Vector as V

type DataBase = Vector (T.Text, T.Text)

-- | Locks a file for thread-safety
lockFile :: FilePath -> IO ()
lockFile file = return ()  -- TODO: Replace stub with working impl.

-- | Checks if a file is locked
isFileLocked :: FilePath -> IO Bool
isFileLocked file = do
    return False  -- TODO: Replace stub with working impl.

-- | Reads a DataBase from a file
readDB :: FilePath -> IO DataBase
readDB dbFile = do
    contents <- BS.readFile dbFile
    case decode NoHeader contents of
         Left  _  -> return $ V.singleton ("","")
         Right db -> return db

-- | Returns `Just T.Text` if `k` is in the database, `Nothing` otherwise.
lookupDB :: T.Text -> DataBase -> Maybe T.Text
lookupDB k = lookup k . V.toList

-- | O(n) - Adds (k,v) to the start of the database
prependDB :: T.Text -> T.Text -> DataBase -> DataBase
prependDB k v = V.cons (k,v)

-- | O(n) - Adds (k,v) to the start of the database
appendDB :: T.Text -> T.Text -> DataBase -> DataBase
appendDB k v db = V.snoc db (k,v)

-- | Writes a DataBase to a file
writeDB :: FilePath -> DataBase -> IO ()
writeDB path = BS.writeFile path . encode . V.toList

-- | Gets a value from a given db; if it doesn't exist, return ""
fetch :: FilePath -> T.Text -> IO T.Text
fetch path k = do
    db <- readDB path
    return $ fromMaybe "" $ lookupDB k db

-- | Prepends a key value pair to a db file. Handles it all!
-- TODO: This should really signal failure to the caller
store :: FilePath -> T.Text -> T.Text -> IO ()
store path k v = do
    db <- readDB path
    case lookupDB k db of
         Just v  -> putStrLn $ T.unpack v <> " was already in the database!"
         Nothing -> writeDB path $ prependDB k v db
