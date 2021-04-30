{-# LANGUAGE OverloadedStrings #-}

module DB
    ( dbDir
    , readKV
    , lookupKV
    , prependKV
    , writeKV
    , fetch
    , store
    ) where

import Control.Monad              ( liftM )

import Data.Maybe                 ( fromMaybe
                                  , isNothing )
import Data.ByteString.Lazy as BS ( ByteString
                                  , readFile
                                  , writeFile )
import Data.Csv                   ( encode
                                  , decode
                                  , HasHeader (NoHeader)
                                  )
import Data.Text as T
import Data.Vector as V
import           System.Directory ( createDirectoryIfMissing
                                  , getXdgDirectory
                                  , XdgDirectory( XdgData )
                                  )

import CSV                        ( configDir )

-- | Directory to store KeyValues in: ~/.local/share/owen/db on UNIX,
-- %APPDATA% on Windows. Controlled by XDG_DATA environment variable.
dbDir :: IO FilePath
dbDir = getXdgDirectory XdgData "owen/db/"

-- | Locks a file for thread-safety
lockFile :: FilePath -> IO ()
lockFile file = return ()  -- TODO: Replace stub with working impl.

-- | Checks if a file is locked
isFileLocked :: FilePath -> IO Bool
isFileLocked file = return False  -- TODO: Replace stub with working impl.

readDB :: FilePath -> IO ByteString
readDB file = do
        base <- dbDir
        BS.readFile $ base <> file

writeDB :: FilePath -> ByteString -> IO ()
writeDB file db = do
    base <- dbDir
    BS.writeFile (base <> file) db


-- LIST OPS


-- KEY/VALUE OPS
type KeyValue = Vector (T.Text, T.Text)

-- | Reads a KeyValue DB from a file
readKV :: FilePath -> IO KeyValue
readKV file = do
    contents <- readDB file
    case decode NoHeader contents of
         Left  _  -> return $ V.singleton ("","")
         Right db -> return db

-- | Writes a KeyValue to a file
-- TODO: Handle errors
writeKV :: FilePath -> KeyValue -> IO ()
writeKV file = writeDB file . encode . V.toList

-- | Returns `Just T.Text` if `k` is in the KeyValue, `Nothing` otherwise.
lookupKV :: T.Text -> KeyValue -> Maybe T.Text
lookupKV k = lookup k . V.toList

-- | O(n) - Adds (k,v) to the start of the KeyValue
prependKV :: T.Text -> T.Text -> KeyValue -> KeyValue
prependKV k v = V.cons (k,v)

-- | O(n) - Adds (k,v) to the start of the KeyValue
appendKV :: T.Text -> T.Text -> KeyValue -> KeyValue
appendKV k v db = V.snoc db (k,v)

-- | Gets a value from a given db; if it doesn't exist, return ""
fetch :: FilePath -> T.Text -> IO T.Text
fetch path k = do
    db <- readKV path
    return $ fromMaybe "" $ lookupKV k db

-- | Prepends a key value pair to a db file. Handles it all!
-- TODO: This should really signal failure to the caller
store :: FilePath -> T.Text -> T.Text -> IO ()
store path k v = do
    db <- readKV path
    case lookupKV k db of
         Just v  -> putStrLn $ "[KeyValue] " <> T.unpack v <> " was already in the KeyValue!"
         Nothing -> writeKV path $ prependKV k v db
