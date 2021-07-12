{-# LANGUAGE OverloadedStrings #-}

module DB
    ( dbDir
    , mkPath
    , readDB
    , writeDB
    ) where

import GHC.Generics

import Control.Monad              ( liftM )

import Data.Maybe                 ( fromMaybe
                                  , fromJust
                                  , isNothing )
import Data.ByteString.Lazy as BL ( ByteString
                                  , fromStrict
                                  , toStrict )
import Data.ByteString      as BS ( readFile
                                  , writeFile )
import Data.Aeson                 ( FromJSON
                                  , ToJSON
                                  , encode
                                  , decode
                                  )
import Data.Text as T
import Data.Vector as V
import           System.Directory ( createDirectoryIfMissing
                                  , getXdgDirectory
                                  , XdgDirectory ( XdgData )
                                  )

-- | Directory to store KeyValues in: ~/.local/share/owen/db on UNIX,
-- %APPDATA% on Windows. Controlled by XDG_DATA environment variable.
dbDir :: IO FilePath
dbDir = getXdgDirectory XdgData "owen/db/"

mkPath :: String -> IO FilePath
mkPath name = (<> name <> ".json") <$> dbDir

-- | Locks a file for thread-safety
lockFile :: FilePath -> IO ()
lockFile file = return ()  -- TODO: Replace stub with working impl.

-- | Checks if a file is locked
isFileLocked :: FilePath -> IO Bool
isFileLocked file = return False  -- TODO: Replace stub with working impl.

-- | Takes a filename and reads json from it into a data structure.
readDB :: FromJSON a => String -> IO (Maybe a)
readDB file = do
    fp <- mkPath file
    json <- BS.readFile fp
    return $ decode $ BL.fromStrict json

-- | Takes a filename (with no suffix) and a data structure, and writes a json
-- file to that location.
writeDB :: ToJSON a => String -> a -> IO ()
writeDB file db = do
    fp <- mkPath file
    BS.writeFile fp $ BL.toStrict $ encode db
