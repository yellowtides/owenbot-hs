{-# LANGUAGE OverloadedStrings #-}

module DB
    ( dbDir
    ) where

import GHC.Generics

import Control.Monad              ( liftM )

import Data.Maybe                 ( fromMaybe
                                  , fromJust
                                  , isNothing )
import Data.ByteString.Lazy as BS ( ByteString
                                  , readFile
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

-- | Locks a file for thread-safety
lockFile :: FilePath -> IO ()
lockFile file = return ()  -- TODO: Replace stub with working impl.

-- | Checks if a file is locked
isFileLocked :: FilePath -> IO Bool
isFileLocked file = return False  -- TODO: Replace stub with working impl.

readDB :: FromJSON a => FilePath -> IO a
readDB file = do
    base <- dbDir
    json <- BS.readFile $ base <> file
    return $ fromJust $ decode json

writeDB :: ToJSON a => FilePath -> a -> IO ()
writeDB file db = do
    base <- dbDir
    BS.writeFile (base <> file) $ encode db
