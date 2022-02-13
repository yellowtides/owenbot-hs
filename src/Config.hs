{-# LANGUAGE DeriveGeneric #-}
module Config where

import Control.Exception (IOException, try)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString as BS (ByteString, readFile, writeFile)
import qualified Data.ByteString.Lazy as BL (ByteString, fromStrict, toStrict)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import GHC.Generics
import System.Directory
    (XdgDirectory(XdgConfig), createDirectoryIfMissing, getXdgDirectory)

import Discord.Types (ChannelId)

-- | OwenConfig represents the configuration of Owenbot!
data OwenConfig = OwenConfig
    { owenConfigToken       :: T.Text
    , owenConfigDevs        :: [T.Text]
    , owenConfigOwoFreq     :: Int -- these two don't do anything yet
    , owenConfigDadFreq     :: Int -- because reading values every time is slow and a solution can't be thought of
    , owenConfigRepoDir     :: Maybe FilePath
    , owenConfigStartupChan :: ChannelId
    , owenConfigQuizChan    :: ChannelId -- maybe move this into a per-guild db
    }
    deriving (Generic, Show)

instance FromJSON OwenConfig
instance ToJSON OwenConfig

getConfigDir :: IO FilePath
getConfigDir = getXdgDirectory XdgConfig "owen"

-- | Takes a filename and reads json from it into a data structure.
readConfig :: IO OwenConfig
readConfig = do
    createDirectoryIfMissing True <$> getConfigDir
    fp   <- (<> "/config.json") <$> getConfigDir
    json <- BS.readFile fp
    case eitherDecode (BL.fromStrict json) of
        Left e ->
            error
                $  "Incorrect config format, can't continue running Owen:\n[ERROR] "
                <> e
        Right cfg -> pure cfg


-- (commented since writing to config is never necessary and goes against rules)
-- | Takes a filename (with no suffix) and a data structure, and writes a json
-- file to that location.
-- writeConfig :: ToJSON a => String -> a -> IO ()
-- writeConfig file db = do
--     fp <- mkPath file
--     BS.writeFile fp $ BL.toStrict $ encode db
