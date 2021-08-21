{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving
           , DeriveGeneric #-}

module Status (writeStatusFile, setStatusFromFile, updateStatus) where

import Control.Monad (when)
import Control.Exception.Safe (onException)
import qualified Data.Text as T
import GHC.Generics
import Text.Read (readMaybe)

import Data.Aeson (FromJSON, ToJSON)
import Discord.Types
import Discord (sendCommand, DiscordHandler)
import UnliftIO (liftIO)

import CSV (readCSV, writeCSV)
import Command
import DB

-- | Instances to allow us to read/write these ADTs
deriving instance Generic UpdateStatusType
instance ToJSON           UpdateStatusType
instance FromJSON         UpdateStatusType

deriving instance Generic ActivityType
instance ToJSON           ActivityType
instance FromJSON         ActivityType

-- | 'updateStatus' updates the status through the Discord gateway.
-- Therefore, it requires DiscordHandler and is not polymorphic.
updateStatus :: UpdateStatusType -> ActivityType -> T.Text -> DiscordHandler ()
updateStatus newStatus newType newName = sendCommand $ UpdateStatus $ UpdateStatusOpts
    { updateStatusOptsSince     = Nothing
    , updateStatusOptsGame      = Just $ Activity
        { activityName = newName
        , activityType = newType
        , activityUrl  = Nothing
        }
    , updateStatusOptsNewStatus = newStatus
    , updateStatusOptsAFK       = False
    }

-- | @setStatusFromFile@ reads from the status db, and gets the 3 values for
-- 'UpdateStatusType', 'ActivityType', and 'T.Text'.
-- The values are used to call 'updateStatus'.
--
-- Incorrect formats (read parse errors) are ignored and reported.
setStatusFromFile :: DiscordHandler ()
setStatusFromFile = do
    status <- liftIO readStatusFile
    case status of
        Nothing        -> liftIO $ putStrLn "Incorrect status format, ignoring."
        Just (s, a, n) -> updateStatus s a n

-- | @writeStatusFile@ puts the status values into the status db.
writeStatusFile :: UpdateStatusType -> ActivityType -> T.Text -> IO ()
writeStatusFile status activity name = writeDB "status" (status, activity, name)

-- | @readStatusFile@ gets the saved status info from the db.
readStatusFile :: IO (Maybe (UpdateStatusType, ActivityType, T.Text))
readStatusFile = readDB "status"
