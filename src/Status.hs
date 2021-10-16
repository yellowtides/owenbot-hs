{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving
           , DeriveAnyClass #-}

module Status (writeStatusFile, setStatusFromFile, updateStatus) where

import Control.Exception.Safe (onException)
import Control.Monad (when)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import GHC.Generics
import Text.Read (readMaybe)
import UnliftIO (liftIO)

import Discord (DiscordHandler, sendCommand)
import Discord.Types

import Command
import DB

-- | Instances to allow us to read these ADTs
deriving instance Read UpdateStatusType
deriving instance Read ActivityType

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
        Nothing ->
            liftIO
                $  putStrLn
                $  "[Info] Incorrect status file format, ignoring. "
                <> "Use :status to fix this."
        Just (s, a, n) -> updateStatus s a n

-- | @writeStatusFile@ puts the status values into the status db.
writeStatusFile :: UpdateStatusType -> ActivityType -> T.Text -> IO ()
writeStatusFile status activity name =
    writeListDB (GlobalDB "status") [T.pack (show status), T.pack (show activity), name]

-- | @readStatusFile@ gets the saved status info from the db.
readStatusFile :: IO (Maybe (UpdateStatusType, ActivityType, T.Text))
readStatusFile = do
    contents <- readListDB (GlobalDB "status")
    if length contents /= 3
        then return Nothing
        else pure $ do
            let [statusS, activityS, name] = contents
            statusType   <- readMaybe $ T.unpack statusS
            activityType <- readMaybe $ T.unpack activityS
            pure (statusType, activityType, name)
