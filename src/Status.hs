{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving
           , DeriveAnyClass
           , DeriveGeneric #-}

module Status(writeStatusFile, setStatusFromFile, updateStatus) where

import           Control.Monad          ( when )
import           Control.Exception.Safe ( onException )
import qualified Data.Text as T
import           GHC.Generics
import           Text.Read              ( readMaybe )

import           Data.Aeson             ( FromJSON
                                        , ToJSON )
import           Discord.Types
import           Discord                ( sendCommand
                                        , DiscordHandler
                                        )
import           UnliftIO               ( liftIO )

import           CSV                    ( readCSV
                                        , writeCSV
                                        )
import           Command
import           DB

-- | These datatypes in discord-haskell do not derive Read, but it's kinda
-- necessary to do @readMaybe@, so here we go:
deriving instance Read UpdateStatusType
deriving instance Read ActivityType

deriving instance Generic  UpdateStatusType
deriving instance FromJSON UpdateStatusType
deriving instance ToJSON   UpdateStatusType

deriving instance Generic  ActivityType
deriving instance FromJSON ActivityType
deriving instance ToJSON   ActivityType

-- | 'updateStatus' updates the status through the Discord gateway.
-- Therefore, it requires DiscordHandler and is not polymorphic.
updateStatus :: UpdateStatusType -> ActivityType -> T.Text -> DiscordHandler ()
updateStatus newStatus newType newName = sendCommand $
    UpdateStatus $ UpdateStatusOpts
        { updateStatusOptsSince = Nothing
        , updateStatusOptsGame = Just $ Activity
            { activityName = newName
            , activityType = newType
            , activityUrl = Nothing
            }
        , updateStatusOptsNewStatus = newStatus
        , updateStatusOptsAFK = False
        }

-- | @setStatusFromFile@ reads "status.csv" from the Config directory, and
-- reads in the 3 columns as 'UpdateStatusType', 'ActivityType', and 'T.Text'.
-- The values are used to call 'updateStatus'. 
--
-- Incorrect formats (read parse errors) are ignored.
setStatusFromFileOld :: DiscordHandler ()
setStatusFromFileOld = do
    line <- liftIO readStatusFile
    when (length line == 3) $ do
        -- Utilising the Maybe Monad whooo!
        let statusInfo = do
                statusType   <- (readMaybe . T.unpack . head) line
                activityType <- (readMaybe . T.unpack . head . tail) line
                let name = head $ tail $ tail line
                pure (statusType, activityType, name)
        case statusInfo of
            Nothing -> liftIO $ putStrLn "Incorrect status format, ignoring."
            Just (s, a, n) -> updateStatus s a n

setStatusFromFile :: DiscordHandler ()
setStatusFromFile = do
    status <- liftIO rdStatusFile
    case status of
         Nothing -> liftIO $ putStrLn "Incorrect status format, ignoring."
         Just (s, a, n) -> updateStatus s a n

-- | @editStatusFile@ puts the status values into "status.csv" by calling
-- 'show' on them and converting it to 'T.Text'.
editStatusFile :: UpdateStatusType -> ActivityType -> T.Text -> IO ()
editStatusFile newStatus newType newName =
    writeCSV "status.csv" [[T.pack (show newStatus), T.pack (show newType), newName]]

writeStatusFile :: UpdateStatusType -> ActivityType -> T.Text -> IO ()
writeStatusFile status activity name = writeDB "status" (status, activity, name)

rdStatusFile :: IO (Maybe (UpdateStatusType, ActivityType, T.Text))
rdStatusFile = readDB "status"

-- | @readStatusFile@ is a wrapper around 'readCSV' that returns only the first
-- row, if it exists.
readStatusFile :: IO [T.Text]
readStatusFile = do
    contents <- readCSV "status.csv"
    pure $ if null contents
        then []
        else head contents
