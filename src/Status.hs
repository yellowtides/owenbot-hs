{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
module Status(editStatusFile, setStatusFromFile, updateStatus) where

import           Control.Monad          ( when )
import           Control.Exception.Safe ( onException )
import qualified Data.Text as T
import           Text.Read              ( readMaybe )

import           Discord.Types
import           Discord                ( sendCommand
                                        , DiscordHandler
                                        )
import           UnliftIO               ( liftIO )
import           CSV                    ( readCSV
                                        , writeCSV
                                        )
import           Command

-- | These datatypes in discord-haskell do not derive Read, but it's kinda
-- necessary to do @readMaybe@, so here we go:
deriving instance Read UpdateStatusType
deriving instance Read ActivityType

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
setStatusFromFile :: DiscordHandler ()
setStatusFromFile = do
    line <- liftIO readStatusFile
    when (length line >= 3) $ do
        -- Utilising the Maybe Monad whooo!
        let mbStuff = do
                statusType <- (readMaybe . T.unpack . head) line
                activityType <- (readMaybe . T.unpack . head . tail) line
                let name = T.unwords $ (tail . tail) line
                pure (statusType, activityType, name)
        case mbStuff of
            Nothing -> liftIO $ putStrLn "Incorrect status format, ignoring."
            Just (s, a, n) -> updateStatus s a n

-- | @editStatusFile@ puts the status values into "status.csv" by calling
-- 'show' on them and converting it to 'T.Text'.
editStatusFile :: UpdateStatusType -> ActivityType -> T.Text -> IO ()
editStatusFile newStatus newType newName =
    writeCSV "status.csv" [[T.pack (show newStatus), T.pack (show newType), newName]]

-- | @readStatusFile@ is a wrapper around 'readCSV' that returns only the first
-- row, if it exists.
readStatusFile :: IO [T.Text]
readStatusFile = do
    contents <- readCSV "status.csv"
    pure $ if null contents
        then []
        else head contents
