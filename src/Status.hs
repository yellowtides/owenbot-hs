{-# LANGUAGE OverloadedStrings #-}
module Status where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Functor ((<&>))
import Discord.Internal.Types.Gateway
import Discord (
    sendCommand,
    DiscordHandler )
import UnliftIO(liftIO)
import Utils (openCSV)

-- | Convert intuitive strings into the respective DataTypes
-- Passes values onto updateStatus'
-- Although we revert to defaults if enums don't match, the caller of this function
-- should always check first on their own and provide approriate error messages.
updateStatus :: String -> String -> String -> Discord.DiscordHandler ()
updateStatus statusStatus statusType statusName =
    let
        statusStatusParsed = case statusStatus of
            "online" -> UpdateStatusOnline
            "dnd" -> UpdateStatusDoNotDisturb
            "idle" -> UpdateStatusAwayFromKeyboard
            "invisible" -> UpdateStatusInvisibleOffline
            _ -> UpdateStatusOnline -- revert to online if not match
        statusTypeParsed = case statusType of
            "playing" -> ActivityTypeGame
            "streaming" -> ActivityTypeStreaming
            "listening" -> ActivityTypeListening
            "competing" -> ActivityTypeCompeting
            _ -> ActivityTypeGame -- revert to playing if not match
     in do
        updateStatus' statusStatusParsed statusTypeParsed statusName

-- | Sets the Discord status
updateStatus' :: UpdateStatusType -> ActivityType -> String -> Discord.DiscordHandler ()
updateStatus' statusStatus statusType statusName = sendCommand (UpdateStatus (UpdateStatusOpts {
    updateStatusOptsSince = Nothing,
    updateStatusOptsGame = Just (Activity {
        activityName = T.pack statusName,
        activityType = statusType,
        activityUrl = Nothing
        }),
    updateStatusOptsNewStatus = statusStatus,
    updateStatusOptsAFK = False 
    }))

-- | Sets the status from file on bot launch
-- Should be called only once.
setStatusFromFile :: Discord.DiscordHandler()
setStatusFromFile = do
    line <- liftIO readStatusFile
    let parts = Prelude.words line
    updateStatus (head parts) ((head.tail) parts) (unwords $ (tail.tail) parts)

editStatusFile :: String -> IO ()
editStatusFile = writeFile "src/config/status.conf"

readStatusFile :: IO String
readStatusFile = openCSV "src/config/status.conf" <&> head
