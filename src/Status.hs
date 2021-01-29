{-# LANGUAGE OverloadedStrings #-}
module Status where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Functor ((<&>))
import Discord.Internal.Types.Gateway
import Discord (
    sendCommand,
    DiscordHandler )

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
            "watching" -> ActivityTypeWatching
            "streaming" -> ActivityTypeStreaming
            "listening" -> ActivityTypeListening
            _ -> ActivityTypeGame -- revert to playing if not match
     in
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

editStatus :: String -> IO ()
editStatus = writeFile "src/config/status.conf"

readStatus :: IO String
readStatus = openCSV "src/config/status.conf" <&> head
