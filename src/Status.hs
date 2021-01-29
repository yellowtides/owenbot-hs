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
updateStatus :: String -> String -> String -> Discord.DiscordHandler ()
updateStatus statusName statusType statusStatus =
    let
        statusTypeParsed = case statusType of
            "playing" -> ActivityTypeGame
            "watching" -> ActivityTypeWatching
            "streaming" -> ActivityTypeStreaming
            "listening" -> ActivityTypeListening
        statusStatusParsed = case statusStatus of
            "online" -> UpdateStatusOnline
            "dnd" -> UpdateStatusDoNotDisturb
            "idle" -> UpdateStatusAwayFromKeyboard
            "invisible" -> UpdateStatusInvisibleOffline
    in
        updateStatus' statusName statusTypeParsed statusStatusParsed

-- | Sets the Discord status
updateStatus' :: String -> ActivityType -> UpdateStatusType -> Discord.DiscordHandler ()
updateStatus' statusName statusType statusStatus = sendCommand (UpdateStatus (UpdateStatusOpts {
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
