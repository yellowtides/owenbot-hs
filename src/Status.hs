{-# LANGUAGE OverloadedStrings #-}
module Status where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Functor           ( (<&>) )
import           Discord.Internal.Types.Gateway
import           Discord                ( sendCommand
                                        , DiscordHandler
                                        )
import           UnliftIO               ( liftIO )
import           Utils                  ( openCSV )

-- | Convert intuitive strings into the respective DataTypes
-- Passes values onto updateStatus'
-- Although we revert to defaults if enums don't match, the caller of this function
-- should always check first on their own and provide approriate error messages.
updateStatus :: T.Text -> T.Text -> T.Text -> DiscordHandler ()
updateStatus newStatus newType newName = 
    updateStatus' newStatusParsed newTypeParsed newName
  where
    newStatusParsed = case newStatus of
        "online" -> UpdateStatusOnline
        "dnd" -> UpdateStatusDoNotDisturb
        "idle" -> UpdateStatusAwayFromKeyboard
        "invisible" -> UpdateStatusInvisibleOffline
        _ -> UpdateStatusOnline -- revert to online if not match
    newTypeParsed = case newType of
        "playing" -> ActivityTypeGame
        "streaming" -> ActivityTypeStreaming
        "listening" -> ActivityTypeListening
        "competing" -> ActivityTypeCompeting
        _ -> ActivityTypeGame -- revert to playing if not match    

-- | Sets the Discord status
updateStatus' :: UpdateStatusType -> ActivityType -> T.Text -> DiscordHandler ()
updateStatus' newStatus newType newName = sendCommand $ 
    UpdateStatus (UpdateStatusOpts {
        updateStatusOptsSince = Nothing,
        updateStatusOptsGame = Just (Activity {
            activityName = newName,
            activityType = newType,
            activityUrl = Nothing
            }),
        updateStatusOptsNewStatus = newStatus,
        updateStatusOptsAFK = False 
        })

-- | Sets the status from file on bot launch
-- Should be called only once.
setStatusFromFile :: DiscordHandler ()
setStatusFromFile = do
    line <- liftIO readStatusFile
    let parts = T.words line
    updateStatus
        (head parts)
        ((head.tail) parts)
        (T.unwords $ (tail . tail) parts)

editStatusFile :: T.Text -> T.Text -> T.Text -> IO ()
editStatusFile newStatus newType newName = 
    writeFile "src/config/status.conf" $ T.unpack $ T.unwords [newStatus, newType, newName]

readStatusFile :: IO T.Text
readStatusFile = openCSV "src/config/status.conf" <&> head <&> T.pack
