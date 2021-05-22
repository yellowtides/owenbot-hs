{-# LANGUAGE OverloadedStrings #-}
module Status(updateStatusCatch, editStatusFile, setStatusFromFile) where

import           Control.Monad          ( when )
import qualified Data.Text as T
import           Discord.Internal.Types.Gateway
import           Discord                ( sendCommand
                                        , DiscordHandler
                                        )
import           UnliftIO               ( liftIO )
import           CSV                    ( readCSV
                                        , writeCSV
                                        )
import           Einmyria.Commands

-- | Convert intuitive strings into the respective DataTypes
-- Passes values onto updateStatus'
-- Although we revert to defaults if enums don't match, the caller of this function
-- should always check first on their own and provide approriate error messages.
updateStatusCatch :: (MonadDiscord m) => T.Text -> T.Text -> T.Text -> m ()
updateStatusCatch newStatus newType newName =
    updateStatus newStatusParsed newTypeParsed newName
  where
    newStatusParsed = case newStatus of
        "online"    -> UpdateStatusOnline
        "dnd"       -> UpdateStatusDoNotDisturb
        "idle"      -> UpdateStatusAwayFromKeyboard
        "invisible" -> UpdateStatusInvisibleOffline
        _           -> UpdateStatusOnline -- revert to online if not match
    newTypeParsed = case newType of
        "playing"      -> ActivityTypeGame
        "streaming"    -> ActivityTypeStreaming
        "listening to" -> ActivityTypeListening
        "competing"    -> ActivityTypeCompeting
        _              -> ActivityTypeGame -- revert to playing if not match

-- | Sets the status from file on bot launch
-- Should be called only once.
setStatusFromFile :: (MonadDiscord m) => m ()
setStatusFromFile = do
    line <- liftIO readStatusFile
    when (length line >= 3) $
        updateStatusCatch
            (head line)
            ((head.tail) line)
            (T.unwords $ (tail . tail) line)

editStatusFile :: T.Text -> T.Text -> T.Text -> IO ()
editStatusFile newStatus newType newName =
    writeCSV "status.csv" [[newStatus, newType, newName]]

readStatusFile :: IO [T.Text]
readStatusFile = do
    contents <- readCSV "status.csv"
    pure $ if null contents
        then []
        else head contents
