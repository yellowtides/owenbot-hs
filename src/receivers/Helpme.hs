{-# LANGUAGE OverloadedStrings #-}

module Helpme where

import           Discord.Types
import           Discord            ( DiscordHandler )
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import           UnliftIO           ( liftIO )
import           Text.Parsec

import           Owoifier           ( owoify )
import           Utils              ( sendMessageDM )
import           Command

receivers :: [Message -> DiscordHandler ()]
receivers = [ sendHelpDM ]

sendHelpDM :: (MonadDiscord m, MonadIO m) => Message -> m ()
sendHelpDM
    = runCommand
    . command "helpme" $ \m ->
        liftIO (TIO.readFile "./src/assets/help.txt")
            >>= sendMessageDM (userId $ messageAuthor m) . owoify

