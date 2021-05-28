{-# LANGUAGE OverloadedStrings #-}

module Helpme ( receivers ) where

import           Discord.Types
import           Discord            ( DiscordHandler )
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import           UnliftIO           ( liftIO )

import           Owoifier           ( owoify )
import           Utils              ( sendMessageDM )
import           Command

receivers :: [Message -> DiscordHandler ()]
receivers = [ sendHelpDM ]

-- sendHelpDM :: (MonadDiscord m) => Command m (Message -> m ())
sendHelpDM
    = runCommand
    . requires (channelRequirement "801763198792368129")
    . command "helpme" $ \m -> do
        liftIO (TIO.readFile "./src/assets/help.txt")
            >>= sendMessageDM (userId $ messageAuthor m) . owoify

channelRequirement :: (MonadDiscord m) => String -> Message -> m (Maybe T.Text)
channelRequirement cid msg = if (messageChannel msg) == (read cid)
    then pure Nothing
    else pure $ Just "need to be in channel"

