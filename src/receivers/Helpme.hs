{-# LANGUAGE OverloadedStrings #-}

module Helpme ( receivers ) where

import           Discord.Types      ( Message ( messageAuthor )
                                    , User ( userId )
                                    )
import           Discord            ( DiscordHandler )
import qualified Data.Text.IO as TIO
import           UnliftIO           ( liftIO )

import           Owoifier           ( owoify )
import           Utils              ( sendMessageDM )
import           Command

receivers :: [Message -> DiscordHandler ()]
receivers = [ runCommand sendHelpDM ]

-- sendHelpDM :: (MonadDiscord m) => Command m (Message -> m ())
sendHelpDM
    -- = help "Ahahaha"
    = command "helpme" $ \m -> do
        liftIO (TIO.readFile "./src/assets/help.txt")
            >>= sendMessageDM (userId $ messageAuthor m) . owoify
