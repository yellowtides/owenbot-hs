{-# LANGUAGE OverloadedStrings #-}

module Helpme ( receivers ) where
  
import           Discord.Types      ( ChannelId
                                    , Message ( messageAuthor )
                                    , User ( userId )
                                    )
import           Discord            ( DiscordHandler )
import qualified Data.Text.IO as TIO
import           UnliftIO           ( liftIO )

import           Owoifier           ( owoify )
import           Utils              ( sendMessageDM
                                    , newCommand )

receivers :: [Message -> DiscordHandler ()]
receivers = [ sendHelpDM ]

sendHelpDM :: Message -> DiscordHandler ()
sendHelpDM m = newCommand m "helpme" $ \_ ->
    liftIO (TIO.readFile "./src/assets/help.txt")
        >>= sendMessageDM (userId author) . owoify
  where
    author = messageAuthor m