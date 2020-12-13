module EventHandler (handleEvent) where 

import Discord.Types
import Discord
import qualified Data.Text as T
import Text.Regex.TDFA
import Control.Monad (when)

import CommandHandler (handleCommand, isCommand)

isFromBot :: Message -> Bool
isFromBot m = userIsBot (messageAuthor m)

handleEvent :: Event -> DiscordHandler ()
handleEvent event = case event of
       MessageCreate m -> when (and [not       $ isFromBot m, 
                                     isCommand $ messageText m]) 
                          $ do
                                handleCommand m  
                                pure ()
       _               -> pure ()
       