{-# LANGUAGE OverloadedStrings #-}

module CommandHandler (handleCommand, isCommand) where

import qualified Discord.Requests as R
import Discord.Types
import Discord
import System.IO.Unsafe ( unsafePerformIO )
import qualified Data.ByteString as B
import qualified Data.Text as T
import Text.Regex.TDFA ( (=~) )

import OwenRegex

-- map through all the regexes and see if any of them match
isCommand :: T.Text -> Bool
isCommand m = any (m =~) commandREs 

handleCommand :: Message -> DiscordHandler (Either RestCallErrorCode Message)
handleCommand m
    | content =~ thmRE        = sendMessage channel
    | content =~ defRE        = sendMessage channel
    | content =~ lemmaRE      = sendMessage channel
    | content =~ textbookRE   = sendImage channel "Textbook.pdf" "./assets/cl/INF1A-2020-11-30.pdf"
    | content =~ syllogismsRE = sendImage channel "Id-smash-aristotle.png" "./assets/cl/syllogisms.png"
    | content =~ booleanRE    = sendMessage channel
    | content =~ hoogleInfRE  = sendMessage channel
    | content =~ helpRE       = sendMessage channel
    where
        content = messageText m
        channel = messageChannel m
            
sendMessage :: ChannelId -> DiscordHandler (Either RestCallErrorCode Message)
sendMessage c = restCall (R.CreateMessage c "Message received")


sendImage :: ChannelId -> T.Text -> FilePath -> DiscordHandler (Either RestCallErrorCode Message)
sendImage c t f= do
    restCall (R.CreateMessageUploadFile c t $ unsafeFileOpen f)

unsafeFileOpen :: FilePath -> B.ByteString
unsafeFileOpen f = unsafePerformIO $ B.readFile f