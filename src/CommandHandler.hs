{-# LANGUAGE OverloadedStrings #-}

module CommandHandler (handleCommand, isCommand) where

import qualified Discord.Requests as R
import Discord.Types
import Discord

import qualified Data.Text as T
import Text.Regex.TDFA

import OwenRegex

-- map through all the regexes and see if any of them match
isCommand :: T.Text -> Bool
isCommand m = any (m =~) commandREs 

handleCommand :: Message -> DiscordHandler (Either RestCallErrorCode Message)
handleCommand m
    | content =~ thmRE        = test channel
    | content =~ defRE        = test channel
    | content =~ lemmaRE      = test channel
    | content =~ textbookRE   = test channel
    | content =~ syllogismsRE = test channel
    | content =~ booleanRE    = test channel
    | content =~ hoogleInfRE  = test channel
    | content =~ helpRE       = test channel
    where
        content = messageText m
        channel = messageChannel m
            
test :: ChannelId -> DiscordHandler (Either RestCallErrorCode Message)
test c = restCall (R.CreateMessage c "Message received")