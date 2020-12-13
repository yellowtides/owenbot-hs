{-# LANGUAGE FlexibleContexts #-}
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
isCommand m = any (T.unpack m =~) owenRegex 

handleCommand :: Message -> DiscordHandler (Either RestCallErrorCode Message)
handleCommand m = checkReg (T.unpack $ messageText m) 
        where
            channel = messageChannel m
            checkReg x
                | x =~ thmRE        = test channel
                | x =~ defRE        = test channel
                | x =~ lemmaRE      = test channel
                | x =~ textbookRE   = test channel
                | x =~ syllogismsRE = test channel
                | x =~ booleanRE    = test channel
                | x =~ hoogleInfRE  = test channel
                | x =~ helpRE       = test channel
            
test :: ChannelId -> DiscordHandler (Either RestCallErrorCode Message)
test c = restCall (R.CreateMessage c "Message received")