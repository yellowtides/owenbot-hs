{-# LANGUAGE OverloadedStrings #-}

module CALC ( sendTextbookChan) where

import Discord.Types ( ChannelId, Message )
import Discord ( DiscordHandler, RestCallErrorCode )
import Utils (sendMessageChan, sendFileChan)

sendTextbookChan :: ChannelId -> DiscordHandler (Either RestCallErrorCode Message)
sendTextbookChan chan = sendFileChan chan "calc-textbook.pdf" "./src/assets/textbooks/Calc-textbook.pdf"
