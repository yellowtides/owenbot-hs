{-# LANGUAGE OverloadedStrings #-}

module Inf1A (sendHDocChan, sendBoolChan, sendTextbookChan, sendSylChan) where

import qualified Discord.Requests as R
import Discord.Types
import Discord

import CommandHandler (sendMessageChan, sendFileChan)

sendSylChan :: ChannelId -> DiscordHandler (Either RestCallErrorCode Message)
sendSylChan chan = sendFileChan chan "id-smash-aristotle.png" "./src/assets/cl/syllogisms.png"

sendBoolChan :: ChannelId -> DiscordHandler (Either RestCallErrorCode Message)
sendBoolChan chan = sendFileChan chan "literally-satan.png" "./src/assets/cl/Bool.png"

sendHDocChan :: ChannelId -> DiscordHandler (Either RestCallErrorCode Message)
sendHDocChan chan = sendMessageChan chan "not yet implemented :^)"

sendTextbookChan :: ChannelId -> DiscordHandler (Either RestCallErrorCode Message)
sendBoolChan chan = sendFileChan chan "the-holy-bible-2.png" "./src/assets/textbooks/i1a-textbook.pdf"