{-# LANGUAGE OverloadedStrings #-}

module Inf1A (sendHDocChan, sendBoolChan, sendTextbookChan, sendSylChan) where

import Discord.Types
import Discord

import Utils (sendMessageChan, sendFileChan)

sendSylChan :: ChannelId -> DiscordHandler ()
sendSylChan chan = sendFileChan chan "id-smash-aristotle.png" 
                                     "./src/assets/cl/syllogisms.png"

sendBoolChan :: ChannelId -> DiscordHandler ()
sendBoolChan chan = sendFileChan chan "literally-satan.png" 
                                      "./src/assets/cl/Bool.png"

sendHDocChan :: ChannelId -> DiscordHandler ()
sendHDocChan chan = sendMessageChan chan "not yet implemented :^)"

sendTextbookChan :: ChannelId -> DiscordHandler ()
sendTextbookChan chan = sendFileChan chan "the-holy-bible-2.png" 
                                          "./src/assets/textbooks/i1a-textbook.pdf"