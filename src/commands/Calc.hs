{-# LANGUAGE OverloadedStrings #-}

module Calc (sendTextbookChan) where

import Discord.Types ( ChannelId, Message )
import Discord ( DiscordHandler, RestCallErrorCode )
import Utils (sendMessageChan, sendFileChan)

sendTextbookChan :: ChannelId -> DiscordHandler (Either RestCallErrorCode Message)
sendTextbookChan chan = sendMessageChan chan "The textbook can be found here: \n http://111.90.145.72/get.php?md5=13ecb7a2ed943dcb4a302080d2d8e6ea&key=WJUIYMKD48JZUQX2&mirr=1"
