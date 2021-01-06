{-# LANGUAGE OverloadedStrings #-}

module Calc (sendTextbookChan) where

import Discord.Types ( ChannelId, Message )
import Discord ( DiscordHandler, RestCallErrorCode )
import Utils (sendMessageChan, sendFileChan)

sendTextbookChan :: ChannelId -> DiscordHandler (Either RestCallErrorCode Message)
sendTextbookChan chan = sendMessageChan chan "The textbook can be found here: \n http://libgen.gs/ads.php?md5=13ecb7a2ed943dcb4a302080d2d8e6ea"
-- sendTextbookChan chan = sendFileChan chan "Bonkulus.pdf" "./src/assets/textbooks/Calc-pdf.pdf"
