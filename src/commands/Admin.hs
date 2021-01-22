{-# LANGUAGE OverloadedStrings #-}

module Admin (sendInstanceChan) where

import Discord.Types ( ChannelId, Message(messageChannel) )
import Discord ( DiscordHandler, RestCallErrorCode )
import Utils (sendMessageChan, isRole, captureCommandOutput)
import UnliftIO(liftIO)

sendInstanceChan :: Message -> DiscordHandler (Either RestCallErrorCode Message)
sendInstanceChan m = do 
  b1 <- isRole m "OwenDev"
  if b1
    then do
      output <- liftIO $ captureCommandOutput "git" ["rev-parse", "HEAD"]
      sendMessageChan (messageChannel m) ("Instance running on git commit: " <> output)
    else sendMessageChan (messageChannel m) "Insufficient privileges."