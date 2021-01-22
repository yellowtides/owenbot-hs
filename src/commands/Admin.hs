{-# LANGUAGE OverloadedStrings #-}

module Admin (sendInstanceChan, gitLocal, gitRemote, commitsAhead) where
import Data.Text as T
import Discord.Types ( ChannelId, Message(messageChannel) )
import Discord ( DiscordHandler, RestCallErrorCode )
import Utils (sendMessageChan, isRole, captureCommandOutput)
import UnliftIO(liftIO)

sendInstanceChan :: Message -> DiscordHandler (Either RestCallErrorCode Message)
sendInstanceChan m = do 
  b1 <- isRole m "OwenDev"
  if b1
    then do
      loc <- liftIO gitLocal
      remote <- liftIO gitRemote
      commits <- liftIO commitsAhead
      sendMessageChan (messageChannel m) ("Instance running on git commit: " <> loc <> 
                                          "\nRemote at: " <> remote <>
                                          "\nRemote is " <> commits <> " commits ahead")
    else sendMessageChan (messageChannel m) "Insufficient privileges."

gitLocal, gitRemote, commitsAhead :: IO T.Text 
gitLocal = captureCommandOutput "git" ["rev-parse", "HEAD"]
gitRemote = captureCommandOutput "git" ["rev-parse", "origin/main"]
commitsAhead = captureCommandOutput "git" ["rev-list", "--count", "HEAD", "^origin/main"]