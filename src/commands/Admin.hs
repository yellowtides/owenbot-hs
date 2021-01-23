{-# LANGUAGE OverloadedStrings #-}

module Admin (sendGitInfo, sendGitInfoChan, gitLocal, gitRemote, commitsAhead, sendInstanceInfo, restartOwen) where
import Data.Text as T
import Discord.Types (ChannelId, Message(messageChannel, messageAuthor), User(userId), Channel(channelId))
import Discord ( DiscordHandler, RestCallErrorCode )
import Utils (sendMessageChan, sendMessageDM, isRole, captureCommandOutput, restart)
import UnliftIO(liftIO)
import Data.Char (isSpace)
import Control.Monad (guard)

rstrip :: Text -> Text
rstrip = T.reverse . T.dropWhile isSpace . T.reverse

gitLocal, gitRemote, commitsAhead, uName, pidOf :: IO T.Text 
gitLocal = captureCommandOutput "git rev-parse HEAD"
gitRemote = do
  captureCommandOutput "git fetch"
  captureCommandOutput "git rev-parse origin/main"
commitsAhead = do
  captureCommandOutput "git fetch"
  captureCommandOutput "git rev-list --count HEAD ^origin/main"
uName = captureCommandOutput "uname -n"
pidOf = captureCommandOutput "pidof owenbot-exe"

sendGitInfo :: Message -> DiscordHandler (Either RestCallErrorCode Message)
sendGitInfo m = do
  isDev <- isRole m "OwenDev"
  guard isDev
  sendGitInfoChan (messageChannel m)

sendGitInfoChan :: ChannelId -> DiscordHandler (Either RestCallErrorCode Message)
sendGitInfoChan chan = do
  loc <- liftIO gitLocal
  remote <- liftIO gitRemote
  commits <- liftIO commitsAhead
  sendMessageChan chan ("Git Status Info: \n"
                        "Local at: " <> loc <>  --as all things returned by captureCommandOutput has a newline at the end
                        "Remote at: " <> remote <>
                        "Remote is " <> rstrip commits <> " commits ahead")

sendInstanceInfo :: Message -> DiscordHandler (Either RestCallErrorCode Message)
sendInstanceInfo m = do
  isDev <- isRole m "OwenDev"
  if isDev then do
    sendInstanceInfoChan $ messageChannel m
  else do
    sendMessageDM (userId $ messageAuthor m) ("Insufficient privileges" :: T.Text)

sendInstanceInfoChan :: ChannelId -> DiscordHandler (Either RestCallErrorCode Message)
sendInstanceInfoChan chan = do
  host <- liftIO uName
  pid <- liftIO pidOf
  sendMessageChan chan ("Instance Info: \n" <>
                        "Host: " <> host <> 
                        "Process ID: " <> pid)

restartOwen :: Message -> DiscordHandler (Either RestCallErrorCode Message)
restartOwen m = do
  b1 <- isRole m "OwenDev"
  if b1
    then do
      sendMessageChan (messageChannel m) "Restarting"
      _ <- liftIO restart
      sendMessageChan (messageChannel m) "Failed"
    else sendMessageChan (messageChannel m) "Insufficient privileges."