{-# LANGUAGE OverloadedStrings #-}

module Admin (sendGitInfo, sendGitInfoChan, gitLocal, gitRemote,
              commitsAhead, sendInstanceInfo, restartOwen, prepareStatus,
              addDevs, devIDs) where
import Data.Text as T hiding (head, tail)
import Discord.Types (ChannelId, Message(messageChannel, messageAuthor, messageId), User(userId), Channel(channelId), Snowflake)
import Discord ( DiscordHandler, RestCallErrorCode )
import UnliftIO(liftIO, retrySTM,UnliftIO (unliftIO))
import Data.Char (isSpace)
import Control.Monad (guard)
import Text.Regex.TDFA ((=~))
import Utils (sendMessageChan, sendMessageDM, isRole, captureCommandOutput, devIDs, restart, checkRoleIDs, openCSV, addToCSV)
import Status (updateStatus, editStatusFile)
import AdminRE (correctStatusRE)



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
  isDev <- checkRoleIDs m
  if isDev then do
    sendGitInfoChan $ messageChannel m
  else do
    sendMessageDM (userId $ messageAuthor m) ("Insufficient Privileges." :: T.Text)

sendGitInfoChan :: ChannelId -> DiscordHandler (Either RestCallErrorCode Message)
sendGitInfoChan chan = do
  loc <- liftIO gitLocal
  remote <- liftIO gitRemote
  commits <- liftIO commitsAhead
  sendMessageChan chan ("Git Status Info: \n" <>
                        "Local at: " <> loc <>  --as all things returned by captureCommandOutput has a newline at the end
                        "Remote at: " <> remote <>
                        "Remote is " <> rstrip commits <> " commits ahead")

sendInstanceInfo :: Message -> DiscordHandler (Either RestCallErrorCode Message)
sendInstanceInfo m = do
  isDev <- checkRoleIDs m
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
  bool <- checkRoleIDs m
  if bool then do
      sendMessageChan (messageChannel m) "Restarting"
      _ <- liftIO restart
      sendMessageChan (messageChannel m) "Failed"
  else do
    sendMessageDM (userId $ messageAuthor m) ("Insufficient privileges." :: T.Text)

addDevs :: Message -> String  -> DiscordHandler (Either RestCallErrorCode Message)
addDevs m s = do
  bool <- checkRoleIDs m
  if bool then do
    _ <- liftIO $ appendFile devIDs (show s ++ ", ")
    sendMessageChan (messageChannel m) "Success!"
  else do
    sendMessageChan (messageChannel m) "Insufficient Permissions"

-- | Checks the input against the correct version of :status
-- If incorrect, return appropriate messages
-- If correct, pass onto Status.updateStatus
prepareStatus :: Message -> T.Text -> DiscordHandler (Either RestCallErrorCode Message)
prepareStatus m text = do
    isDev <- checkRoleIDs m
    if isDev then do
        if Prelude.length captures == 3 then do
            updateStatus statusStatus statusType statusName
            liftIO $ editStatusFile (Prelude.unwords [statusStatus, statusType, statusName])
            sendMessageChan (messageChannel m) "Status updated :) Keep in mind it may take up to a minute for your client to refresh."
        else do
            sendMessageChan (messageChannel m) "Syntax: `:status <online|dnd|idle|invisible> <playing|streaming|watching|listening> <custom text...>`"
    else do
       sendMessageDM (userId $ messageAuthor m) ("Insufficient privileges." :: T.Text)

    where
        match :: (String, String, String, [String])
        match@(_, _, _, captures) = unpack text =~ correctStatusRE

        statusStatus = head captures
        statusType = (head . tail) captures
        statusName = (head . tail . tail) captures
