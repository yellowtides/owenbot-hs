{-# LANGUAGE OverloadedStrings #-}

module Admin ( receivers ) where

import qualified Data.Text as T
import           Discord.Types      ( ChannelId
                                    , Message   ( messageChannel
                                                , messageAuthor
                                                , messageId
                                                , messageText)
                                    , User      ( userId )
                                    , Channel   ( channelId )
                                    , Snowflake
                                    )
import           Discord            ( DiscordHandler )
import           UnliftIO           ( liftIO )
import           Data.Char          ( isSpace )
import           Control.Monad      ( when )
import           Utils              ( newCommand
                                    , sendMessageChan
                                    , sendMessageDM
                                    , isRole
                                    , captureCommandOutput
                                    , devIDs
                                    , restart
                                    , isSenderDeveloper
                                    , openCSV
                                    , addToCSV
                                    , (=~=)
                                    )
import           Status             ( updateStatus
                                    , editStatusFile
                                    )

receivers :: [Message -> DiscordHandler ()]
receivers = 
    [ sendGitInfo
    , sendInstanceInfo
    , restartOwen
    -- , addDevs
    -- , devIDs
    ]

rstrip :: T.Text -> T.Text
rstrip = T.reverse . T.dropWhile isSpace . T.reverse

gitLocal, gitRemote, commitsAhead, uName, pidOf :: IO T.Text
gitLocal = captureCommandOutput "git rev-parse HEAD"
gitRemote = captureCommandOutput "git fetch"
  >> captureCommandOutput "git rev-parse origin/main"
commitsAhead = captureCommandOutput "git fetch"
  >> captureCommandOutput "git rev-list --count HEAD ^origin/main"
uName = captureCommandOutput "uname -n"
pidOf = captureCommandOutput "pidof owenbot-exe"

sendGitInfo :: Message -> DiscordHandler ()
sendGitInfo m = newCommand m "repo" $ \_ -> do
    isDev <- isSenderDeveloper m
    if isDev 
        then sendGitInfoChan $ messageChannel m
        else sendMessageDM (userId $ messageAuthor m) "Insufficient Privileges."

sendGitInfoChan :: ChannelId -> DiscordHandler ()
sendGitInfoChan chan = do
    loc <- liftIO gitLocal
    remote <- liftIO gitRemote
    commits <- liftIO commitsAhead
    sendMessageChan chan ("Git Status Info: \n" <>
                          "Local at: " <> loc <>  --as all things returned by captureCommandOutput has a newline at the end
                          "Remote at: " <> remote <>
                          "Remote is " <> rstrip commits <> " commits ahead")

sendInstanceInfo :: Message -> DiscordHandler ()
sendInstanceInfo m = newCommand m "instance" $ \_ -> do
    isDev <- isSenderDeveloper m 
    if isDev then do
        sendInstanceInfoChan $ messageChannel m
    else do
        sendMessageDM (userId $ messageAuthor m) "Insufficient privileges"

sendInstanceInfoChan :: ChannelId -> DiscordHandler ()
sendInstanceInfoChan chan = do
    host <- liftIO uName
    pid <- liftIO pidOf
    sendMessageChan chan ("Instance Info: \n" <>
                          "Host: " <> host <>
                          "Process ID: " <> pid)

restartOwen :: Message -> DiscordHandler ()
restartOwen m = newCommand m "restart" $ \_ -> do
    isDev <- isSenderDeveloper m
    if isDev then do
        sendMessageChan (messageChannel m) "Restarting"
        _ <- liftIO restart
        sendMessageChan (messageChannel m) "Failed"
    else do
      sendMessageDM (userId $ messageAuthor m) "Insufficient privileges."

-- addDevs :: Message -> String  -> DiscordHandler (Either RestCallErrorCode Message)
-- addDevs m s = do
--   bool <- checkRoleIDs m
--   if or bool then do
--     _ <- liftIO $ appendFile devIDs (show s ++ ", ")
--     sendMessageChan (messageChannel m) "Success!"
--   else do
--     sendMessageChan (messageChannel m) "Insufficient Permissions"

-- | Checks the input against the correct version of :status
-- If incorrect, return appropriate messages
-- If correct, pass onto Status.updateStatus
-- prepareStatus :: Message -> T.Text -> DiscordHandler (Either RestCallErrorCode Message)
-- prepareStatus m text = do
--     isDev <- checkRoleIDs m
--     if or isDev then do
--         if Prelude.length captures == 3 then do
--             updateStatus statusStatus statusType statusName
--             liftIO $ editStatusFile (Prelude.unwords [statusStatus, statusType, statusName])
--             sendMessageChan (messageChannel m) "Status updated :) Keep in mind it may take up to a minute for your client to refresh."
--         else do
--             sendMessageChan (messageChannel m) "Syntax: `:status <online|dnd|idle|invisible> <playing|streaming|watching|listening> <custom text...>`"
--     else do
--        sendMessageDM (userId $ messageAuthor m) ("Insufficient privileges." :: T.Text)


-- randomOwoify :: Message -> DiscordHandler (Either RestCallErrorCode Message)
-- randomOwoify m = sendMessageChan (messageChannel m) (pingAuthorOf m <> ": " <> owoify (messageText m))