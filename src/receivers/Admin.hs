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
import           Text.Regex.TDFA    ( (=~) )
import           Utils              ( newCommand
                                    , sendMessageChan
                                    , sendMessageDM
                                    , captureCommandOutput
                                    , devIDs
                                    , restart
                                    , isSenderDeveloper
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
    , prepareStatus
    , addDevs
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
    if isDev then
        sendGitInfoChan $ messageChannel m
    else
        sendMessageDM (userId $ messageAuthor m) "Insufficient Privileges."

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
    if isDev then
        sendInstanceInfoChan $ messageChannel m
    else
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
    else
        sendMessageDM (userId $ messageAuthor m) "Insufficient privileges."

addDevs :: Message -> DiscordHandler ()
addDevs m = newCommand m "addDev ([0-9]{1,32})" $ \captures -> do
    isDev <- isSenderDeveloper m
    if isDev then do
        liftIO $ putStrLn (show captures)
        let id = T.unpack $ head captures
        liftIO $ addToCSV devIDs (id ++ ", ")
        sendMessageChan (messageChannel m) "Success!"
    else
        sendMessageChan (messageChannel m) "Insufficient Permissions"

statusRE :: T.Text
statusRE = ("(online|idle|dnd|invisible) " <>
            "(playing|streaming|listening|competing) " <>
            "(.*)")

-- | Checks the input against the correct version of :status
-- If incorrect, return appropriate messages
-- If correct, pass onto Status.updateStatus
prepareStatus :: Message -> DiscordHandler ()
prepareStatus m = newCommand m "status(.*)" $ \captures -> do
    isDev <- isSenderDeveloper m
    let (_, _, _, components) = (head captures =~ statusRE) :: (T.Text, T.Text, T.Text, [T.Text])
    let newStatus = head components
    let newType = (head . tail) components
    let newName = (head . tail . tail) components
    if isDev then
        if length components == 3 then do
            updateStatus newStatus newType newName
            liftIO $ editStatusFile newStatus newType newName
            sendMessageChan (messageChannel m)
                $ "Status updated :) Keep in mind it may take up to a minute for your client to refresh."
        else
            sendMessageChan (messageChannel m) 
                $ "Syntax: `:status <online|dnd|idle|invisible> <playing|streaming|watching|listening> <custom text...>`"
    else
        sendMessageDM (userId $ messageAuthor m) "Insufficient privileges."