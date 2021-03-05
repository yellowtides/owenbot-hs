{-# LANGUAGE OverloadedStrings #-}

module Admin ( receivers, sendGitInfoChan ) where

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
                                    , newDevCommand
                                    , sendMessageChan
                                    , sendMessageDM
                                    , captureCommandOutput
                                    , devIDs
                                    , restart
                                    , (=~=)
                                    )
import           Status             ( updateStatus
                                    , editStatusFile
                                    )
import           CSV                ( readSingleColCSV
                                    , writeSingleColCSV
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
  >> captureCommandOutput "git rev-list --count origin/main..main"
uName = captureCommandOutput "uname -n"
pidOf = captureCommandOutput "pidof owenbot-exe"

sendGitInfo :: Message -> DiscordHandler ()
sendGitInfo m = newDevCommand m "repo" $ \_ -> do
    sendGitInfoChan $ messageChannel m
    
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
sendInstanceInfo m = newDevCommand m "instance" $ \_ -> do
    sendInstanceInfoChan $ messageChannel m
    
sendInstanceInfoChan :: ChannelId -> DiscordHandler ()
sendInstanceInfoChan chan = do
    host <- liftIO uName
    pid <- liftIO pidOf
    sendMessageChan chan ("Instance Info: \n" <>
                          "Host: " <> host <>
                          "Process ID: " <> pid)

restartOwen :: Message -> DiscordHandler ()
restartOwen m = newDevCommand m "restart" $ \_ -> do
    sendMessageChan (messageChannel m) "Restarting"
    _ <- liftIO restart
    sendMessageChan (messageChannel m) "Failed"
    
addDevs :: Message -> DiscordHandler ()
addDevs m = newDevCommand m "addDev ([0-9]{1,32})" $ \captures -> do
    let id = head captures
    contents <- liftIO $ readSingleColCSV devIDs
    if null contents
        then liftIO $ writeSingleColCSV devIDs [id]
        else do
            liftIO $ writeSingleColCSV devIDs (id:contents) 
            sendMessageChan (messageChannel m) "Success!"
    
statusRE :: T.Text
statusRE = ("(online|idle|dnd|invisible) " <>
            "(playing|streaming|listening|competing) " <>
            "(.*)")

-- | Checks the input against the correct version of :status
-- If incorrect, return appropriate messages
-- If correct, pass onto Status.updateStatus
prepareStatus :: Message -> DiscordHandler ()
prepareStatus m = newDevCommand m "status(.*)" $ \captures -> do
    let (_, _, _, components) = (head captures =~ statusRE) :: (T.Text, T.Text, T.Text, [T.Text])
    let newStatus = head components
    let newType = (head . tail) components
    let newName = (head . tail . tail) components
    if length components == 3 then do
        updateStatus newStatus newType newName
        liftIO $ editStatusFile newStatus newType newName
        sendMessageChan (messageChannel m)
            $ "Status updated :) Keep in mind it may take up to a minute for your client to refresh."
    else
        sendMessageChan (messageChannel m) 
            $ "Syntax: `:status <online|dnd|idle|invisible> <playing|streaming|watching|listening> <custom text...>`"