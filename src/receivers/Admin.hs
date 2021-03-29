{-# LANGUAGE OverloadedStrings #-}

module Admin ( receivers, sendGitInfoChan ) where

import qualified Data.Text as T
import           Discord.Types        ( ChannelId
                                      , Message   ( messageChannel
                                                  , messageAuthor
                                                  , messageId
                                                  , messageText)
                                      , User      ( userId )
                                      , Channel   ( channelId )
                                      , Snowflake
                                      )
import           Discord              ( DiscordHandler )
import           UnliftIO             ( liftIO )
import           Data.Char            ( isSpace )
import           Control.Monad        ( when
                                      , unless )
import           Text.Regex.TDFA      ( (=~) )

import           System.Directory     ( doesPathExist )
import           System.Posix.Process ( getProcessID )

import           Owoifier             ( owoify )

import           Utils                ( newCommand
                                      , newDevCommand
                                      , sendMessageChan
                                      , sendMessageDM
                                      , captureCommandOutput
                                      , devIDs
                                      , restart
                                      , update
                                      , (=~=)
                                      )
import           Status               ( updateStatus
                                      , editStatusFile
                                      )
import           CSV                  ( readSingleColCSV
                                      , writeSingleColCSV
                                      )

receivers :: [Message -> DiscordHandler ()]
receivers =
    [ sendGitInfo
    , sendInstanceInfo
    , restartOwen
    , updateOwen
    , prepareStatus
    , listDevs
    , addDevs
    , removeDevs
    ]

rstrip :: T.Text -> T.Text
rstrip = T.reverse . T.dropWhile isSpace . T.reverse

gitLocal, gitRemote, commitsAhead, uName :: IO T.Text
gitLocal = captureCommandOutput "git rev-parse HEAD"
gitRemote = captureCommandOutput "git fetch"
  >> captureCommandOutput "git rev-parse origin/main"
commitsAhead = captureCommandOutput "git fetch"
  >> captureCommandOutput "git rev-list --count HEAD..origin/main"
uName = captureCommandOutput "uname -n"

isGitRepo :: IO Bool
isGitRepo = doesPathExist ".git"

sendGitInfo :: Message -> DiscordHandler ()
sendGitInfo m = newDevCommand m "repo" $ \_ -> do
    sendGitInfoChan $ messageChannel m

sendGitInfoChan :: ChannelId -> DiscordHandler ()
sendGitInfoChan chan = do
    inRepo <- liftIO isGitRepo
    if not inRepo then
        sendMessageChan chan "Not in git repo (sorry)!"
    else do
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
    pid <- liftIO getProcessID
    sendMessageChan chan ("Instance Info: \n" <>
                          "Host: " <> host <>
                          "Process ID: " <> T.pack (show pid))

restartOwen :: Message -> DiscordHandler ()
restartOwen m = newDevCommand m "restart" $ \_ -> do
    sendMessageChan (messageChannel m) "Restarting"
    _ <- liftIO restart
    sendMessageChan (messageChannel m) "Failed to restart"

updateOwen :: Message -> DiscordHandler ()
updateOwen m = newDevCommand m "update" $ \_ -> do
    sendMessageChan (messageChannel m) "Updating Owen"
    result <- liftIO update
    if result then
        sendMessageChan (messageChannel m)
            $ owoify "Finished update"
    else
        sendMessageChan (messageChannel m)
            $ owoify "Failed to update! Please check the logs"

listDevs :: Message -> DiscordHandler ()
listDevs m = newDevCommand m "devs" $ \_ -> do
    contents <- liftIO $ readSingleColCSV devIDs
    unless (null contents) $ do
        sendMessageChan (messageChannel m)
            $ T.intercalate "\n" contents

addDevs :: Message -> DiscordHandler ()
addDevs m = newDevCommand m "devs add ([0-9]{1,32})" $ \captures -> do
    let id = head captures
    contents <- liftIO $ readSingleColCSV devIDs
    if null contents then do
        liftIO $ writeSingleColCSV devIDs [id]
        sendMessageChan (messageChannel m) "Added!"
    else do
        liftIO $ writeSingleColCSV devIDs (id:contents)
        sendMessageChan (messageChannel m) "Added!"

removeDevs :: Message -> DiscordHandler ()
removeDevs m = newDevCommand m "devs remove ([0-9]{1,32})" $ \captures -> do
    let id = head captures
    contents <- liftIO $ readSingleColCSV devIDs
    if null contents then do
        sendMessageChan (messageChannel m) "No devs in first place :("
    else do
        liftIO $ writeSingleColCSV devIDs (filter (/= id) contents)
        sendMessageChan (messageChannel m) "Removed!"

statusRE :: T.Text
statusRE = "(online|idle|dnd|invisible) "
           <> "(playing|streaming|competing|listening to) "
           <> "(.*)"

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
            "Status updated :) Keep in mind it may take up to a minute for your client to refresh."
    else
        sendMessageChan (messageChannel m)
            "Syntax: `:status <online|dnd|idle|invisible> <playing|streaming|competing|listening to> <custom text...>`"
