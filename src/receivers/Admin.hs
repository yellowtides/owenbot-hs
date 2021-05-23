{-# LANGUAGE OverloadedStrings #-}

module Admin ( receivers, sendGitInfoChan, sendInstanceInfoChan ) where

import qualified Data.Text as T
import           Discord.Monad
import           Discord.Types          ( ChannelId
                                        , Message (..)
                                        , Channel ( ChannelText )
                                        , OverwriteId
                                        , ActivityType
                                        , UpdateStatusType
                                        )
import           Discord                ( DiscordHandler
                                        , stopDiscord, restCall
                                        )
import           Discord.Requests as R
import           UnliftIO               ( liftIO )
import           Data.Char              ( isSpace )
import Data.Maybe                       ( fromJust )
import           Control.Monad          ( unless
                                        , void
                                        )
import           Network.BSD            ( getHostName )
import           Text.Regex.TDFA        ( (=~) )

import           System.Directory       ( doesPathExist )
import           System.Posix.Process   ( getProcessID )
import qualified System.Process as Process

import           Einmyria.Commands
import           Owoifier               ( owoify )

import           Utils                  ( newDevCommand
                                        , newModCommand
                                        , sendMessageChan
                                        , captureCommandOutput
                                        , devIDs
                                        , update
                                        )
import           Status                 ( editStatusFile )
import           CSV                    ( readSingleColCSV
                                        , writeSingleColCSV
                                        )

receivers :: [Message -> DiscordHandler ()]
receivers =
    [ sendGitInfo
    , sendInstanceInfo
    , restartOwen
    , stopOwen
    , updateOwen
    , runCommand setStatus
    , listDevs
    , addDevs
    , removeDevs
    , lockdown
    , unlock
    , lockAll
    , unlockAll
    ]

rstrip :: T.Text -> T.Text
rstrip = T.reverse . T.dropWhile isSpace . T.reverse

-- captureCommandOutput appends newlines automatically
gitLocal, gitRemote, commitsAhead :: IO T.Text
gitLocal = captureCommandOutput "git rev-parse HEAD"
gitRemote = captureCommandOutput "git fetch"
  >> captureCommandOutput "git rev-parse origin/main"
commitsAhead = captureCommandOutput "git fetch"
  >> captureCommandOutput "git rev-list --count HEAD..origin/main"

isGitRepo :: IO Bool
isGitRepo = doesPathExist ".git"

sendGitInfo :: Message -> DiscordHandler ()
sendGitInfo m = newDevCommand m "repo" $ \_ ->
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
                              "Local at: "  <> loc <>
                              "Remote at: " <> remote <>
                              "Remote is "  <> rstrip commits <> " commits ahead")

sendInstanceInfo :: Message -> DiscordHandler ()
sendInstanceInfo m = newDevCommand m "instance" $ \_ ->
    sendInstanceInfoChan $ messageChannel m

sendInstanceInfoChan :: ChannelId -> DiscordHandler ()
sendInstanceInfoChan chan = do
    host <- liftIO getHostName
    pid  <- liftIO getProcessID
    sendMessageChan chan ("Instance Info: \n" <>
                          "Host: "            <> T.pack host <> "\n" <>
                          "Process ID: "      <> T.pack (show pid))

restartOwen :: Message -> DiscordHandler ()
restartOwen m = newDevCommand m "restart" $ \_ -> do
    sendMessageChan (messageChannel m) "Restarting"
    void $ liftIO $ Process.spawnCommand "owenbot-exe"
    stopDiscord

-- | Stops the entire Discord chain.
stopOwen :: Message -> DiscordHandler ()
stopOwen m = newDevCommand m "stop" $ \_ -> do
    sendMessageChan (messageChannel m) "Stopping."
    stopDiscord

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

-- DEV COMMANDS
getDevs :: IO [T.Text]
getDevs = readSingleColCSV devIDs

setDevs :: [T.Text] -> IO ()
setDevs = writeSingleColCSV devIDs

listDevs :: Message -> DiscordHandler ()
listDevs m = newDevCommand m "devs" $ \_ -> do
    contents <- liftIO getDevs
    unless (null contents) $ sendMessageChan (messageChannel m)
            $ T.intercalate "\n" contents

addDevs :: Message -> DiscordHandler ()
addDevs m = newDevCommand m "devs add ([0-9]{1,32})" $ \captures -> do
    let id = head captures
    contents <- liftIO getDevs
    if null contents then do
        liftIO $ setDevs [id]
        sendMessageChan (messageChannel m) "Added!"
    else do
        liftIO $ setDevs (id:contents)
        sendMessageChan (messageChannel m) "Added!"

removeDevs :: Message -> DiscordHandler ()
removeDevs m = newDevCommand m "devs remove ([0-9]{1,32})" $ \captures -> do
    let id = head captures
    contents <- liftIO getDevs
    if null contents then
        sendMessageChan (messageChannel m) "No devs in first place :("
    else do
        liftIO $ setDevs (filter (/= id) contents)
        sendMessageChan (messageChannel m) "Removed!"

statusRE :: T.Text
statusRE = "(online|idle|dnd|invisible) "
           <> "(playing|streaming|competing|listening to) "
           <> "(.*)"

-- | Checks the input against the correct version of :status
-- If incorrect, return appropriate messages
-- If correct, pass onto Status.updateStatus
setStatus :: (MonadDiscord m) => Einmyria (Message -> UpdateStatusType -> ActivityType -> T.Text -> m ()) m
setStatus =
    command "status"
    $ \msg newStatus newType newName -> do
        updateStatus newStatus newType newName
        liftIO $ editStatusFile newStatus newType newName
        respond msg
            "Status updated :) Keep in mind it may take up to a minute for your client to refresh."
        -- else
        --     sendMessageChan (messageChannel m) $
        --         "Syntax: `:status <online|dnd|idle|invisible> "
        --         <> "<playing|streaming|competing|listening to> "
        --         <> "<custom text...>`"

data Lock = Lockdown | Unlock deriving (Show, Eq)

lockdown :: Message -> DiscordHandler ()
lockdown m = newModCommand m "lockdown" $ \captures -> do
    let chan = messageChannel m
    eChannelObj <- restCall $ R.GetChannel chan
    case eChannelObj of
        Left err -> liftIO $ print err
        Right channel -> do
            case channel of
                ChannelText _ guild _ _ _ _ _ _ _ _ -> do
                    -- Guild is used in place of role ID as guildID == @everyone role ID
                    restCall $ lockdownChan chan guild Lockdown
                    sendMessageChan chan $ owoify "Locking Channel. To unlock use :unlock"

                _ -> do sendMessageChan (messageChannel m) $ owoify "channel is not a valid Channel"

unlock :: Message -> DiscordHandler ()
unlock m = newModCommand m "unlock" $ \captures -> do
  let chan = messageChannel m

  eChannelObj <- restCall $ R.GetChannel chan
  case eChannelObj of
    Left err -> liftIO $ print err
    Right channel -> do
      case channel of
        ChannelText _ guild _ _ _ _ _ _ _ _ -> do
          -- Guild is used in place of role ID as guildID == @everyone role ID
          restCall $ lockdownChan chan guild Unlock
          sendMessageChan chan $ owoify "Unlocking channel, GLHF!"
        _ -> do sendMessageChan (messageChannel m) $ owoify "channel is not a valid Channel (How the fuck did you pull that off?)"


lockdownChan :: ChannelId -> OverwriteId -> Lock -> ChannelRequest ()
lockdownChan chan guild b = do
    let switch  = case b of Lockdown -> fst; Unlock -> snd
    let swapPermOpts = ChannelPermissionsOpts
                            { channelPermissionsOptsAllow = switch (0, 0x0000000800)
                            , channelPermissionsOptsDeny  = switch (0x0000000800, 0)
                            , channelPermissionsOptsType  = ChannelPermissionsOptsRole
                            }
    R.EditChannelPermissions chan guild swapPermOpts


--https://discordapi.com/permissions.html#2251673153
unlockAll :: Message -> DiscordHandler ()
unlockAll m = newModCommand m "unlockAll" $ \_ -> do
    let opts = ModifyGuildRoleOpts Nothing (Just 2251673153) Nothing Nothing Nothing

    let g = fromJust $ messageGuild m
    restCall $ ModifyGuildRole g g opts
    sendMessageChan (messageChannel m) "unlocked"

-- https://discordapi.com/permissions.html#2251671105
lockAll :: Message -> DiscordHandler ()
lockAll m = newModCommand m "lockAll" $ \_ -> do
  let opts = ModifyGuildRoleOpts Nothing (Just 2251671105) Nothing Nothing Nothing

  let g = fromJust $ messageGuild m
  restCall $ ModifyGuildRole g g opts
  sendMessageChan (messageChannel m) "locked"

