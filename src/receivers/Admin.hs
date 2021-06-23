{-# LANGUAGE OverloadedStrings #-}

module Admin ( receivers, sendGitInfoChan, sendInstanceInfoChan ) where

import qualified Data.Text as T
import           Discord.Types
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

import           Command
import           Owoifier               ( owoify )

import           Utils                  ( newDevCommand
                                        , newModCommand
                                        , sendMessageChan
                                        , captureCommandOutput
                                        , devIDs
                                        , update
                                        , developerRequirement
                                        , roleNameRequirement
                                        )
import           Status                 ( editStatusFile
                                        , updateStatus
                                        )
import           CSV                    ( readSingleColCSV
                                        , writeSingleColCSV
                                        )

receivers :: [Message -> DiscordHandler ()]
receivers = fmap runCommand
    [ sendGitInfo
    , sendInstanceInfo
    , restartOwen
    , stopOwen
    , updateOwen
    , setStatus
    , someComplexThing
    , devs
    , lockdown
    , unlock
    , lockAll
    , unlockAll
    ]

rstrip :: T.Text -> T.Text
rstrip = T.reverse . T.dropWhile isSpace . T.reverse

gitLocal :: IO T.Text
gitLocal = do
    -- can't use captureCommandOutput because --format would be broken into
    -- separate args (as it splits on space).
    let args = [ "log"
               , "-n", "1"
               , "--format=format:`%h` %ar by **%an**: %s"
               , "--date=short"
               , "HEAD"
               ]
    T.pack <$> Process.readCreateProcess
        ((Process.proc "git" args) { Process.cwd = Just "." }) ""
commitsAhead :: IO T.Text
commitsAhead = captureCommandOutput "git rev-list --count HEAD..origin/main"

isGitRepo :: IO Bool
isGitRepo = doesPathExist ".git"

sendGitInfo :: Command DiscordHandler
sendGitInfo
    = requires developerRequirement
    . command "repo" $ \m ->
        sendGitInfoChan $ messageChannel m

sendGitInfoChan :: (MonadDiscord m, MonadIO m) => ChannelId -> m ()
sendGitInfoChan chan = do
    inRepo <- liftIO isGitRepo
    if not inRepo then
        sendMessageChan chan "Not in git repo (sorry)!"
    else do
        localStatus <- liftIO gitLocal
        liftIO $ captureCommandOutput "git fetch"
        commits <- liftIO commitsAhead
        -- git always has echoes an empty line at the end, so no need
        case rstrip commits of
            "0" -> sendMessageChan chan $ "*Git: All caught up!* \n" <> localStatus
            x   -> sendMessageChan chan $ "*Git: Upstream is " <> x <>
                " commits ahead.* \n" <> localStatus

sendInstanceInfo :: Command DiscordHandler
sendInstanceInfo
  = requires developerRequirement
  . command "instance" $ \m -> 
        sendInstanceInfoChan $ messageChannel m

sendInstanceInfoChan :: (MonadDiscord m, MonadIO m) => ChannelId -> m ()
sendInstanceInfoChan chan = do
    host <- liftIO getHostName
    pid  <- liftIO getProcessID
    sendMessageChan chan ("Instance Info: \n" <>
                          "Host: "            <> T.pack host <> "\n" <>
                          "Process ID: "      <> T.pack (show pid))

restartOwen :: Command DiscordHandler
restartOwen
  = requires developerRequirement
  . command "restart" $ \m -> do
        sendMessageChan (messageChannel m) "Restarting"
        void $ liftIO $ Process.spawnCommand "owenbot-exe"
        stopDiscord

-- | Stops the entire Discord chain.
stopOwen :: Command DiscordHandler
stopOwen
  = requires developerRequirement
  . command "stop" $ \m -> do
        sendMessageChan (messageChannel m) "Stopping."
        stopDiscord

updateOwen :: Command DiscordHandler
updateOwen
  = requires developerRequirement
  . command "update" $ \m -> do
        respond m "Updating Owen"
        result <- liftIO update
        if result then
            respond m $ owoify "Finished update"
        else
            respond m $ owoify "Failed to update! Please check the logs"

-- DEV COMMANDS
getDevs :: IO [T.Text]
getDevs = readSingleColCSV devIDs

setDevs :: [T.Text] -> IO ()
setDevs = writeSingleColCSV devIDs

devs :: Command DiscordHandler
devs
    = requires developerRequirement
    . help "List/add/remove registered developer role IDs"
    . command "devs" $ \m maybeActionValue -> do
        contents <- liftIO getDevs
        case maybeActionValue :: Maybe (T.Text, RoleId) of
            Nothing -> do
                unless (null contents) $
                    respond m $ T.intercalate "\n" contents
            Just ("add", roleId) -> do
                liftIO $ setDevs (T.pack (show roleId):contents)
                respond m "Added!"
            Just ("remove", roleId) -> do
                liftIO $ setDevs (filter (/= T.pack (show roleId)) contents)
                respond m "Removed!"
            Just _ -> respond m "Usage: `:devs {add|remove} <roleId>"

statusRE :: T.Text
statusRE = "(online|idle|dnd|invisible) "
           <> "(playing|streaming|competing|listening to) "
           <> "(.*)"

-- | This can't be polymoprhic because updateStatus requires gateway specific
-- things.
setStatus :: Command DiscordHandler
setStatus
    = command "status"
    $ \msg newStatus newType (Remaining newName) -> do
        updateStatus newStatus newType newName
        liftIO $ editStatusFile newStatus newType newName
        respond msg
            "Status updated :) Keep in mind it may take up to a minute for your client to refresh."

someComplexThing :: (MonadDiscord m) => Command m
someComplexThing
    = command "complex"
    $ \msg words -> do
        respond msg $
            "Length: " <> (T.pack . show . length) words <> "\n" <>
                "Caught items: \n" <> T.intercalate "\n" words


data Lock = Lockdown | Unlock deriving (Show, Eq)

lockdown :: Command DiscordHandler
lockdown
    = requires (roleNameRequirement ["Mod", "Moderator"])
    . command "lockdown" $ \m -> do
        let chan = messageChannel m
        channel <- getChannel chan
        case channel of
            ChannelText _ guild _ _ _ _ _ _ _ _ -> do
                -- Guild is used in place of role ID as guildID == @everyone role ID
                lockdownChan chan guild Lockdown
                respond m $ owoify "Locking Channel. To unlock use :unlock"

            _ -> do respond m $ owoify "channel is not a valid Channel"

unlock :: Command DiscordHandler
unlock
  = requires (roleNameRequirement ["Mod", "Moderator"])
  . command "unlock" $ \m -> do
      let chan = messageChannel m
      channel <- getChannel chan
      case channel of
          ChannelText _ guild _ _ _ _ _ _ _ _ -> do
              -- Guild is used in place of role ID as guildID == @everyone role ID
              lockdownChan chan guild Unlock
              respond m $ owoify "Unlocking channel, GLHF!"
          _ -> do respond m $ owoify "channel is not a valid Channel (How the fuck did you pull that off?)"


lockdownChan :: (MonadDiscord m) => ChannelId -> OverwriteId -> Lock -> m ()
lockdownChan chan guild b = do
    let switch  = case b of Lockdown -> fst; Unlock -> snd
    let swapPermOpts = ChannelPermissionsOpts
                            { channelPermissionsOptsAllow = switch (0, 0x0000000800)
                            , channelPermissionsOptsDeny  = switch (0x0000000800, 0)
                            , channelPermissionsOptsType  = ChannelPermissionsOptsRole
                            }
    editChannelPermissions chan guild swapPermOpts


--https://discordapi.com/permissions.html#2251673153
unlockAll :: Command DiscordHandler
unlockAll
    = requires (roleNameRequirement ["Mod", "Moderator"])
    . command "unlockAll" $ \m -> do
        let opts = ModifyGuildRoleOpts Nothing (Just 2251673153) Nothing Nothing Nothing

        let g = fromJust $ messageGuild m
        modifyGuildRole g g opts
        respond m "unlocked"

-- https://discordapi.com/permissions.html#2251671105
lockAll :: Command DiscordHandler
lockAll
    = requires (roleNameRequirement ["Mod", "Moderator"])
    . command "lockAll" $ \m -> do
        let opts = ModifyGuildRoleOpts Nothing (Just 2251671105) Nothing Nothing Nothing

        let g = fromJust $ messageGuild m
        modifyGuildRole g g opts
        respond m "locked"

