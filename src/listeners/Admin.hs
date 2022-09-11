{-# LANGUAGE OverloadedStrings #-}

module Admin (commands, sendGitInfoChan, sendInstanceInfoChan) where

import Control.Monad (join, unless, void)
import Data.Char (isSpace)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Discord
import Discord.Requests as R
import Discord.Types
import Network.BSD (getHostName)
import UnliftIO (liftIO)

import System.Directory (doesPathExist)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import qualified System.Process as Process

import Command
import Owoifier (owoify)

import Config
import Status (updateStatus, writeStatusFile)
import Utils (devPerms, modPerms, respond, sendMessageChan, sentInServer)

commands :: [Command DiscordHandler]
commands =
    [ sendGitInfo
    , sendInstanceInfo
    , restartOwen
    , stopOwen
    , updateOwen
    , upgradeOwen
    , setStatus
    , listDevs
    , lockdown
    , unlock
    , lockAll
    , unlockAll
    ]

rstrip :: T.Text -> T.Text
rstrip = T.reverse . T.dropWhile isSpace . T.reverse

gitInfo :: FilePath -> IO T.Text
gitInfo dir = do
    let args =
            [ "log"
            , "-n"
            , "1"
            , "--format=format:`%h` %ar by **%an**: %s"
            , "--date=short"
            , "HEAD"
            ]
    T.pack <$> Process.readCreateProcess
        ((Process.proc "git" args) { Process.cwd = Just dir })
        ""

gitFetch :: FilePath -> IO T.Text
gitFetch dir = T.pack <$> Process.readCreateProcess
    ((Process.proc "git" ["fetch"]) { Process.cwd = Just dir })
    ""

commitsAhead :: FilePath -> IO T.Text
commitsAhead dir = T.pack <$> Process.readCreateProcess
    ((Process.proc "git" ["rev-list", "--count", "HEAD..origin/main"])
        { Process.cwd = Just dir
        }
    )
    ""

-- | Sends the git info to a specific channel
sendGitInfoChan :: ChannelId -> DiscordHandler ()
sendGitInfoChan chan = do
    dir <- liftIO $ owenConfigRepoDir <$> readConfig
    case dir of
        Nothing  -> sendMessageChan chan $ owoify "No git repo specified in the config!"
        Just dir -> do
            localStatus <- liftIO $ gitInfo dir
            liftIO $ gitFetch dir
            commits <- liftIO $ commitsAhead dir
            -- git always has echoes an empty line at the end, so no need
            case rstrip commits of
                "0" -> sendMessageChan chan $ "*Git: All caught up!* \n" <> localStatus
                x ->
                    sendMessageChan chan
                        $  "*Git: Upstream is "
                        <> x
                        <> " commits ahead. Current local HEAD is:* \n"
                        <> localStatus

-- | Responds to a message with the git info
sendGitInfo :: Command DiscordHandler
sendGitInfo =
    requires devPerms . command "repo" $ \m -> sendGitInfoChan $ messageChannelId m

-- | Sends process instance info to a given channel
sendInstanceInfoChan :: ChannelId -> DiscordHandler ()
sendInstanceInfoChan chan = do
    host <- liftIO getHostName
    pid  <- liftIO Process.getCurrentPid
    sendMessageChan
        chan
        (  "Instance Info: \n"
        <> "Host: "
        <> T.pack host
        <> "\n"
        <> "Process ID: "
        <> T.pack (show pid)
        )

-- | Responds to a message with the instance info
sendInstanceInfo :: Command DiscordHandler
sendInstanceInfo = requires devPerms . command "instance" $ \m ->
    sendInstanceInfoChan $ messageChannelId m

-- | Kills the bot and spawns a new process.
-- The new process is spawned in the same cwd (at least on linux)
restartOwen :: Command DiscordHandler
restartOwen =
    requires devPerms
        . help "Kills the bot and starts a new instance."
        . command "restart"
        $ \m -> do
            respond m "Restarting..."
            void $ liftIO $ Process.spawnCommand "owenbot-exe"
            stopDiscord

-- | Safely kills the bot without restarting it.
-- Any in-progress actions (file writing) should complete, but may not respond
-- to their calling message.
stopOwen :: Command DiscordHandler
stopOwen = requires devPerms . command "stop" $ \m -> do
    respond m "Stopping..."
    stopDiscord

-- | Updates the git repo, compiles, and installs the new binary.
-- Must be running in a git repo. This is currently hard-coded, and we don't
-- check if it's the correct repo. Since this is for testing, it's fine.
updateOwen :: Command DiscordHandler
updateOwen =
    requires devPerms
        . help
            (  "Updates the bot to the latest git commit and compiles."
            <> "The bot must be running from `~/owenbot-hs`"
            )
        . command "update"
        $ \m -> do
            respond m "Updating..."
            result <- liftIO update
            respond m $ owoify $ case result of
                ExitSuccess   -> "Finished update"
                ExitFailure _ -> "Failed to update! Please check the logs"

-- | `update` calls a shell script that updates the bot's repo
update :: IO ExitCode
update = do
    dir <- owenConfigRepoDir <$> readConfig
    case dir of
        Nothing   -> return $ ExitFailure 0
        Just path -> Process.waitForProcess =<< Process.spawnCommand
            ("cd " <> path <> " && git reset --hard @{u} && git pull && stack install")

-- | Simple hack to run :update and :restart in one go.
upgradeOwen :: Command DiscordHandler
upgradeOwen =
    requires devPerms
        . help "Updates and restarts the bot in one go."
        . command "upgrade"
        $ \m -> do
            runCommand updateOwen $ m { messageContent = ":update" }
            runCommand restartOwen $ m { messageContent = ":restart" }

------- DEV COMMANDS
-- | Gets the list of developer role IDs
getDevs :: IO [T.Text]
getDevs = owenConfigDevs <$> readConfig

-- | Get all the devs.
listDevs :: Command DiscordHandler
listDevs =
    help
            (  "List registered developer role IDs. Edit the config and "
            <> "restart the bot if you want to change this."
            )
        . command "listDevs"
        $ \m -> do
            contents <- liftIO getDevs
            unless (null contents) $ respond m $ T.intercalate "\n" contents

-- | This can't be polymorphic because updateStatus requires gateway specific
-- things.
setStatus :: Command DiscordHandler
setStatus =
    requires devPerms
        . command "status"
        $ \msg newStatus newType (Remaining newName) -> do
            updateStatus newStatus newType newName
            liftIO $ writeStatusFile newStatus newType newName
            respond
                msg
                (  "Status updated :) Keep in mind it may take up to a "
                <> "minute for your client to refresh."
                )

------ LOCKDOWN COMMANDS
data Lock = Lockdown | Unlock deriving (Show, Eq)

-- | Locks @everyone-level posting permissions in a given channel
lockdown :: Command DiscordHandler
lockdown =
    requires (sentInServer <> modPerms)
        . help
            (  "`:lockdown` locks posting permissions for the `everyone` role in "
            <> "this channel."
            )
        . command "lockdown"
        $ \m -> do
            let chan = messageChannelId m
            channel <- call $ GetChannel (messageChannelId m)
            case channel of
                ChannelText _ guildId _ _ _ _ _ _ _ _ -> do
                    -- Guild is used in place of role ID as guildID == @everyone rID
                    lockdownChan chan (Left $ DiscordId $ unId guildId) Lockdown
                    respond m $ owoify "Locking Channel. To unlock use :unlock"

                _ -> respond m $ owoify "Channel is not a valid Channel"

-- | Unlocks a locked channel.
unlock :: Command DiscordHandler
unlock =
    requires (sentInServer <> modPerms)
        . help "`:unlock` reverses a `:lockdown`."
        . command "unlock"
        $ \m -> do
            let chan = messageChannelId m
            channel <- call $ GetChannel chan
            case channel of
                ChannelText _ guildId _ _ _ _ _ _ _ _ -> do
                    -- Guild is used in place of role ID as guildID == @everyone rID
                    lockdownChan chan (Left $ DiscordId $ unId guildId) Unlock
                    respond m $ owoify "Unlocking channel, GLHF!"
                _ -> do
                    respond m
                        $ owoify
                            "channel is not a valid Channel (How the fuck did you pull that off?)"

-- | Toggles the locking of a specified channel
lockdownChan :: ChannelId -> Either RoleId UserId -> Lock -> DiscordHandler ()
lockdownChan chan overwriteId b = do
    let switch = case b of
            Lockdown -> fst
            Unlock   -> snd
    let swapPermOpts = ChannelPermissionsOpts
            { channelPermissionsOptsAllow = switch (0, 0x0000000800)
            , channelPermissionsOptsDeny  = switch (0x0000000800, 0)
            }
    call $ EditChannelPermissions chan overwriteId swapPermOpts

-- | Locks every channel in a Guild
-- https://discordapi.com/permissions.html#2251673153
unlockAll :: Command DiscordHandler
unlockAll =
    requires (sentInServer <> modPerms)
        . help "`:unlock` for every channel"
        . command "unlockAll"
        $ \m -> do
            let
                opts = ModifyGuildRoleOpts
                    Nothing
                    (Just "2251673153")
                    Nothing
                    Nothing
                    Nothing

            let guildId = fromJust $ messageGuildId m
            call $ ModifyGuildRole guildId (DiscordId $ unId guildId) opts
            respond m "unlocked"

-- | Unlocks every channel.
-- https://discordapi.com/permissions.html#2251671105
lockAll :: Command DiscordHandler
lockAll =
    requires (sentInServer <> modPerms)
        . help "`:lockdown` for every channel"
        . command "lockAll"
        $ \m -> do
            let
                opts = ModifyGuildRoleOpts
                    Nothing
                    (Just "2251671105")
                    Nothing
                    Nothing
                    Nothing

            let guildId = fromJust $ messageGuildId m
            call $ ModifyGuildRole guildId (DiscordId $ unId guildId) opts
            respond m "locked"
