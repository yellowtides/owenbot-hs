{-# LANGUAGE OverloadedStrings #-}

module ModifyEventsChannel (commands) where

import Discord
import Discord.Requests as R
import Discord.Types
import UnliftIO (MonadIO(liftIO))

import Command
import Owoifier (owoify)
import Utils (modPerms, moveChannel, sendMessageChan, sentInServer, respond)

commands :: [Command DiscordHandler]
commands = [moveEventsChannel]

eventsChannelId :: ChannelId
eventsChannelId = 837700461192151120

-- | Move a registered events channel to the top of the server.
moveEventsChannel :: Command DiscordHandler
moveEventsChannel = requires (sentInServer <> modPerms) $ command "showEvents" $ \m ->
    do
        respond m $ owoify "Moving Events Channel."

        channel <- call $ GetChannel eventsChannelId
        case channel of
            ChannelGuildCategory _ guildId _ position _ -> do
                -- Selects the first value if the category is at the top of the channel.
                let selector     = if position == 0 then fst else snd
                    swapPermOpts = ChannelPermissionsOpts
                        { channelPermissionsOptsAllow = selector (0, 0x000000400)
                        , channelPermissionsOptsDeny  = selector (0x000000400, 0)
                        }

                -- Guild is used in place of role ID as guildID == @everyone role ID
                call $ EditChannelPermissions eventsChannelId (Left $ DiscordId $ unId guildId) swapPermOpts
                -- Shift to opposite of current location (99 is arbitrarily large to shift to the bottom)
                moveChannel guildId eventsChannelId $ selector (99, 0)

            _ -> respond m $ owoify "eventsChannelId is not a valid ChannelCategory"
