{-# LANGUAGE OverloadedStrings #-}

module ModifyEventsChannel (receivers) where
import Discord
import Discord.Types
import Discord.Internal.Rest.Channel
import Discord.Requests as R
import Utils
import Control.Monad ( void )
import UnliftIO
import Data.Maybe ( fromJust )
import Data.Text as T
import GHC.Word ( Word64 )

import Owoifier ( owoify )

receivers :: [Message -> DiscordHandler ()]
receivers =
    [moveEventsChannel]


eventsChannelId :: GuildId   
eventsChannelId = 837700461192151120


-- | Move a registered events channel to the top of the server.
moveEventsChannel :: Message -> DiscordHandler ()
moveEventsChannel m = newModCommand m "showEvents" $ \_ -> do
    sendMessageChan (messageChannel m) $ owoify "Moving Events Channel."

    mbchannelObj <- restCall $ GetChannel eventsChannelId
    case mbchannelObj of 
        Left err -> liftIO $ print err
        Right mbchannelObj -> do
            case mbchannelObj of
                ChannelGuildCategory _ guild _ position _ -> do
                    if position == 0
                        then do
                            -- Guild is used in place of role ID as guildID == @everyone role ID
                            restCall $ EditChannelPermissions eventsChannelId guild ChannelPermissionsOpts {channelPermissionsOptsAllow = 0, channelPermissionsOptsDeny = 0x000000400, channelPermissionsOptsType = ChannelPermissionsOptsRole}
                            moveChannel guild eventsChannelId 99 -- Arbitrarily large number to shift to bottom
                        else do
                            restCall $ EditChannelPermissions eventsChannelId guild ChannelPermissionsOpts {channelPermissionsOptsAllow = 0x000000400, channelPermissionsOptsDeny = 0, channelPermissionsOptsType = ChannelPermissionsOptsRole}
                            moveChannel guild eventsChannelId 0
                _ -> do sendMessageChan (messageChannel m) $ owoify "eventsChannelId is not a valid ChannelCategory"

