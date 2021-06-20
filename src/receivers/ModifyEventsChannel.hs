{-# LANGUAGE OverloadedStrings #-}

module ModifyEventsChannel (receivers) where

import Discord ( DiscordHandler
               , restCall )
import Discord.Types
    ( Channel(ChannelGuildCategory)
    , Message(messageChannel)
    , ChannelId )

import Discord.Requests as R
    ( ChannelPermissionsOpts(ChannelPermissionsOpts,
                             channelPermissionsOptsAllow, channelPermissionsOptsDeny,
                             channelPermissionsOptsType)
    , ChannelPermissionsOptsType ( ChannelPermissionsOptsRole )
    , ChannelRequest ( EditChannelPermissions, GetChannel ) )
import Utils ( sendMessageChan, newModCommand, moveChannel )
import UnliftIO ( MonadIO(liftIO) )
import Data.Text as T ()

import Owoifier ( owoify )

receivers :: [Message -> DiscordHandler ()]
receivers = [moveEventsChannel]

eventsChannelId :: ChannelId
eventsChannelId = 837700461192151120

-- | Move a registered events channel to the top of the server.
-- TODO: utilise Either monad
moveEventsChannel :: Message -> DiscordHandler ()
moveEventsChannel m = newModCommand m "showEvents" $ \_ -> do
    sendMessageChan (messageChannel m) $ owoify "Moving Events Channel."

    eChannelObj <- restCall $ R.GetChannel eventsChannelId
    case eChannelObj of
        Left err -> liftIO $ print err
        Right channel -> case channel of
            ChannelGuildCategory _ guild _ position _ -> do
                -- Selects the first value if the category is at the top of the channel.
                let selector = if position == 0 then fst else snd
                    swapPermOpts = ChannelPermissionsOpts
                        { channelPermissionsOptsAllow = selector (0, 0x000000400)
                        , channelPermissionsOptsDeny  = selector (0x000000400, 0)
                        , channelPermissionsOptsType  = ChannelPermissionsOptsRole
                        }

                -- Guild is used in place of role ID as guildID == @everyone role ID
                restCall $ R.EditChannelPermissions eventsChannelId guild swapPermOpts
                -- Shift to opposite of current location (99 is arbitrarily large to shift to the bottom)
                moveChannel guild eventsChannelId $ selector (99,0)

            _ -> sendMessageChan (messageChannel m)
                $ owoify "eventsChannelId is not a valid ChannelCategory"
