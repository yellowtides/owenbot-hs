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

receivers :: [Message -> DiscordHandler ()]
receivers =
    [moveEventsChannel]


eventsChannelId :: ChannelId 
eventsChannelId = 837451182946517054

moveEventsChannel :: Message -> DiscordHandler ()
moveEventsChannel m = newModCommand m "showEvents" $ \_ -> do
    sendMessageChan (messageChannel m) "Moving Events Channel."

    channelObj <- restCall $ GetChannel eventsChannelId
    let Left testl = channelObj
    let Right textr = channelObj
    liftIO $ print testl
    let location = channelPosition textr
    sendMessageChan (messageChannel m) (T.pack $ show location)
    if location == 0
        then do
            moveChannel (fromJust (messageGuild m)) 99 -- Arbitrarily large number to shift to bottom
        else do
            moveChannel (fromJust (messageGuild m)) 0


    -- void $ restCall $ EditChannelPermissions 837451182946517054 0x000000400 undefined

moveChannel :: GuildId -> Int -> DiscordHandler ()
moveChannel guild location = void $ restCall $ R.ModifyGuildChannelPositions guild [(eventsChannelId, location)]