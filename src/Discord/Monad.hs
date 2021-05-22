{-# LANGUAGE FlexibleInstances #-} -- allow instance declaration of MonadDiscord
module Discord.Monad
    ( MonadDiscord(..)
    ) where

import           Control.Exception.Safe     ( MonadThrow
                                            , SomeException
                                            , throwM
                                            )
import           Control.Monad.IO.Class     ( MonadIO )
import           Control.Monad              ( void )
import qualified Data.ByteString as B
import qualified Data.Text as T
import           Discord.Internal.Rest.Prelude
                                            ( Request )
import qualified Discord.Requests as R
import           Discord.Types
import           Discord

import           Einmyria.Error             ( EinmyriaError(..) )

-- | Monads that can interact with Discord.
class (Monad m, MonadThrow m, MonadIO m) => MonadDiscord m where
    -- Channels
    getChannel :: ChannelId -> m Channel
    modifyChannel :: ChannelId -> R.ModifyChannelOpts -> m Channel
    deleteChannel :: ChannelId -> m Channel
    getChannelMessages :: ChannelId -> (Int, R.MessageTiming) -> m [Message]
    getChannelMessage :: (ChannelId, MessageId) -> m Message
    createMessage :: ChannelId -> T.Text -> m Message
    createMessageEmbed :: ChannelId -> T.Text -> CreateEmbed -> m Message
    createMessageUploadFile :: ChannelId -> T.Text -> B.ByteString -> m Message
    createMessageDetailed :: ChannelId -> R.MessageDetailedOpts -> m Message
    createReaction :: (ChannelId, MessageId) -> T.Text -> m ()
    deleteOwnReaction :: (ChannelId, MessageId) -> T.Text -> m ()
    deleteUserReaction :: (ChannelId, MessageId) -> UserId -> T.Text -> m ()
    deleteSingleReaction :: (ChannelId, MessageId) -> T.Text -> m ()
    getReactions :: (ChannelId, MessageId) -> T.Text -> (Int, R.ReactionTiming) -> m [User]
    deleteAllReactions :: (ChannelId, MessageId) -> m ()
    editMessage :: (ChannelId, MessageId) -> T.Text -> Maybe CreateEmbed -> m Message
    deleteMessage :: (ChannelId, MessageId) -> m ()
    bulkDeleteMessage :: (ChannelId, [MessageId]) -> m ()
    editChannelPermissions :: ChannelId -> OverwriteId -> R.ChannelPermissionsOpts -> m ()
    getChannelInvites  :: ChannelId -> m Object
    createChannelInvite :: ChannelId -> R.ChannelInviteOpts -> m Invite
    deleteChannelPermission :: ChannelId -> OverwriteId -> m ()
    triggerTypingIndicator :: ChannelId -> m ()
    getPinnedMessages :: ChannelId -> m [Message]
    addPinnedMessage :: (ChannelId, MessageId) -> m ()
    deletePinnedMessage :: (ChannelId, MessageId) -> m ()
    groupDMAddRecipient :: ChannelId -> R.GroupDMAddRecipientOpts -> m ()
    groupDMRemoveRecipient :: ChannelId -> UserId -> m ()
    -- Emojis
    listGuildEmojis :: GuildId -> m [Emoji]
    getGuildEmoji :: GuildId -> EmojiId -> m Emoji
    createGuildEmoji :: GuildId -> T.Text -> B.ByteString -> m Emoji
    modifyGuildEmoji :: GuildId -> EmojiId -> R.ModifyGuildEmojiOpts -> m Emoji
    deleteGuildEmoji :: GuildId -> EmojiId -> m ()
    -- Guilds
    createGuild :: R.CreateGuildOpts -> m Guild
    getGuild :: GuildId -> m Guild
    modifyGuild :: GuildId -> R.ModifyGuildOpts -> m Guild
    deleteGuild :: GuildId -> m ()
    getGuildChannels :: GuildId -> m [Channel]
    createGuildChannel :: GuildId -> T.Text -> [Overwrite] -> R.CreateGuildChannelOpts -> m Channel
    modifyGuildChannelPositions :: GuildId -> [(ChannelId, Int)] -> m [Channel]
    getGuildMember :: GuildId -> UserId -> m GuildMember
    listGuildMembers :: GuildId -> R.GuildMembersTiming -> m [GuildMember]
    addGuildMember :: GuildId -> UserId -> R.AddGuildMemberOpts -> m ()
    modifyGuildMember :: GuildId -> UserId -> R.ModifyGuildMemberOpts -> m ()
    modifyCurrentUserNick :: GuildId -> T.Text -> m ()
    addGuildMemberRole :: GuildId -> UserId -> RoleId -> m ()
    removeGuildMemberRole :: GuildId -> UserId -> RoleId -> m ()
    removeGuildMember :: GuildId -> UserId -> m ()
    getGuildBans :: GuildId -> m [GuildBan]
    getGuildBan :: GuildId -> UserId -> m GuildBan
    createGuildBan :: GuildId -> UserId -> R.CreateGuildBanOpts -> m ()
    removeGuildBan :: GuildId -> UserId -> m ()
    getGuildRoles :: GuildId -> m [Role]
    createGuildRole :: GuildId -> R.ModifyGuildRoleOpts -> m Role
    modifyGuildRolePositions :: GuildId -> [(RoleId, Integer)] -> m [Role]
    modifyGuildRole :: GuildId -> RoleId -> R.ModifyGuildRoleOpts -> m Role
    deleteGuildRole :: GuildId -> RoleId -> m ()
    getGuildPruneCount :: GuildId -> Integer -> m Object
    beginGuildPrune :: GuildId -> Integer -> m Object
    getGuildVoiceRegions :: GuildId -> m [VoiceRegion]
    getGuildInvites :: GuildId -> m [Invite]
    getGuildIntegrations :: GuildId -> m [Integration]
    createGuildIntegration :: GuildId -> IntegrationId -> R.CreateGuildIntegrationOpts -> m ()
    modifyGuildIntegration :: GuildId -> IntegrationId -> R.ModifyGuildIntegrationOpts -> m ()
    deleteGuildIntegration :: GuildId -> IntegrationId -> m ()
    syncGuildIntegration :: GuildId -> IntegrationId -> m ()
    getGuildEmbed :: GuildId -> m GuildEmbed
    modifyGuildEmbed :: GuildId -> GuildEmbed -> m GuildEmbed
    getGuildVanityURL :: GuildId -> m T.Text
    -- Invites
    getInvite :: T.Text -> m Invite
    deleteInvite :: T.Text -> m Invite
    -- Users
    getCurrentUser :: m User
    getUser :: UserId -> m User
    modifyCurrentUser :: T.Text -> R.CurrentUserAvatar -> m User
    getCurrentUserGuilds :: m [PartialGuild]
    leaveGuild :: GuildId -> m ()
    getUserDMs :: m [Channel]
    createDM :: UserId -> m Channel
    getUserConnections :: m [ConnectionObject]
    -- Voice
    listVoiceRegions :: m [VoiceRegion]
    -- Webhooks
    createWebhook :: ChannelId -> R.CreateWebhookOpts -> m Webhook
    getChannelWebhooks :: ChannelId -> m [Webhook]
    getGuildWebhooks :: GuildId -> m [Webhook]
    getWebhook :: WebhookId -> m Webhook
    getWebhookWithToken :: WebhookId -> T.Text -> m Webhook
    modifyWebhook :: WebhookId -> R.ModifyWebhookOpts -> m Webhook
    modifyWebhookWithToken :: WebhookId -> T.Text -> R.ModifyWebhookOpts -> m Webhook
    deleteWebhook :: WebhookId -> m ()
    deleteWebhookWithToken :: WebhookId -> T.Text -> m ()
    executeWebhookWithToken :: WebhookId -> T.Text -> R.ExecuteWebhookWithTokenOpts -> m ()

    -- Gateway
    updateStatus :: UpdateStatusType -> ActivityType -> T.Text -> m ()

    -- Custom utilities
    respond       :: Message -> T.Text -> m ()
    reply         :: Message -> Bool -> T.Text -> m ()

-- | Implements every single possible rest call as a wrapper function.
-- Convenient notation achieved using a point-free restCallAndHandle
instance MonadDiscord DiscordHandler where
    getChannel = restCallAndHandle . R.GetChannel
    modifyChannel = (restCallAndHandle .) . R.ModifyChannel
    deleteChannel = restCallAndHandle . R.DeleteChannel
    getChannelMessages = (restCallAndHandle .) . R.GetChannelMessages
    getChannelMessage = restCallAndHandle . R.GetChannelMessage
    createMessage = (restCallAndHandle .) . R.CreateMessage
    createMessageEmbed = ((restCallAndHandle .) .) . R.CreateMessageEmbed
    createMessageUploadFile = ((restCallAndHandle .) .) . R.CreateMessageUploadFile
    createMessageDetailed = (restCallAndHandle .) . R.CreateMessageDetailed
    createReaction = (restCallAndHandle .) . R.CreateReaction
    deleteOwnReaction = (restCallAndHandle .) . R.DeleteOwnReaction
    deleteUserReaction = ((restCallAndHandle .) .) . R.DeleteUserReaction
    deleteSingleReaction = (restCallAndHandle .) . R.DeleteSingleReaction
    getReactions = ((restCallAndHandle .) .) . R.GetReactions
    -- getReactions = restCallAndHandle <=< R.GetReactions
    deleteAllReactions = restCallAndHandle . R.DeleteAllReactions
    editMessage = ((restCallAndHandle .) .) . R.EditMessage
    deleteMessage = restCallAndHandle . R.DeleteMessage
    bulkDeleteMessage = restCallAndHandle . R.BulkDeleteMessage
    editChannelPermissions = ((restCallAndHandle .) .) . R.EditChannelPermissions
    getChannelInvites = restCallAndHandle . R.GetChannelInvites
    createChannelInvite = (restCallAndHandle .) . R.CreateChannelInvite
    deleteChannelPermission = (restCallAndHandle .) . R.DeleteChannelPermission
    triggerTypingIndicator = restCallAndHandle . R.TriggerTypingIndicator
    getPinnedMessages = restCallAndHandle . R.GetPinnedMessages
    addPinnedMessage = restCallAndHandle . R.AddPinnedMessage
    deletePinnedMessage = restCallAndHandle . R.DeletePinnedMessage
    groupDMAddRecipient = (restCallAndHandle .) . R.GroupDMAddRecipient
    groupDMRemoveRecipient = (restCallAndHandle .) . R.GroupDMRemoveRecipient
    -- Emojis
    listGuildEmojis = restCallAndHandle . R.ListGuildEmojis
    getGuildEmoji = (restCallAndHandle .) . R.GetGuildEmoji
    createGuildEmoji g t b =
        case R.parseEmojiImage b of
            Left x  -> throwM $ ProcessingError x
            Right x -> restCall (R.CreateGuildEmoji g t x) >>= handleDiscordResult
    modifyGuildEmoji = ((restCallAndHandle .) .) . R.ModifyGuildEmoji
    deleteGuildEmoji = (restCallAndHandle .) . R.DeleteGuildEmoji
    -- Guilds
    createGuild = restCallAndHandle . R.CreateGuild
    getGuild = restCallAndHandle . R.GetGuild
    modifyGuild = (restCallAndHandle .) . R.ModifyGuild
    deleteGuild = restCallAndHandle . R.DeleteGuild
    getGuildChannels = restCallAndHandle . R.GetGuildChannels
    createGuildChannel = (((restCallAndHandle .) .) .) . R.CreateGuildChannel
    modifyGuildChannelPositions = (restCallAndHandle .) . R.ModifyGuildChannelPositions
    getGuildMember = (restCallAndHandle .) . R.GetGuildMember
    listGuildMembers = (restCallAndHandle .) . R.ListGuildMembers
    addGuildMember = ((restCallAndHandle .) .) . R.AddGuildMember
    modifyGuildMember = ((restCallAndHandle .) .) . R.ModifyGuildMember
    modifyCurrentUserNick = (restCallAndHandle .) . R.ModifyCurrentUserNick
    addGuildMemberRole = ((restCallAndHandle .) .) . R.AddGuildMemberRole
    removeGuildMemberRole = ((restCallAndHandle .) .) . R.RemoveGuildMemberRole
    removeGuildMember = (restCallAndHandle .) . R.RemoveGuildMember
    getGuildBans = restCallAndHandle . R.GetGuildBans
    getGuildBan = (restCallAndHandle .) . R.GetGuildBan
    createGuildBan = ((restCallAndHandle .) .) . R.CreateGuildBan
    removeGuildBan = (restCallAndHandle .) . R.RemoveGuildBan
    getGuildRoles = restCallAndHandle . R.GetGuildRoles
    createGuildRole = (restCallAndHandle .) . R.CreateGuildRole
    modifyGuildRolePositions = (restCallAndHandle .) . R.ModifyGuildRolePositions
    modifyGuildRole = ((restCallAndHandle .) .) . R.ModifyGuildRole
    deleteGuildRole = (restCallAndHandle .) . R.DeleteGuildRole
    getGuildPruneCount = (restCallAndHandle .) . R.GetGuildPruneCount
    beginGuildPrune = (restCallAndHandle .) . R.BeginGuildPrune
    getGuildVoiceRegions = restCallAndHandle . R.GetGuildVoiceRegions
    getGuildInvites = restCallAndHandle . R.GetGuildInvites
    getGuildIntegrations = restCallAndHandle . R.GetGuildIntegrations
    createGuildIntegration = ((restCallAndHandle .) .) . R.CreateGuildIntegration
    modifyGuildIntegration = ((restCallAndHandle .) .) . R.ModifyGuildIntegration
    deleteGuildIntegration = (restCallAndHandle .) . R.DeleteGuildIntegration
    syncGuildIntegration = (restCallAndHandle .) . R.SyncGuildIntegration
    getGuildEmbed = restCallAndHandle . R.GetGuildEmbed
    modifyGuildEmbed = (restCallAndHandle .) . R.ModifyGuildEmbed
    getGuildVanityURL = restCallAndHandle . R.GetGuildVanityURL
    -- Invites
    getInvite = restCallAndHandle . R.GetInvite
    deleteInvite = restCallAndHandle . R.DeleteInvite
    -- Users
    getCurrentUser = restCallAndHandle R.GetCurrentUser
    getUser = restCallAndHandle . R.GetUser
    modifyCurrentUser = (restCallAndHandle .) . R.ModifyCurrentUser
    getCurrentUserGuilds = restCallAndHandle R.GetCurrentUserGuilds
    leaveGuild = restCallAndHandle . R.LeaveGuild
    getUserDMs = restCallAndHandle R.GetUserDMs
    createDM = restCallAndHandle . R.CreateDM
    getUserConnections = restCallAndHandle R.GetUserConnections
    -- Voice
    listVoiceRegions = restCallAndHandle R.ListVoiceRegions
    -- Webhooks
    createWebhook = (restCallAndHandle .) . R.CreateWebhook
    getChannelWebhooks = restCallAndHandle . R.GetChannelWebhooks
    getGuildWebhooks = restCallAndHandle . R.GetGuildWebhooks
    getWebhook = restCallAndHandle . R.GetWebhook
    getWebhookWithToken = (restCallAndHandle .) . R.GetWebhookWithToken
    modifyWebhook = (restCallAndHandle .) . R.ModifyWebhook
    modifyWebhookWithToken = ((restCallAndHandle .) .) . R.ModifyWebhookWithToken
    deleteWebhook = restCallAndHandle . R.DeleteWebhook
    deleteWebhookWithToken = (restCallAndHandle .) . R.DeleteWebhookWithToken
    executeWebhookWithToken = ((restCallAndHandle .) .) . R.ExecuteWebhookWithToken

    -- Gateway
    updateStatus newStatus newType newName = sendCommand $
        UpdateStatus $ UpdateStatusOpts
            { updateStatusOptsSince = Nothing
            , updateStatusOptsGame = Just $ Activity
                { activityName = newName
                , activityType = newType
                , activityUrl = Nothing
                }
            , updateStatusOptsNewStatus = newStatus
            , updateStatusOptsAFK = False
            }

    -- Custom utilities
    respond m t = void $ createMessage (messageChannel m) t
    reply m mention t = void $ createMessageDetailed (messageChannel m) $ def
        { R.messageDetailedContent = t
        , R.messageDetailedReference = Just $
            def { referenceMessageId = Just $ messageId m }
        , R.messageDetailedAllowedMentions = Just $
            def { R.mentionRepliedUser = mention }
        }

-- | Point-free (composable) function that calls a request and returns it in
-- a monad, throwing DiscordError on error.
restCallAndHandle :: (Request (r a), FromJSON a) => r a -> DiscordHandler a
restCallAndHandle req = restCall req >>= handleDiscordResult

-- | Handles the response of discord-haskell's REST calls in a monad @m@, where
-- and throws a @EinmyriaError@ (DiscordError) if the call errored.
handleDiscordResult :: Either RestCallErrorCode a -> DiscordHandler a
handleDiscordResult result =
    case result of
        Left e  -> throwM $ DiscordError e
        Right x -> pure $ x


