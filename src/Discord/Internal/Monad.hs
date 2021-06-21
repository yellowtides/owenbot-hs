{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-} -- allow instance declaration of MonadDiscord
{-
Module      : Discord.Internal.Monad
License     : BSD (see the LICENSE file)
Description : The DiscordMonad class and its instances.

This module contains the 'MonadDiscord' data class, which abstracts away all
the possible REST interactions with Discord. It also defines instances for
'DiscordHandler' and 'ReaderT Auth IO'.

-}
module Discord.Internal.Monad
    ( MonadDiscord(..)
    ) where

import           Control.Concurrent         ( threadDelay )
import           Control.Exception.Safe     ( MonadThrow
                                            , MonadMask
                                            , SomeException
                                            , throwM
                                            , try
                                            )
import           Control.Monad.IO.Class     ( MonadIO
                                            , liftIO
                                            )
import           Control.Monad              ( void )
import           Control.Monad.Reader       ( ReaderT
                                            , ask
                                            )
import           Data.Aeson                 ( FromJSON
                                            , eitherDecode
                                            )
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import           Data.Ix                    ( inRange )
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import           Data.Time.Clock.POSIX      ( POSIXTime
                                            , getPOSIXTime
                                            )
import qualified Network.HTTP.Req as Req
import           Text.Read                  ( readMaybe )

import           Discord.Internal.Rest.Prelude
import           Discord.Internal.Rest.HTTP ( RestCallInternalException(..)
                                            , Request(..)
                                            , JsonRequest(..)
                                            )
import qualified Discord.Requests as R
import           Discord.Types
import           Discord

import           Command.Error             ( CommandError(..) )

-- | @MonadDiscord@ is a data class of Monads that can interact with Discord.
-- It requires MonadThrow to throw possible errors like HTTP errors, MonadMask
-- as a helper for common operations paired with errors (like `finally`), and
-- MonadFail to allow convenient pattern-matching in do-notation (not necessary,
-- but helps a lot with our code style).
class (Monad m, MonadThrow m, MonadMask m, MonadFail m) => MonadDiscord m where
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

    -- Custom utilities
    respond       :: Message -> T.Text -> m ()

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

    -- Custom utilities
    respond m t = void $ createMessage (messageChannel m) t

-- | @restCallAndHandle@ calls a request and returns it in the DiscordHandler
-- monad, throwing @DiscordError@ on errors.
restCallAndHandle :: (Request (r a), FromJSON a) => r a -> DiscordHandler a
restCallAndHandle req = restCall req >>= handleDiscordResult

-- | Handles the response of discord-haskell's REST calls in DiscordHandler
-- and throws a 'DiscordError' if the call errored.
handleDiscordResult :: Either RestCallErrorCode a -> DiscordHandler a
handleDiscordResult result =
    case result of
        Left e  -> throwM $ DiscordError e
        Right x -> pure x

-- | Unfortunately, IO cannot be an instance of MonadDiscord because it needs
-- access to the Auth tokens, but that can't be put in the type signature.
-- So instead, it is implicitly passed through a ReaderT.
--
-- [Usage:]
-- @
-- import Control.Monad.Reader ( runReaderT )
-- someFunc :: IO ()
-- someFunc = runReaderT (createMessage 1234 "text" >> pure ()) (Auth "tokenhere")
-- @
instance MonadDiscord (ReaderT Auth IO) where
    getChannel = callRestIO . R.GetChannel
    modifyChannel = (callRestIO .) . R.ModifyChannel
    deleteChannel = callRestIO . R.DeleteChannel
    getChannelMessages = (callRestIO .) . R.GetChannelMessages
    getChannelMessage = callRestIO . R.GetChannelMessage
    createMessage = (callRestIO .) . R.CreateMessage
    createMessageEmbed = ((callRestIO .) .) . R.CreateMessageEmbed
    createMessageUploadFile = ((callRestIO .) .) . R.CreateMessageUploadFile
    createMessageDetailed = (callRestIO .) . R.CreateMessageDetailed
    createReaction = (callRestIO .) . R.CreateReaction
    deleteOwnReaction = (callRestIO .) . R.DeleteOwnReaction
    deleteUserReaction = ((callRestIO .) .) . R.DeleteUserReaction
    deleteSingleReaction = (callRestIO .) . R.DeleteSingleReaction
    getReactions = ((callRestIO .) .) . R.GetReactions
    deleteAllReactions = callRestIO . R.DeleteAllReactions
    editMessage = ((callRestIO .) .) . R.EditMessage
    deleteMessage = callRestIO . R.DeleteMessage
    bulkDeleteMessage = callRestIO . R.BulkDeleteMessage
    editChannelPermissions = ((callRestIO .) .) . R.EditChannelPermissions
    getChannelInvites = callRestIO . R.GetChannelInvites
    createChannelInvite = (callRestIO .) . R.CreateChannelInvite
    deleteChannelPermission = (callRestIO .) . R.DeleteChannelPermission
    triggerTypingIndicator = callRestIO . R.TriggerTypingIndicator
    getPinnedMessages = callRestIO . R.GetPinnedMessages
    addPinnedMessage = callRestIO . R.AddPinnedMessage
    deletePinnedMessage = callRestIO . R.DeletePinnedMessage
    groupDMAddRecipient = (callRestIO .) . R.GroupDMAddRecipient
    groupDMRemoveRecipient = (callRestIO .) . R.GroupDMRemoveRecipient
    -- Emojis
    listGuildEmojis = callRestIO . R.ListGuildEmojis
    getGuildEmoji = (callRestIO .) . R.GetGuildEmoji
    createGuildEmoji g t b =
        case R.parseEmojiImage b of
            Left x  -> throwM $ ProcessingError x
            Right x -> callRestIO $ R.CreateGuildEmoji g t x
    modifyGuildEmoji = ((callRestIO .) .) . R.ModifyGuildEmoji
    deleteGuildEmoji = (callRestIO .) . R.DeleteGuildEmoji
    -- Guilds
    createGuild = callRestIO . R.CreateGuild
    getGuild = callRestIO . R.GetGuild
    modifyGuild = (callRestIO .) . R.ModifyGuild
    deleteGuild = callRestIO . R.DeleteGuild
    getGuildChannels = callRestIO . R.GetGuildChannels
    createGuildChannel = (((callRestIO .) .) .) . R.CreateGuildChannel
    modifyGuildChannelPositions = (callRestIO .) . R.ModifyGuildChannelPositions
    getGuildMember = (callRestIO .) . R.GetGuildMember
    listGuildMembers = (callRestIO .) . R.ListGuildMembers
    addGuildMember = ((callRestIO .) .) . R.AddGuildMember
    modifyGuildMember = ((callRestIO .) .) . R.ModifyGuildMember
    modifyCurrentUserNick = (callRestIO .) . R.ModifyCurrentUserNick
    addGuildMemberRole = ((callRestIO .) .) . R.AddGuildMemberRole
    removeGuildMemberRole = ((callRestIO .) .) . R.RemoveGuildMemberRole
    removeGuildMember = (callRestIO .) . R.RemoveGuildMember
    getGuildBans = callRestIO . R.GetGuildBans
    getGuildBan = (callRestIO .) . R.GetGuildBan
    createGuildBan = ((callRestIO .) .) . R.CreateGuildBan
    removeGuildBan = (callRestIO .) . R.RemoveGuildBan
    getGuildRoles = callRestIO . R.GetGuildRoles
    createGuildRole = (callRestIO .) . R.CreateGuildRole
    modifyGuildRolePositions = (callRestIO .) . R.ModifyGuildRolePositions
    modifyGuildRole = ((callRestIO .) .) . R.ModifyGuildRole
    deleteGuildRole = (callRestIO .) . R.DeleteGuildRole
    getGuildPruneCount = (callRestIO .) . R.GetGuildPruneCount
    beginGuildPrune = (callRestIO .) . R.BeginGuildPrune
    getGuildVoiceRegions = callRestIO . R.GetGuildVoiceRegions
    getGuildInvites = callRestIO . R.GetGuildInvites
    getGuildIntegrations = callRestIO . R.GetGuildIntegrations
    createGuildIntegration = ((callRestIO .) .) . R.CreateGuildIntegration
    modifyGuildIntegration = ((callRestIO .) .) . R.ModifyGuildIntegration
    deleteGuildIntegration = (callRestIO .) . R.DeleteGuildIntegration
    syncGuildIntegration = (callRestIO .) . R.SyncGuildIntegration
    getGuildEmbed = callRestIO . R.GetGuildEmbed
    modifyGuildEmbed = (callRestIO .) . R.ModifyGuildEmbed
    getGuildVanityURL = callRestIO . R.GetGuildVanityURL
    -- Invites
    getInvite = callRestIO . R.GetInvite
    deleteInvite = callRestIO . R.DeleteInvite
    -- Users
    getCurrentUser = callRestIO R.GetCurrentUser
    getUser = callRestIO . R.GetUser
    modifyCurrentUser = (callRestIO .) . R.ModifyCurrentUser
    getCurrentUserGuilds = callRestIO R.GetCurrentUserGuilds
    leaveGuild = callRestIO . R.LeaveGuild
    getUserDMs = callRestIO R.GetUserDMs
    createDM = callRestIO . R.CreateDM
    getUserConnections = callRestIO R.GetUserConnections
    -- Voice
    listVoiceRegions = callRestIO R.ListVoiceRegions
    -- Webhooks
    createWebhook = (callRestIO .) . R.CreateWebhook
    getChannelWebhooks = callRestIO . R.GetChannelWebhooks
    getGuildWebhooks = callRestIO . R.GetGuildWebhooks
    getWebhook = callRestIO . R.GetWebhook
    getWebhookWithToken = (callRestIO .) . R.GetWebhookWithToken
    modifyWebhook = (callRestIO .) . R.ModifyWebhook
    modifyWebhookWithToken = ((callRestIO .) .) . R.ModifyWebhookWithToken
    deleteWebhook = callRestIO . R.DeleteWebhook
    deleteWebhookWithToken = (callRestIO .) . R.DeleteWebhookWithToken
    executeWebhookWithToken = ((callRestIO .) .) . R.ExecuteWebhookWithToken

    -- Custom utilities
    respond m t = void $ createMessage (messageChannel m) t


-- the following is a GIGANTIC mess basically cut and pasted from discord-haskell's
-- internals and simplified, because it's alas, not exported.

data RequestResponse
    = ResponseTryAgain
    | ResponseByteString BL.ByteString
    | ResponseErrorCode Int B.ByteString B.ByteString
    deriving (Show)

-- | Do an IO call to get the result of the request, and throw errors
-- whenever some error occurs. A simplified version of the logic within
-- discord-haskell's Discord.Internal.Rest.HTTP module, as it doesn't need to
-- store the error information, it can just throw it.
callRestIO
    :: (Request (r a), FromJSON a)
    => r a
    -> ReaderT Auth IO a
callRestIO req = do
    -- get the token
    auth <- ask
    -- create a request with the auth in the header
    let action = compileRequest auth (jsonRequest req)
    -- send off the request, get the response
    resp <- liftIO $ restIOtoIO action
    let body = Req.responseBody resp
        code = Req.responseStatusCode resp
        status = TE.decodeUtf8 $ Req.responseStatusMessage resp
    case () of
        _ | code `elem` [429, 500, 502] -> do
            -- wait a bit before retrying.
            liftIO $ threadDelay (10 * 10^(6 :: Int))
            callRestIO req
        _ | code >= 200 && code <= 299 -> do
            let parsableBody = if body == "" then "[]" else body
            case eitherDecode parsableBody of
                Right o -> pure o
                Left er -> do
                    let formaterr = T.pack $ "Response could not be parsed " <> er
                    throwM $ DiscordError $ RestCallErrorCode code "err" formaterr
        _ | otherwise -> do
            throwM $ DiscordError $ RestCallErrorCode code status (TE.decodeUtf8 $ BL.toStrict body)

-- | From a JsonRequest, create a RestIO thingy with the appropriately auth
-- headers. Idk why this is necessary but it doesn't work without it.
compileRequest :: Auth -> JsonRequest -> RestIO Req.LbsResponse
compileRequest auth request = 
  let
    authopt = authHeader auth <> Req.header "X-RateLimit-Precision" "millisecond"
  in
    case request of
        (Delete url opts) ->
            Req.req Req.DELETE url Req.NoReqBody Req.lbsResponse (authopt <> opts)
        (Get url opts) ->
            Req.req Req.GET url Req.NoReqBody Req.lbsResponse (authopt <> opts)
        (Put url body opts) ->
            Req.req Req.PUT url body Req.lbsResponse (authopt <> opts)
        (Patch url body opts) -> do
            b <- body
            Req.req Req.PATCH url b Req.lbsResponse (authopt <> opts)
        (Post url body opts) -> do
            b <- body
            Req.req Req.POST url b Req.lbsResponse (authopt <> opts)

