{-# LANGUAGE OverloadedStrings #-}

module AprilFools ( messageReceivers
                  , reactionReceivers
                  ) where

import           Control.Monad      ( guard
                                    , when
                                    , join, unless
                                    )
import           Data.Functor       ( (<&>) )
import qualified Data.Text as T
import           Discord            ( restCall
                                    , DiscordHandler
                                    , RestCallErrorCode
                                    )
import qualified Discord.Requests as R
import           Discord.Types      ( ChannelId
                                    , Snowflake
                                    , Attachment ( attachmentUrl )
                                    , Emoji ( emojiName )
                                    , Message ( messageReactions
                                              , messageId
                                              , messageText
                                              , messageChannel
                                              , messageAttachments
                                              , messageChannel
                                              , messageAuthor
                                              , messageEmbeds
                                              , messageMentionRoles
                                              , messageMentions
                                              , referencedMessage
                                              , messageGuild
                                              )
                                    , MessageReaction ( messageReactionCount
                                                      , messageReactionEmoji
                                                      )
                                    , CreateEmbed ( CreateEmbed )
                                    , CreateEmbedImage ( CreateEmbedImageUrl )
                                    , ReactionInfo ( reactionEmoji
                                                   , reactionChannelId
                                                   , reactionMessageId, reactionUserId
                                                   ), User ( userName, userId ), UserId
                                    )
import           Text.Read          ( readMaybe )
import           UnliftIO           ( liftIO )

import           Utils              ( sendMessageChan
                                    , pingAuthorOf
                                    , messageFromReaction
                                    , linkChannel
                                    , getMessageLink
                                    , sendMessageChanPingsDisabled
                                    , getTimestampFromMessage
                                    , newDevCommand, pingUser, pingRole, stripAllPings, pingWithUsername
                                    )
import           CSV                ( readSingleColCSV
                                    , writeSingleColCSV
                                    )
import Discord.Internal.Rest.User   ( UserRequest ( GetUser ) )
import Data.Either                  ( fromRight )

reactionReceivers :: [ReactionInfo -> DiscordHandler ()]
reactionReceivers = [ rewriteReactionAsIRC ]

messageReceivers :: [Message -> DiscordHandler ()]
messageReceivers = [ rewriteMessageAsIRC ]

ircMessageToUsername :: Message -> T.Text
ircMessageToUsername = T.drop 1                   -- finally, drop the remaining <
                       . T.dropWhile (/= '<')     -- drop until < (usually **)
                       . T.takeWhile (/= '>')     -- take until >**
                       . messageText              -- everything that's left is inbetween!

isLikelyIRCMessage :: Message -> Bool
isLikelyIRCMessage m = T.take 3 (messageText m) == "**<"

botIdM :: DiscordHandler UserId
botIdM = do
    Right user <- restCall R.GetCurrentUser
    pure $ userId user

rewriteReactionAsIRC :: ReactionInfo -> DiscordHandler ()
rewriteReactionAsIRC r = do
    Right user <- restCall $ R.GetUser uid
    Right message <- restCall $ R.GetChannelMessage (cid, mid)
    _ <- restCall $ R.DeleteAllReactions (cid, mid)
    let reactedMessageT = messageText message
    let reacterUsername = userName user
    let maxMessageLen = 69
    let ellipses = if T.length reactedMessageT > maxMessageLen
                        then "[...]"
                        else ""
    let reactMessageT = T.concat [
                "_",
                reacterUsername,
                " reacted to \"",
                T.take maxMessageLen $ messageText message,
                ellipses,
                "\" with ",
                emojiT,
                "_"
            ]
    when (isLikelyIRCMessage message) (sendMessageChanPingsDisabled cid reactMessageT)
    where
        uid    = reactionUserId r
        cid    = reactionChannelId r
        mid    = reactionMessageId r
        emojiT = emojiName $ reactionEmoji r

rewriteMessageAsIRC :: Message -> DiscordHandler ()
rewriteMessageAsIRC m = do
    excludeThisPing <- botIdM
    let userPings = pingUser <$> filter ((/= excludeThisPing) . userId) mentionU
    let rolePings = pingRole <$> mentionR
    let replyUsername = maybe "" ircMessageToUsername (referencedMessage m)
    _ <- liftIO . print $ replyUsername
    replyPing <- case messageGuild m of 
            Just guildId -> pingWithUsername replyUsername guildId
            Nothing      -> pure ""
    _ <- liftIO . print $ replyPing
    -- > AUTHOR HANDLE
    let authorHandle = T.concat
            [ "**<"
            , userName author
            , ">** "
            ]
    -- > MESSAGE CONTENTS
    let mentionsT = T.concat
            [ onNonEmptyAddAfter replyPing ": "
            , onNonEmptyAddAfter (T.intercalate ": " userPings) ": "
            , onNonEmptyAddAfter (T.intercalate ": " rolePings) ": "
            ]
    let attachURLs = attachmentUrl <$> attach
    let attachmentsT = T.concat
            [ onNonEmptyAddBefore (T.intercalate "\n" attachURLs) "\n"
            ]
    let postHandleT = T.concat
            [ mentionsT
            , stripAllPings messageT
            , attachmentsT
            ] 
    let newMessageT = T.concat
            [ authorHandle
            , postHandleT
            ]
    -- > SEND IRC MESSAGE
    unless (T.null postHandleT)
        $ sendMessageChan cid newMessageT
    _ <- liftIO . print $ messageT
    _ <- restCall $ R.DeleteMessage (cid, mid)
    pure ()
    where
        cid                   = messageChannel m
        mid                   = messageId m
        author                = messageAuthor m
        messageT              = messageText m
        attach                = messageAttachments m
        mentionU              = messageMentions m
        mentionR              = messageMentionRoles m
        replyM                = referencedMessage m
        onNonEmptyAddAfter t extra  = if T.length t /= 0
                                         then t <> extra
                                         else t
        onNonEmptyAddBefore t extra = if T.length t /= 0
                                         then extra <> t
                                         else t
