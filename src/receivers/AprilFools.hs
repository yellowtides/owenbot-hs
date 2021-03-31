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
                                    , Message (..)
                                    , MessageReaction (..)
                                    , CreateEmbed ( CreateEmbed )
                                    , CreateEmbedImage ( CreateEmbedImageUrl )
                                    , ReactionInfo (..)
                                    , User (..)
                                    , UserId
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
                                    , emojiToUsableText, sendMessageDM
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
                "*",
                reacterUsername,
                " reacted to \"",
                T.take maxMessageLen $ messageText message,
                ellipses,
                "\" with ",
                emojiT,
                "*"
            ]
    when (isLikelyIRCMessage message) (sendMessageChanPingsDisabled cid reactMessageT)
    where
        uid    = reactionUserId r
        cid    = reactionChannelId r
        mid    = reactionMessageId r
        emojiT = emojiToUsableText $ reactionEmoji r

-- | Returns false only if it a crosspost, new pin add, announcement follow added, boost, etc.
isProperMessage :: Message -> Bool
isProperMessage m = not (messageReference m /= Nothing && referencedMessage m == Nothing)

rewriteMessageAsIRC :: Message -> DiscordHandler ()
rewriteMessageAsIRC m = do

    -- Safely use guard because there won't be any handlers using this.
    -- Concern is that a CompSoc msg may get sent during the day, but it's an insignificance
    -- compared to how often msgs can get pinned by mods.
    guard $ isProperMessage m
    
    excludeThisPing <- botIdM
    let userPings = pingUser <$> filter ((/= excludeThisPing) . userId) mentionU
    let rolePings = pingRole <$> mentionR
    let replyUsername = maybe "" ircMessageToUsername (referencedMessage m)
    -- _ <- liftIO . print $ replyUsername
    replyPing <- case messageGuild m of 
            Just guildId -> pingWithUsername replyUsername guildId
            Nothing      -> pure ""
    -- _ <- liftIO . print $ replyPing
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
    let postHandleT = T.concat
            [ mentionsT
            , stripAllPings messageT
            ] 
    let newMessageT = T.concat
            [ authorHandle
            , postHandleT
            ]
    -- > SEND IRC MESSAGE
    _ <- restCall $ R.DeleteMessage (cid, mid)
    unless (null attach)
        . sendMessageDM (userId $ messageAuthor m) $ T.concat 
          [ "**An IRC client doesn't support direct file upload!** "
          , "Consider uploading your media to https://imgur.com/upload and "
          , "posting a link in chat instead."
          ]
    -- Only send a message when the attachments are null.
    guard $ null attach

    unless (T.null postHandleT)
        $ sendMessageChan cid newMessageT
    -- _ <- liftIO . print $ messageT
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
