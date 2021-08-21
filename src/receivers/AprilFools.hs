{-# LANGUAGE OverloadedStrings #-}

module AprilFools (messageReceivers, reactionReceivers) where

import Control.Monad (guard, when, unless)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.Maybe (isNothing, isJust)
import qualified Data.Text as T
import Discord (restCall, DiscordHandler, def)
import qualified Discord.Requests as R
import Discord.Types (Attachment(..), Message(..), ReactionInfo(..), User(..), UserId)
import Network.HTTP.Conduit (simpleHttp)
import UnliftIO (liftIO)

import Utils
    ( sendMessageChanPingsDisabled
    , pingUser
    , pingRole
    , stripAllPings
    , pingWithUsername
    , emojiToUsableText
    )

reactionReceivers :: [ReactionInfo -> DiscordHandler ()]
reactionReceivers = [rewriteReactionAsIRC]

messageReceivers :: [Message -> DiscordHandler ()]
messageReceivers = [rewriteMessageAsIRC]

ircMessageToUsername :: Message -> T.Text
ircMessageToUsername =
    T.drop 1                   -- finally, drop the remaining <
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
    Right user    <- restCall $ R.GetUser uid
    Right message <- restCall $ R.GetChannelMessage (cid, mid)
    _             <- restCall $ R.DeleteAllReactions (cid, mid)
    let reactedMessageT = messageText message
    let reacterUsername = userName user
    let maxMessageLen   = 69
    let ellipses = if T.length reactedMessageT > maxMessageLen then "[...]" else ""
    let reactMessageT = T.concat
            [ "*"
            , reacterUsername
            , " reacted to \""
            , T.take maxMessageLen $ messageText message
            , ellipses
            , "\" with "
            , emojiT
            , "*"
            ]
    when (isLikelyIRCMessage message) (sendMessageChanPingsDisabled cid reactMessageT)
  where
    uid    = reactionUserId r
    cid    = reactionChannelId r
    mid    = reactionMessageId r
    emojiT = emojiToUsableText $ reactionEmoji r

-- | Returns false only if it a crosspost, new pin add, announcement follow added, boost, etc.
isProperMessage :: Message -> Bool
isProperMessage m = isNothing (messageReference m) && isJust (referencedMessage m)

rewriteMessageAsIRC :: Message -> DiscordHandler ()
rewriteMessageAsIRC m = do

    -- Safely use guard because there won't be any handlers using this.
    -- Concern is that a CompSoc msg may get sent during the day, but it's an insignificance
    -- compared to how often msgs can get pinned by mods.
    guard $ isProperMessage m

    excludeThisPing <- botIdM
    let userPings = pingUser <$> filter ((/= excludeThisPing) . userId) mentionU
    let rolePings     = pingRole <$> mentionR
    let replyUsername = maybe "" ircMessageToUsername (referencedMessage m)
    -- _ <- liftIO . print $ replyUsername
    replyPing <- case messageGuild m of
        Just guildId -> pingWithUsername replyUsername guildId
        Nothing      -> pure ""
    -- _ <- liftIO . print $ replyPing
    -- > AUTHOR HANDLE
    let authorHandle = T.concat ["**<", userName author, ">** "]
    -- > MESSAGE CONTENTS
    let mentionsT = T.concat
            [ onNonEmptyAddAfter replyPing                      ": "
            , onNonEmptyAddAfter (T.intercalate ": " userPings) ": "
            , onNonEmptyAddAfter (T.intercalate ": " rolePings) ": "
            ]
    let postHandleT = T.concat [mentionsT, stripAllPings messageT]
    let newMessageT = T.concat [authorHandle, postHandleT]

    -- Download before deletion, will return Nothing if attach is null
    downloadedAttachment <- liftIO $ downloadFirstAttachment attach

    -- > SEND IRC MESSAGE
    _                    <- restCall $ R.DeleteMessage (cid, mid)

    -- Unless it's a truly empty message, i.e. no attachments & no text
    unless (T.null postHandleT && isNothing downloadedAttachment) $ do
        let opts = def
                { R.messageDetailedContent = newMessageT
                , R.messageDetailedFile    = downloadedAttachment
                }
        _ <- restCall $ R.CreateMessageDetailed cid opts
        pure ()

    -- _ <- liftIO . print $ messageT






  where
    cid      = messageChannel m
    mid      = messageId m
    author   = messageAuthor m
    messageT = messageText m
    attach   = messageAttachments m
    mentionU = messageMentions m
    mentionR = messageMentionRoles m
    onNonEmptyAddAfter t extra = if T.length t /= 0 then t <> extra else t

-- | If possible download the first attachment into memory, return it in Maybe
-- I don't really know how GHC handles it but I guess the memory is freed when
-- the thread finishes processing.
downloadFirstAttachment :: [Attachment] -> IO (Maybe (T.Text, B.ByteString))
downloadFirstAttachment []      = pure Nothing
downloadFirstAttachment (a : _) = do
    -- Start lazy download of bytestring
    bytestring <- simpleHttp $ T.unpack $ attachmentUrl a
    -- Convert lazy to not lazy. Forces entire lazy data into memory so it's expensive.
    -- Only possible because the VPS has enough RAM.
    let stricter = BL.toStrict bytestring
    pure $ Just (attachmentFilename a, stricter)
