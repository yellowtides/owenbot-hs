{-# LANGUAGE OverloadedStrings #-}

module MiscHandler (isOwoifiable, handleOwoify,
                    isNietzsche, handleNietzsche,
                    isHallOfFameEmote, isEligibleForHallOfFame, handleHallOfFame, notInHallOfFameChannel) where

import qualified Discord.Requests as R
import Discord.Types
import Discord

import qualified Data.Maybe as M
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Time.Format as TF
import UnliftIO (liftIO)
import Text.Regex.TDFA

import Utils (sendMessageChan, sendMessageChanEmbed, sendFileChan, pingAuthorOf, linkChannel, getMessageLink, (=~=))
import Owoifier (owoify)

isOwoifiable :: T.Text -> Bool
isOwoifiable = (=~= ("[lLrR]|[nNmM][oO]" :: T.Text))

handleOwoify :: Message -> DiscordHandler (Either RestCallErrorCode Message)
handleOwoify m = sendMessageChan (messageChannel m) (pingAuthorOf m <> ": " <> owoify (messageText m))

isNietzsche :: T.Text -> Bool
isNietzsche = (=~= ("[gG]od *[iI]s *[dD]ead" :: T.Text))

handleNietzsche :: Message -> DiscordHandler (Either RestCallErrorCode Message)
handleNietzsche m = liftIO (TIO.readFile "./src/assets/nietzsche.txt") >>= sendMessageChan (messageChannel m) . owoify

hallOfFameEmote :: T.Text
hallOfFameEmote = "\11088"

hallOfFameChannel :: Snowflake
hallOfFameChannel = 797837507798499329

notInHallOfFameChannel :: ReactionInfo -> Bool
notInHallOfFameChannel r = reactionChannelId r /= hallOfFameChannel

isHallOfFameEmote :: ReactionInfo -> Bool
isHallOfFameEmote r = emojiName (reactionEmoji r) == hallOfFameEmote

messageFromReaction :: ReactionInfo -> DiscordHandler (Either RestCallErrorCode Message)
messageFromReaction r = restCall (R.GetChannelMessage (reactionChannelId r, reactionMessageId r))

isEligibleForHallOfFame :: ReactionInfo -> DiscordHandler (Either RestCallErrorCode Bool)
isEligibleForHallOfFame r = do
    messM <- messageFromReaction r
    case messM of
        Right mess -> pure $ Right $ any (\x -> (messageReactionCount x == 3) && (emojiName (messageReactionEmoji x) == hallOfFameEmote)) (messageReactions mess)
        Left err -> pure $ Left err

handleHallOfFame :: ReactionInfo -> DiscordHandler (Either RestCallErrorCode Message)
handleHallOfFame r = do
    messM <- messageFromReaction r
    case messM of
        Right mess -> do
            embedM <- getHallOfFameEmbed mess
            case embedM of
                Right embed -> sendMessageChanEmbed hallOfFameChannel "" embed
                Left err -> pure $ Left err
        Left err -> pure $ Left err


getHallOfFameDescription :: Message -> T.Text
getHallOfFameDescription m = messageText m <> "\n- " <> pingAuthorOf m <> " in " <> linkChannel (messageChannel m)

getImageFromMessage :: Message -> T.Text
getImageFromMessage m
    | not . null $ messageAttachments m = attachmentUrl (head $ messageAttachments m)
    | otherwise = ""

getTimestampFromMessage :: Message -> T.Text
getTimestampFromMessage m = T.pack $ TF.formatTime TF.defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" (messageTimestamp m)

getHallOfFameEmbed :: Message -> DiscordHandler (Either RestCallErrorCode CreateEmbed )
getHallOfFameEmbed m = do
  messLinkM <- getMessageLink m
  case messLinkM of
        Right messLink -> pure $ Right (CreateEmbed "" "" Nothing "New Hall of Fame entry!" messLink (Just (CreateEmbedImageUrl $ getImageFromMessage m)) (getHallOfFameDescription m) [] Nothing (getTimestampFromMessage m) Nothing)
        Left err -> pure $ Left err