{-# LANGUAGE OverloadedStrings #-}
module ReactHandler where

import qualified Discord.Requests as R
import Data.Text as T (pack, Text)
import Discord ( restCall, DiscordHandler, RestCallErrorCode )
import Discord.Types
import MiscHandler
import Utils 


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
  messM <- messageFromReaction r --gets contents of message that was reacted to.
  case messM of
    Right mess -> do
      embedM <- getHallOfFameEmbed mess
      case embedM of
        Right embed ->
          sendMessageChanEmbed hallOfFameChannel "" embed
        Left err -> pure $ Left err
    Left err -> pure $ Left err

getHallOfFameDescription :: Message -> T.Text
getHallOfFameDescription m = messageText m <> "\n- " <> pingAuthorOf m <> " in " <> linkChannel (messageChannel m)

getImageFromMessage :: Message -> T.Text
getImageFromMessage m
  | not . null $ messageAttachments m = attachmentUrl (head $ messageAttachments m)
  | otherwise = ""

getHallOfFameEmbed :: Message -> DiscordHandler (Either RestCallErrorCode CreateEmbed)
getHallOfFameEmbed m = do
  messLinkM <- getMessageLink m
  case messLinkM of
    Right messLink -> pure $ Right (CreateEmbed "" "" Nothing "New Hall of Fame entry!" messLink (Just (CreateEmbedImageUrl $ getImageFromMessage m)) (getHallOfFameDescription m) [] Nothing (getTimestampFromMessage m) Nothing)
    Left err -> pure $ Left err