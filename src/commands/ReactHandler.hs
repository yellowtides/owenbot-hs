{-# LANGUAGE OverloadedStrings #-}
module ReactHandler where

import qualified Discord.Requests as R
import Data.Text as T (pack, toUpper, Text)
import Data.Functor ((<&>))
import Discord ( restCall, DiscordHandler, RestCallErrorCode )
import Discord.Types
    ( Snowflake,
      Attachment(attachmentUrl),
      Emoji(emojiName),
      Message(messageReactions, messageId, messageText, messageChannel,
              messageAttachments, messageChannel),
      MessageReaction(messageReactionCount, messageReactionEmoji),
      CreateEmbed(CreateEmbed),
      CreateEmbedImage(CreateEmbedImageUrl),
      ReactionInfo(reactionEmoji, reactionChannelId, reactionMessageId) )
import MiscHandler ()
import Utils
    (sendMessageChan, checkRoleIDs,  pingAuthorOf,
      linkChannel,
      getMessageLink,
      sendMessageChanEmbed,
      getTimestampFromMessage,
      openCSV,
      addToCSV,
      rmFuncText )
import UnliftIO(liftIO)
import Control.Monad (guard)


hallOfFameEmotes :: [T.Text]
hallOfFameEmotes = ["XREE", "KEKW", "STAR"] --capitalised emotes

hallOfFameChannel :: Snowflake
hallOfFameChannel = 790936269382615082 --the channel id for the hall of fame

notInHallOfFameChannel :: ReactionInfo -> Bool
notInHallOfFameChannel r = reactionChannelId r /= hallOfFameChannel

isHallOfFameEmote :: ReactionInfo -> Bool
isHallOfFameEmote r = toUpper (emojiName (reactionEmoji r)) `elem` hallOfFameEmotes

messageFromReaction :: ReactionInfo -> DiscordHandler (Either RestCallErrorCode Message)
messageFromReaction r = restCall (R.GetChannelMessage (reactionChannelId r, reactionMessageId r))

isEligibleForHallOfFame :: ReactionInfo -> DiscordHandler (Either RestCallErrorCode Bool)
isEligibleForHallOfFame r = do
  messM <- messageFromReaction r
  case messM of
    Right mess -> do
      msgIdlist <- liftIO $ openCSV "fame.csv"
      limit <- liftIO readLimit
      pure $ Right $ any (\x -> (messageReactionCount x >= limit) && (toUpper (emojiName (messageReactionEmoji x)) `elem` hallOfFameEmotes) && (show (messageId mess) `notElem` msgIdlist)) (messageReactions mess)
    Left err -> pure $ Left err

handleHallOfFame :: ReactionInfo -> DiscordHandler (Either RestCallErrorCode Message)
handleHallOfFame r = do
  messM <- messageFromReaction r --gets contents of message that was reacted to.
  case messM of
    Right mess -> do
      embedM <- getHallOfFameEmbed mess
      case embedM of
        Right embed -> do
          liftIO $ addToCSV "fame.csv" (show (messageId mess) ++ ", ") >> pure () --adds the message id to the csv to make sure we dont add it multiple times.
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
    Right messLink -> pure $ Right (CreateEmbed "" "" Nothing "👑 best of ouw buwwshit" "" Nothing (getHallOfFameDescription m <> "\n\n[Original Message](" <> messLink <> ")") [] (Just (CreateEmbedImageUrl $ getImageFromMessage m)) (getTimestampFromMessage m) Nothing)
    Left err -> pure $ Left err

-- setLimit :: Message -> Int -> IO()
-- setLimit :: Message -> Int -> DiscordHandler ()
setLimit :: Message -> Int -> DiscordHandler (Either RestCallErrorCode Message)
setLimit m i = do
  b1 <- checkRoleIDs m
  if or b1 then do
      liftIO $ editLimit i
      sendMessageChan (messageChannel m) "New Limit Set"
    else sendMessageChan (messageChannel m ) "Insufficient Priveledges"

editLimit :: Int -> IO()
editLimit = writeFile "src/config/reactLim.conf" . show

readLimit :: IO Int
readLimit = openCSV "src/config/reactLim.conf" <&> (read . head)
