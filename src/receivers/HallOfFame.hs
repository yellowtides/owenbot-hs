{-# LANGUAGE OverloadedStrings #-}

module HallOfFame ( reactionReceivers, messageReceivers ) where

import           Control.Monad      ( guard
                                    , when
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
                                              )
                                    , MessageReaction ( messageReactionCount
                                                      , messageReactionEmoji
                                                      )
                                    , CreateEmbed ( CreateEmbed )
                                    , CreateEmbedImage ( CreateEmbedImageUrl )
                                    , ReactionInfo ( reactionEmoji
                                                   , reactionChannelId
                                                   , reactionMessageId
                                                   )
                                    )
import           Text.Read          ( readMaybe )
import           UnliftIO           ( liftIO )

import           Utils              ( sendMessageChan
                                    , pingAuthorOf
                                    , messageFromReaction
                                    , linkChannel
                                    , getMessageLink
                                    , sendMessageChanEmbed
                                    , getTimestampFromMessage
                                    , newDevCommand
                                    )
import           CSV                ( readSingleColCSV
                                    , writeSingleColCSV
                                    )


reactionReceivers :: [ReactionInfo -> DiscordHandler ()]
reactionReceivers = [ attemptHallOfFame ]

messageReceivers :: [Message -> DiscordHandler ()]
messageReceivers = [ reactLimit ]

attemptHallOfFame :: ReactionInfo -> DiscordHandler ()
attemptHallOfFame r = do
    when (isHallOfFameEmote r && notInHallOfFameChannel r) $ do
        eligible <- isEligibleForHallOfFame r
        when eligible $ putInHallOfFame r

hallOfFameEmotes :: [T.Text]
hallOfFameEmotes =
    [ "XREE"
    , "KEKW"
    , "\11088" -- star
    ] -- These are matched case-insensitively

hallOfFameChannel :: ChannelId
hallOfFameChannel = 790936269382615082 --the channel id for the hall of fame

notInHallOfFameChannel :: ReactionInfo -> Bool
notInHallOfFameChannel r = reactionChannelId r /= hallOfFameChannel

isHallOfFameEmote :: ReactionInfo -> Bool
isHallOfFameEmote r = T.toUpper (emojiName (reactionEmoji r)) `elem` hallOfFameEmotes

isEligibleForHallOfFame :: ReactionInfo -> DiscordHandler Bool
isEligibleForHallOfFame r = do
    messM <- messageFromReaction r
    case messM of
        Right mess -> do
            msgIdList <- liftIO $ readSingleColCSV "fame.csv"
            let msgIdListStr = T.unpack <$> msgIdList
            limit <- liftIO readLimit
            let existsInHOF = show (messageId mess) `elem` msgIdListStr
            let reactions   = messageReactions mess
            let capsEmotes  = map T.toUpper hallOfFameEmotes
            let fulfillCond = \x ->
                    messageReactionCount x >= limit
                    && T.toUpper (emojiName $ messageReactionEmoji x) `elem` capsEmotes
                    && not existsInHOF
            pure $ any fulfillCond reactions
        Left err -> liftIO (print err) >> pure False

putInHallOfFame :: ReactionInfo -> DiscordHandler ()
putInHallOfFame r = do
    messM <- messageFromReaction r --gets contents of message that was reacted to.
    case messM of
        Right mess -> do
            embedM <- createHallOfFameEmbed mess
            case embedM of
                Right embed -> do
                    msgIdList <- liftIO $ readSingleColCSV "fame.csv"
                    liftIO $ writeSingleColCSV "fame.csv" (T.pack (show $ messageId mess):msgIdList)
                    --adds the message id to the csv to make sure we dont add it multiple times.
                    sendMessageChanEmbed hallOfFameChannel "" embed
                Left err -> liftIO (putStrLn "Couldn't get link to message")
        Left err -> liftIO (putStrLn "Couldn't find associated message")

createDescription :: Message -> T.Text
createDescription m = messageText m <> "\n- " <> pingAuthorOf m <> " in " <> linkChannel (messageChannel m)

getImageFromMessage :: Message -> T.Text
getImageFromMessage m
  | not . null $ messageAttachments m = attachmentUrl (head $ messageAttachments m)
  | otherwise = ""

createHallOfFameEmbed :: Message -> DiscordHandler (Either RestCallErrorCode CreateEmbed)
createHallOfFameEmbed m = do
    messLinkM <- getMessageLink m
    case messLinkM of
        Right messLink -> do
            let authorName = ""
            let authorUrl = ""
            let authorIcon = Nothing
            let embedTitle = "👑 best of ouw buwwshit"
            let embedUrl = ""
            let embedThumbnail = Nothing
            let embedDescription = createDescription m <> "\n\n[Original Message](" <> messLink <> ")"
            let embedFields = []
            let embedImage = Just (CreateEmbedImageUrl $ getImageFromMessage m)
            let embedFooterText = getTimestampFromMessage m
            let embedFooterIcon = Nothing
            pure $ Right (CreateEmbed authorName
                                      authorUrl
                                      authorIcon
                                      embedTitle
                                      embedUrl
                                      embedThumbnail
                                      embedDescription
                                      embedFields
                                      embedImage
                                      embedFooterText
                                      embedFooterIcon )
        Left err -> pure $ Left err

reactLimit :: Message -> DiscordHandler ()
reactLimit m = newDevCommand m "reactLimit *([0-9]{1,3})?" $ \captures -> do
    let parsed = readMaybe (T.unpack $ head captures)
    case parsed of
        Nothing -> do
            i <- liftIO $ readLimit
            sendMessageChan (messageChannel m)
                $ "Current limit is at " <> T.pack (show i)
        Just i -> do
            liftIO $ editLimit i
            sendMessageChan (messageChannel m) "New Limit Set"

editLimit :: Int -> IO ()
editLimit i = writeSingleColCSV "reactLim.csv" [T.pack $ show i]

readLimit :: IO Int
readLimit = do
    contents <- readSingleColCSV "reactLim.csv"
    if null contents
        then writeSingleColCSV "reactLim.csv" ["1"] >> pure 1
        else pure $ read $ T.unpack $ head contents
