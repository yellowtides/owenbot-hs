{-# LANGUAGE OverloadedStrings #-}

module HallOfFame ( reactionReceivers, messageReceivers ) where

import           Control.Monad      ( when )
import qualified Data.Text as T
import           Discord            ( DiscordHandler
                                    , RestCallErrorCode
                                    )
import           Discord.Types      ( ChannelId
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
                                                   )
                                    )
import           Text.Read          ( readMaybe )
import           UnliftIO           ( liftIO )

import           Owoifier           ( owoify )
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
import           DB                 ( fetch
                                    , store
                                    )


reactionReceivers :: [ReactionInfo -> DiscordHandler ()]
reactionReceivers = [ attemptHallOfFame ]

messageReceivers :: [Message -> DiscordHandler ()]
messageReceivers = [ runCommand reactLimit ]

attemptHallOfFame :: ReactionInfo -> DiscordHandler ()
attemptHallOfFame r =
    when (isHallOfFameEmote r && notInHallOfFameChannel r) $ do
        eligible <- isEligibleForHallOfFame r
        when eligible $ putInHallOfFame r

hallOfFameEmotes :: [T.Text]
hallOfFameEmotes = T.toUpper <$>
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

existsInHOF :: Message -> IO Bool
existsInHOF m = do
    msgIdList <- liftIO $ readSingleColCSV "fame.csv"
    return $ show (messageId m) `elem` (T.unpack <$> msgIdList)

isEligibleForHallOfFame :: ReactionInfo -> DiscordHandler Bool
isEligibleForHallOfFame r = do
    mess <- messageFromReaction r
    limit <- liftIO readLimit
    exists <- liftIO $ existsInHOF mess
    let reactions   = messageReactions mess
    let fulfillCond = \x ->
            T.toUpper (emojiName $ messageReactionEmoji x) `elem` hallOfFameEmotes
            && messageReactionCount x >= limit
            && not exists
    pure $ any fulfillCond reactions

putInHallOfFame :: ReactionInfo -> DiscordHandler ()
putInHallOfFame r = do
    mess <- messageFromReaction r --gets contents of message that was reacted to.
    embed <- createHallOfFameEmbed mess
    msgIdList <- liftIO $ readSingleColCSV "fame.csv"
    liftIO $ writeSingleColCSV "fame.csv" (T.pack (show $ messageId mess):msgIdList)
    --adds the message id to the csv to make sure we dont add it multiple times.
    sendMessageChanEmbed hallOfFameChannel "" embed

createDescription :: Message -> T.Text
createDescription m = messageText m <> "\n- " <> pingAuthorOf m <> " in " <> linkChannel (messageChannel m)

getImageFromMessage :: Message -> T.Text
getImageFromMessage m
  | not . null $ messageAttachments m = attachmentUrl (head $ messageAttachments m)
  | otherwise                         = ""

createHallOfFameEmbed :: Message -> DiscordHandler CreateEmbed
createHallOfFameEmbed m = do
    messLink <- getMessageLink m
    let authorName       = ""
        authorUrl        = ""
        authorIcon       = Nothing
        embedTitle       = "👑 best of ouw buwwshit"
        embedUrl         = ""
        embedThumbnail   = Nothing
        embedDescription = createDescription m
                <> "\n\n[Original Message](" <> messLink <> ")"
        embedFields      = []
        embedImage       = Just $ CreateEmbedImageUrl $ getImageFromMessage m
        embedFooterText  = getTimestampFromMessage m
        embedFooterIcon  = Nothing
    pure $ CreateEmbed authorName
                       authorUrl
                       authorIcon
                       embedTitle
                       embedUrl
                       embedThumbnail
                       embedDescription
                       embedFields
                       embedImage
                       embedFooterText
                       embedFooterIcon

reactLimit :: (MonadDiscord m, MonadIO m) => Command m
reactLimit = requires devPerms $ command "reactLimit" $ \m mbI -> do
    case mbI of
        Nothing -> do
            i <- liftIO readLimit
            respond m $ owoify $ "Current limit is at " <> T.pack (show i)
        Just i -> do
            liftIO $ setLimit i
            respond m $ owoify "New Limit Set as " <> T.pack (show i)

setLimit :: Int -> IO ()
setLimit i = writeSingleColCSV "reactLim.csv" [T.pack $ show i]

readLimit :: IO Int
readLimit = do
    contents <- readSingleColCSV "reactLim.csv"
    if null contents
        then writeSingleColCSV "reactLim.csv" ["1"] >> pure 1
        else pure $ read $ T.unpack $ head contents
