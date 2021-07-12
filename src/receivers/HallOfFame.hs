{-# LANGUAGE OverloadedStrings #-}

module HallOfFame ( reactionReceivers, commands ) where

import           Control.Monad      ( when )
import qualified Data.Text as T
import           Discord
import           Discord.Types
import           UnliftIO           ( liftIO )

import           Owoifier           ( owoify )
import           Utils              ( pingAuthorOf
                                    , messageFromReaction
                                    , linkChannel
                                    , getMessageLink
                                    , sendMessageChanEmbed
                                    , getTimestampFromMessage
                                    , devPerms
                                    )
import           Command
import           CSV                ( readSingleColCSV
                                    , writeSingleColCSV
                                    )
import           DB                 ( readDB
                                    , writeDB
                                    )


reactionReceivers :: [ReactionInfo -> DiscordHandler ()]
reactionReceivers = [ attemptHallOfFame ]

commands :: [Command DiscordHandler]
commands = [ reactLimit ]

attemptHallOfFame :: ReactionInfo -> DiscordHandler ()
attemptHallOfFame r =
    when (isHallOfFameEmote (reactionEmoji r) && notInHallOfFameChannel r) $ do
        m <- messageFromReaction r
        eligible <- isEligibleForHallOfFame m
        when eligible $ putInHallOfFame m

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

isHallOfFameEmote :: Emoji -> Bool
isHallOfFameEmote e = T.toUpper (emojiName e) `elem` hallOfFameEmotes

existsInHOF :: Message -> IO Bool
existsInHOF m = do
    msgIdList <- liftIO $ readSingleColCSV "fame.csv"
    return $ show (messageId m) `elem` (T.unpack <$> msgIdList)

isEligibleForHallOfFame :: Message -> DiscordHandler Bool
isEligibleForHallOfFame m = do
    limit <- liftIO readLimit
    exists <- liftIO $ existsInHOF m
    let reactions   = messageReactions m
    let fulfillCond = \r ->
            isHallOfFameEmote (messageReactionEmoji r)
            && messageReactionCount r >= limit
            && not exists
    pure $ any fulfillCond reactions

putInHallOfFame :: Message -> DiscordHandler ()
putInHallOfFame m = do
    embed <- createHallOfFameEmbed m
    msgIdList <- liftIO $ readSingleColCSV "fame.csv"
    liftIO $ writeSingleColCSV "fame.csv" (T.pack (show $ messageId m):msgIdList)
    --adds the message id to the csv to make sure we dont add it multiple times.
    sendMessageChanEmbed hallOfFameChannel "" embed

createDescription :: Message -> T.Text
createDescription m = messageText m
                   <> "\n- "
                   <> pingAuthorOf m
                   <> " in "
                   <> linkChannel (messageChannel m)

getImageFromMessage :: Message -> T.Text
getImageFromMessage m
    | not . null $ attachments = attachmentUrl $ head attachments
    | otherwise                  = ""
    where attachments = messageAttachments m

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
            respond m $ owoify $ "New Limit Set as " <> T.pack (show i)

setLimit :: Int -> IO ()
setLimit i = writeSingleColCSV "reactLim.csv" [T.pack $ show i]

readLimit :: IO Int
readLimit = do
    contents <- readSingleColCSV "reactLim.csv"
    if null contents
        then writeSingleColCSV "reactLim.csv" ["1"] >> pure 1
        else pure $ read $ T.unpack $ head contents
