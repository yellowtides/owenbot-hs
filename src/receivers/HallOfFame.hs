{-# LANGUAGE OverloadedStrings #-}

module HallOfFame (reactionReceivers, commands) where

import Control.Monad (when)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Discord
import Discord.Types
import UnliftIO (liftIO)

import Command
import DB
import Owoifier (owoify)
import Utils
    ( devPerms
    , modPerms
    , getMessageLink
    , getTimestampFromMessage
    , linkChannel
    , messageFromReaction
    , pingAuthorOf
    , sendMessageChanEmbed
    , sentInServer
    )


reactionReceivers :: [ReactionInfo -> DiscordHandler ()]
reactionReceivers = [attemptHallOfFame]

commands :: [Command DiscordHandler]
commands = [reactLimit, setFameChan]

attemptHallOfFame :: ReactionInfo -> DiscordHandler ()
attemptHallOfFame r = case reactionGuildId r of
    Nothing  -> pure ()
    Just gid -> do
        let fameTable  = GuildDB gid "hallOfFame"
            limitTable = GuildDB gid "reactLimit"
        hofChannel <- liftIO $ readListDB $ GuildDB gid "fameChannel"
        m          <- messageFromReaction r
        case hofChannel of
            [chan] -> do
                let hofChanId = read $ T.unpack chan
                    check1    = isHallOfFameEmote (reactionEmoji r)
                    check2    = reactionChannelId r /= hofChanId
                check3 <- isEligibleForHallOfFame fameTable limitTable m
                when (check1 && check2 && check3)
                    $ putInHallOfFame fameTable hofChanId m
            _ ->
                respond m
                    $  "Hall of Fame has not been set up in this server! "
                    <> "Use :setFameChan to set a channel ID."


hallOfFameEmotes :: [T.Text]
hallOfFameEmotes =
    T.toUpper
        <$> [ "XREE"
            , "KEKW"
            , "\11088" -- star
            ] -- These are matched case-insensitively

hallOfFameChannel :: ChannelId
hallOfFameChannel = 790936269382615082 --the channel id for the hall of fame

isHallOfFameEmote :: Emoji -> Bool
isHallOfFameEmote e = T.toUpper (emojiName e) `elem` hallOfFameEmotes

existsInHOF :: DBTable -> Message -> IO Bool
existsInHOF table m = do
    msgIdList <- liftIO $ readListDB table
    return $ show (messageId m) `elem` (T.unpack <$> msgIdList)

isEligibleForHallOfFame :: DBTable -> DBTable -> Message -> DiscordHandler Bool
isEligibleForHallOfFame fameTable limitTable m = do
    limit  <- liftIO $ readLimit limitTable
    exists <- liftIO $ existsInHOF fameTable m
    let reactions = messageReactions m
    let fulfillCond = \r ->
            isHallOfFameEmote (messageReactionEmoji r)
                && messageReactionCount r
                >= limit
                && not exists
    pure $ any fulfillCond reactions

putInHallOfFame :: DBTable -> ChannelId -> Message -> DiscordHandler ()
putInHallOfFame table hofChan m = do
    embed     <- createHallOfFameEmbed m
    msgIdList <- liftIO $ readListDB table
    liftIO $ writeListDB table (T.pack (show $ messageId m) : msgIdList)
    --adds the message id to the csv to make sure we dont add it multiple times.
    sendMessageChanEmbed hofChan "" embed

createDescription :: Message -> T.Text
createDescription m =
    messageText m <> "\n- " <> pingAuthorOf m <> " in " <> linkChannel
        (messageChannel m)

getImageFromMessage :: Message -> T.Text
getImageFromMessage m
    | not . null $ attachments = attachmentUrl $ head attachments
    | otherwise                = ""
    where attachments = messageAttachments m

createHallOfFameEmbed :: Message -> DiscordHandler CreateEmbed
createHallOfFameEmbed m = do
    messLink <- getMessageLink m
    let authorName     = ""
        authorUrl      = ""
        authorIcon     = Nothing
        embedTitle     = "👑 best of ouw buwwshit"
        embedUrl       = ""
        embedThumbnail = Nothing
        embedDescription =
            createDescription m <> "\n\n[Original Message](" <> messLink <> ")"
        embedFields     = []
        embedImage      = Just $ CreateEmbedImageUrl $ getImageFromMessage m
        embedFooterText = getTimestampFromMessage m
        embedFooterIcon = Nothing
    pure $ CreateEmbed
        authorName
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
reactLimit =
    requires sentInServer
        . requires modPerms
        . help
            "Specify a new limit for this server, or leave empty to see the current limit."
        $ command "reactLimit"
        $ \m mbI -> do
            let gid        = fromJust (messageGuild m)
            let limitTable = GuildDB gid "reactLimit"
            case mbI of
                Nothing -> do
                    i <- liftIO $ readLimit limitTable
                    respond m
                        $  owoify
                        $  "The current react limit for this server is at "
                        <> T.pack (show i)
                Just i -> do
                    liftIO $ setLimit limitTable i
                    respond m
                        $  owoify
                        $  "The new limit for this server set as "
                        <> T.pack (show i)

setLimit :: DBTable -> Int -> IO ()
setLimit limitTable i = writeListDB limitTable [T.pack $ show i]

readLimit :: DBTable -> IO Int
readLimit limitTable = do
    contents <- readListDB limitTable
    if null contents
        then writeListDB limitTable ["1"] >> pure 1
        else pure $ read $ T.unpack $ head contents

setFameChan :: (MonadDiscord m, MonadIO m) => Command m
setFameChan =
    requires sentInServer
        . requires modPerms
        . help "Set the channel for Hall of Fame in this server."
        $ command "setFameChan"
        $ \m chanId -> do
            let gid = fromJust (messageGuild m)
            c <- getChannel chanId
            liftIO $ writeListDB (GuildDB gid "fameChannel") [T.pack $ show chanId]
            respond m
                $  owoify
                $  "I've set the hall of fame channel to `"
                <> channelName c
                <> "`!"
