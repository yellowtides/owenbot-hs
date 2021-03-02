{-# LANGUAGE OverloadedStrings #-}

module HallOfFame ( reactionReceivers, messageReceivers ) where

import qualified Discord.Requests as R
import qualified Data.Text as T         ( pack
                                        , toUpper
                                        , Text
                                        )
import           Data.Functor           ( (<&>) )
import           Discord                ( restCall
                                        , DiscordHandler
                                        , RestCallErrorCode
                                        )
import           Discord.Types          ( ChannelId
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
import           UnliftIO               ( liftIO )
import           Control.Monad          ( guard, when )
import           Utils                  ( sendMessageChan
                                        , isSenderDeveloper
                                        , pingAuthorOf
                                        , linkChannel
                                        , getMessageLink
                                        , sendMessageChanEmbed
                                        , getTimestampFromMessage
                                        , openCSV
                                        , addToCSV
                                        , rmFuncText
                                        )


reactionReceivers :: [ReactionInfo -> DiscordHandler ()]
reactionReceivers = [ attemptHallOfFame ]

messageReceivers :: [Message -> DiscordHandler ()]
messageReceivers = []

attemptHallOfFame :: ReactionInfo -> DiscordHandler ()
attemptHallOfFame r = do
    when (isHallOfFameEmote r && notInHallOfFameChannel r) $ do
        eligible <- isEligibleForHallOfFame r
        liftIO $ putStrLn ("aaaaaa" ++ show eligible)
        when eligible $ putInHallOfFame r >> pure ()

hallOfFameEmotes :: [T.Text]
hallOfFameEmotes = ["xree", "kekw", "\11088"]

hallOfFameChannel :: ChannelId
hallOfFameChannel = 790936269382615082 --the channel id for the hall of fame

notInHallOfFameChannel :: ReactionInfo -> Bool
notInHallOfFameChannel r = reactionChannelId r /= hallOfFameChannel

isHallOfFameEmote :: ReactionInfo -> Bool
isHallOfFameEmote r = T.toUpper (emojiName (reactionEmoji r)) `elem` hallOfFameEmotes

messageFromReaction :: ReactionInfo -> DiscordHandler (Either RestCallErrorCode Message)
messageFromReaction r = restCall 
    $ R.GetChannelMessage (reactionChannelId r, reactionMessageId r)

isEligibleForHallOfFame :: ReactionInfo -> DiscordHandler Bool
isEligibleForHallOfFame r = do
    messM <- messageFromReaction r
    case messM of
        Right mess -> do
            msgIdlist <- liftIO $ openCSV "fame.csv"
            limit <- liftIO readLimit
            let reactions = messageReactions mess
            let fulfillCond = \x -> 
                    (messageReactionCount x) >= limit
                    && T.toUpper (emojiName $ messageReactionEmoji x) `elem` map T.upper hallOfFameEmotes
                    && show (messageId mess) `notElem` msgIdlist
            pure $ any fulfillCond reactions
        Left err -> liftIO (putStrLn (show err)) >> pure False    

putInHallOfFame :: ReactionInfo -> DiscordHandler ()
putInHallOfFame r = do
    messM <- messageFromReaction r --gets contents of message that was reacted to.
    case messM of
        Right mess -> do
            embedM <- createHallOfFameEmbed mess
            case embedM of
                Right embed -> do
                    liftIO $ addToCSV "fame.csv" (show (messageId mess) ++ ", ") --adds the message id to the csv to make sure we dont add it multiple times.
                    sendMessageChanEmbed hallOfFameChannel "" embed
                Left err -> liftIO (putStrLn "Couldn't get link to message") >> pure ()
        Left err -> liftIO (putStrLn "Couldn't find associated message") >> pure ()

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
            let embedDescription = (createDescription m <> "\n\n[Original Message](" <> messLink <> ")")
            let embedFields = []
            let embedImage = (Just (CreateEmbedImageUrl $ getImageFromMessage m))
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

-- setLimit :: Message -> Int -> IO()
-- setLimit :: Message -> Int -> DiscordHandler ()
setLimit :: Message -> Int -> DiscordHandler ()
setLimit m i = do
  isDev <- isSenderDeveloper m
  if isDev then do
      liftIO $ editLimit i
      sendMessageChan (messageChannel m) "New Limit Set"
    else sendMessageChan (messageChannel m ) "Insufficient Priveledges"

editLimit :: Int -> IO()
editLimit = writeFile "src/config/reactLim.conf" . show

readLimit :: IO Int
readLimit = openCSV "src/config/reactLim.conf" <&> (read . head)
