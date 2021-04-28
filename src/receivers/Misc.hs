{-# LANGUAGE OverloadedStrings #-}

module Misc ( commandReceivers, miscReceivers, reactionReceivers ) where

import qualified Discord.Requests as R
import           Discord.Types
import           Discord
import qualified Data.Maybe as M
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import           UnliftIO               ( liftIO )
import           Text.Regex.TDFA        ( (=~) )
import           System.IO as S         ( readFile )
import qualified System.Exit as SE
import qualified System.Process as SP
import           Control.Monad          ( when )
import           System.Random          ( randomR
                                        , getStdRandom
                                        )

import           Utils                  ( sendMessageChan
                                        , sendReply
                                        , sendFileChan
                                        , addReaction
                                        , messageFromReaction
                                        , pingAuthorOf
                                        , newCommand
                                        , (=~=)
                                        )
import           Owoifier               ( owoify )
import           ADAPriceFetcher    ( fetchTicker
                                        , fetchADADetails)

commandReceivers :: [Message -> DiscordHandler ()]
commandReceivers =
    [ handleFortune
    , handleTicker
    , handleAda24h
    ]

miscReceivers :: [Message -> DiscordHandler ()]
miscReceivers =
    [ owoifyIfPossible
    , godIsDead
    , thatcherIsDead
    , thatcherIsAlive
    , dadJokeIfPossible
    ]

reactionReceivers :: [ReactionInfo -> DiscordHandler ()]
reactionReceivers =
    [ forceOwoify ]

-- TODO: put these in config so they can be changed at runtime
owoifyChance, dadJokeChance :: Int
owoifyChance = 500
dadJokeChance = 20

owoifiedEmoji :: T.Text
owoifiedEmoji = "✅"

roll :: Int -> IO Int
roll n = getStdRandom $ randomR (1, n)

owoifyIfPossible :: Message -> DiscordHandler ()
owoifyIfPossible m = do
    r <- liftIO $ roll owoifyChance
    let isOwoifiable = messageText m =~= "[lLrR]|[nNmM][oO]"
    when (r == 1 && isOwoifiable)
        $ sendReply m True $ owoify (messageText m)

-- | Emote names for which to trigger force owoify on. All caps.
forceOwoifyEmotes :: [T.Text]
forceOwoifyEmotes =
    [ "BEGONEBISH" ]

-- | Forces the owofication of a message upon a reaction of forceOwoifyEmotes.
-- Marks it as done with a green check, and checks for its existence to prevent
-- duplicates.
forceOwoify :: ReactionInfo -> DiscordHandler ()
forceOwoify r = do
    messM <- messageFromReaction r
    case messM of
        Right mess -> do
            -- Get all reactions for the associated message
            let reactions = messageReactions mess
            -- Define conditions that stops execution
            let blockCond = \x ->
                    messageReactionMeIncluded x
                    && emojiName (messageReactionEmoji x) == owoifiedEmoji
            -- Conditions that say go!
            let fulfillCond = \x ->
                    T.toUpper (emojiName $ messageReactionEmoji x) `elem` forceOwoifyEmotes
            -- If all goes good, add a checkmark and send an owoification reply
            if any blockCond reactions || not (any fulfillCond reactions) then
                pure ()
            else do
                addReaction (reactionChannelId r) (reactionMessageId r) owoifiedEmoji
                -- Send reply without pinging (this isn't as ping-worthy as random trigger)
                sendReply mess False $ owoify (messageText mess)
        Left err -> liftIO (print err) >> pure ()

godIsDead :: Message -> DiscordHandler ()
godIsDead m = do
    let isMatch = messageText m =~= "[gG]od *[iI]s *[dD]ead"
    when isMatch $ liftIO (TIO.readFile "./src/assets/nietzsche.txt")
            >>= sendMessageChan (messageChannel m) . owoify

thatcherRE :: T.Text
thatcherRE = "thatcher('s *| *[Ii]s) *"

thatcherIsDead :: Message -> DiscordHandler ()
thatcherIsDead m = when (messageText m =~= (thatcherRE <> "[Dd]ead"))
        $ sendMessageChan (messageChannel m)
            "https://www.youtube.com/watch?v=ILvd5buCEnU"

thatcherIsAlive :: Message -> DiscordHandler ()
thatcherIsAlive m = when (messageText m =~= (thatcherRE <> "[Aa]live"))
        $ sendFileChan (messageChannel m)
            "god_help_us_all.mp4" "./src/assets/god_help_us_all.mp4"

dadJokeIfPossible :: Message -> DiscordHandler ()
dadJokeIfPossible m = do
    let name = attemptParseDadJoke (messageText m)
    when (M.isJust name) $ do
        let Just n = name
        r <- liftIO $ roll dadJokeChance
        when (r == 1 && T.length n >= 3)
            $ sendMessageChan (messageChannel m)
                $ owoify ("hello " <> n <> ", i'm owen")

attemptParseDadJoke :: T.Text -> Maybe T.Text
attemptParseDadJoke t =
    case captures of
        [] -> Nothing
        e  -> Just (head captures :: T.Text)
  where
    match :: (T.Text, T.Text, T.Text, [T.Text])
    match@(_, _, _, captures) =
        t =~ ("^[iI] ?[aA]?'?[mM] +([a-zA-Z'*]+)([!;:.,?~-]+| *$)" :: T.Text)

handleFortune :: Message -> DiscordHandler ()
handleFortune m = newCommand m "fortune" $ \_ -> do
    cowText <- liftIO fortuneCow
    sendMessageChan (messageChannel m)
        $ "```" <> T.pack cowText <> "```"

fortune :: IO String
fortune = SP.readProcess "fortune" [] []

fortuneCow :: IO String
fortuneCow = do
    f <- T.pack <$> fortune
    SP.readProcess "cowsay" [] . T.unpack $ owoify f

handleTicker :: Message -> DiscordHandler ()
handleTicker m = newCommand m "binance ([A-Z]+) ([A-Z]+)" $ \symbol -> do
    let [base, quote] = T.unpack <$> symbol
    announcementM <- liftIO $ fetchTicker base quote
    case announcementM of
         Left err -> do
            liftIO (putStrLn $ "Cannot get ticker from binance: " ++ err)
            sendMessageChan (messageChannel m)
                $ owoify "Couldn't get the data! Sorry"
         Right announcement ->
            sendMessageChan (messageChannel m)
                $ owoify . T.pack $ base <> "/" <> quote <> " is "
                                 <> announcement

handleAda24h :: Message -> DiscordHandler ()
handleAda24h m = newCommand m "ada24h" $ \_ -> do
    adaAnnouncementM <- liftIO fetchADADetails
    case adaAnnouncementM of
        Left err -> do
            liftIO (putStrLn $ "Cannot fetch ADA details from Binance: " ++ err)
            sendMessageChan (messageChannel m)
                $ owoify "Couldn't get the data! Sorry"
        Right announcement ->
            sendMessageChan (messageChannel m)
                $ owoify $ T.pack announcement
