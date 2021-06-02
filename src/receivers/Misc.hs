{-# LANGUAGE OverloadedStrings #-}

module Misc (commandReceivers, miscReceivers, reactionReceivers, changePronouns) where

import              Discord.Internal.Monad
import              Discord.Types
import              Discord
import              Discord.Internal.Rest.User
import              Discord.Internal.Rest.Guild

import              Data.Char               ( toUpper )
import qualified    Data.Maybe as M
import qualified    Data.Text.IO as TIO
import qualified    Data.Text as T
import qualified    Data.Bifunctor          (second)
import              UnliftIO                ( liftIO, UnliftIO (unliftIO) )
import              Text.Regex.TDFA         ( (=~) )
import qualified    System.Process as SP
import              Control.Monad           ( when
                                            , unless
                                            , void
                                            , join
                                            , guard, forM_ )
import              Control.Monad.IO.Class  ( MonadIO )
import              System.Random           ( randomR
                                            , getStdRandom
                                            , randomRIO
                                            )

import qualified    Text.Parsec.Text as T
import              Text.Parsec
import              Command
import              Utils                   ( sendMessageChan
                                            , sendReply
                                            , sendFileChan
                                            , addReaction
                                            , messageFromReaction
                                            , newCommand
                                            , (=~=)
                                            , assetDir, isRoleInGuild
                                            )
import              Owoifier                ( owoify )

commandReceivers :: [Message -> DiscordHandler ()]
commandReceivers =
    [ handleFortune
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
owoifyChance  = 500
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

-- | Emote names for which to trigger force owoify on. Use All Caps.
forceOwoifyEmotes :: [T.Text]
forceOwoifyEmotes =
    [ "BEGONEBISH" ]

-- | Forces the owofication of a message upon a reaction of forceOwoifyEmotes.
-- Marks it as done with a green check, and checks for its existence to prevent
-- duplicates.
forceOwoify :: ReactionInfo -> DiscordHandler ()
forceOwoify r = do
    mess <- messageFromReaction r
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
    unless (any blockCond reactions || not (any fulfillCond reactions)) $ do
        addReaction (reactionChannelId r) (reactionMessageId r) owoifiedEmoji
        -- Send reply without pinging (this isn't as ping-worthy as random trigger)
        sendReply mess False $ owoify (messageText mess)

godIsDead :: Message -> DiscordHandler ()
godIsDead = runCommand . regexCommand "[gG]od *[iI]s *[dD]ead" $ \m _ ->
    liftIO (TIO.readFile $ assetDir <> "nietzsche.txt")
        >>= sendMessageChan (messageChannel m) . owoify

thatcherRE :: T.Text
thatcherRE = "thatcher('s *| *[Ii]s) *"

thatcherIsDead :: Message -> DiscordHandler ()
thatcherIsDead
    = runCommand
    . regexCommand (thatcherRE <> "[Dd]ead") $ \m _ ->
        sendMessageChan (messageChannel m)
            "https://www.youtube.com/watch?v=ILvd5buCEnU"

thatcherIsAlive :: Message -> DiscordHandler ()
thatcherIsAlive
    = runCommand
    . regexCommand (thatcherRE <> "[Aa]live") $ \m _ ->
        sendFileChan (messageChannel m)
            "god_help_us_all.mp4" $ assetDir <> "god_help_us_all.mp4"

dadJokeIfPossible :: Message -> DiscordHandler ()
dadJokeIfPossible m = do
    let name = attemptParseDadJoke (messageText m)
    when (M.isJust name) $ do
        let n = M.fromJust name
        r <- liftIO $ roll dadJokeChance
        when (r == 1 && T.length n >= 3)
            $ sendMessageChan (messageChannel m)
                $ owoify ("hello " <> n <> ", i'm owen")

attemptParseDadJoke :: T.Text -> Maybe T.Text
attemptParseDadJoke t =
    case captures of
        [] -> Nothing
        _  -> Just (head captures :: T.Text)
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

fortuneCowFiles :: [String]
fortuneCowFiles =
    [ "default"
    , "milk"
    , "moose"
    , "moofasa"
    , "three-eyes"
    , "www"
    ]

fortuneCow :: IO String
fortuneCow = do
    f <- T.pack <$> fortune
    roll <- roll $ length fortuneCowFiles
    let file = if roll == 1
        then assetDir <> "freddy.cow"
        else fortuneCowFiles !! max 0 roll
    SP.readProcess "cowsay" ["-f", file] . T.unpack $ owoify f

-- | "Joke" function to change owen's pronouns randomly in servers on startup, cause owen is our favourite genderfluid icon
changePronouns :: (MonadDiscord m, MonadIO m) => m ()
changePronouns = do
    u <- getCurrentUser
    -- get partial guilds, don't contain full information, so getId is defined below
    pGuilds <- getCurrentUserGuilds
    let guilds = partialGuildId <$> pGuilds
    let pronouns = ["she/her", "he/him", "they/them"]

    guildPronounMapUnfiltered <- sequence $ do
        g <- guilds
        pure $ do
            pronounRoles <- sequence $ (`isRoleInGuild` g) <$> pronouns
            let matchingRoles = M.catMaybes pronounRoles
            pure (g, matchingRoles)

    -- remove guilds without pronoun roles
    let guildPronounMap = filter (not . null . snd) guildPronounMapUnfiltered

    -- remove current pronoun roles
    let simplifiedMap = concat $ sequence <$> guildPronounMap
    forM_ simplifiedMap (\(g, r) -> removeGuildMemberRole g (userId u) r)

    chosenPronouns <- sequence $ do
        (gid, roles) <- guildPronounMap
        pure $ do
            Just role <- liftIO $ randomChoice roles
            pure (gid, role)

    forM_ chosenPronouns (\(g, r) -> addGuildMemberRole g (userId u) r)

    where
        randomChoice :: [a] -> IO (Maybe a)
        randomChoice [] = return Nothing
        randomChoice l = sequence $ Just $ (l !!) <$> randomRIO (0, length l - 1)

