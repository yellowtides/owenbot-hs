{-# LANGUAGE OverloadedStrings #-}

module Misc (commandReceivers, miscReceivers, reactionReceivers, changePronouns) where

import              Discord.Types
import              Discord
import              Discord.Internal.Rest.User
import              Discord.Internal.Rest.Guild

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
import              System.Random           ( randomR
                                            , getStdRandom
                                            , randomRIO
                                            )


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
            unless (any blockCond reactions || not (any fulfillCond reactions)) $ do
                addReaction (reactionChannelId r) (reactionMessageId r) owoifiedEmoji
                -- Send reply without pinging (this isn't as ping-worthy as random trigger)
                sendReply mess False $ owoify (messageText mess)
        Left err -> liftIO (print err) >> pure ()

godIsDead :: Message -> DiscordHandler ()
godIsDead m = do
    let isMatch = messageText m =~= "[gG]od *[iI]s *[dD]ead"
    when isMatch $ liftIO (TIO.readFile $ assetDir <> "nietzsche.txt")
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
changePronouns :: DiscordHandler ()
changePronouns = do
    Right u <- restCall GetCurrentUser
    -- get partial guilds, don't contain full information, so getId is defined below
    Right pGuilds <- restCall GetCurrentUserGuilds
    let guilds = partialGuildId <$> pGuilds
    let pronouns = ["she/her", "he/him", "they/them"]

    _ <- sendMessageChan (825522165868134440) "reached ln 180"

    guildPronounMapUnfiltered <- sequence $ do
        g <- guilds
        pure $ do
            pronounRoles <- sequence $ (`isRoleInGuild` g) <$> pronouns
            let matchingRoles = M.catMaybes pronounRoles
            void $ liftIO $ print matchingRoles
            -- make sure that pronoun roles exist on server
            -- guard (not $ null matchingRoles)
            pure (g, matchingRoles)

    let guildPronounMap = filter (not . null . snd) guildPronounMapUnfiltered

    _ <- sendMessageChan (825522165868134440) "reached ln 191"

    chosenPronouns <- sequence $ do
        (gid, roles) <- guildPronounMap
        pure $ do
            Just role <- liftIO $ randomChoice roles
            pure (gid, role)

    _ <- sendMessageChan (825522165868134440) "reached ln 199"

    void $ liftIO $ print chosenPronouns
    -- 799452686742323230 = they/them
    -- 799452696083038218 = he/him
    -- 799452697371344966 = she/her

    forM_ chosenPronouns (\(g, r) -> restCall $ AddGuildMemberRole g (userId u) r)

    sendMessageChan (825522165868134440) "reached"


    where
        randomChoice :: [a] -> IO (Maybe a)
        randomChoice [] = return Nothing
        randomChoice l = sequence $ Just $ (l !!) <$> randomRIO (0, length l - 1)

