{-# LANGUAGE OverloadedStrings #-}

module Misc (commands, reactionReceivers, changePronouns) where

import              Control.Concurrent      ( threadDelay )
import              Control.Monad           ( when
                                            , unless
                                            , forM_ )
import              Data.Char               ( toUpper )
import qualified    Data.Maybe as M
import qualified    Data.Text.Encoding as E
import qualified    Data.Text.IO as TIO
import qualified    Data.Text as T
import qualified    Network.HTTP.Types as W ( renderQuery )
import              UnliftIO                ( liftIO, UnliftIO (unliftIO) )
import              Text.Regex.TDFA         ( (=~) )
import qualified    System.Process as SP
import              System.Random           ( randomR
                                            , getStdRandom
                                            , randomRIO
                                            )

import qualified    Text.Parsec.Text as T
import              Text.Parsec
import              Discord.Types
import              Discord

import              Command
import              Utils                   ( sendMessageChan
                                            , sendReply
                                            , respondAsset
                                            , addReaction
                                            , messageFromReaction
                                            , (=~=)
                                            , assetDir
                                            , isRoleInGuild
                                            , toMaybe
                                            )
import              Owoifier                ( owoify )

commands :: [Command DiscordHandler]
commands =
    [ owoifyIfPossible
    , fortune
    , godIsDead
    , thatcherIsDead
    , thatcherIsAlive
    , dadJokeIfPossible
    , magic8ball
    , texRender
    ]

reactionReceivers :: [ReactionInfo -> DiscordHandler ()]
reactionReceivers =
    [ forceOwoify ]

-- TODO: put these in config so they can be changed at runtime
owoifyChance, dadJokeChance :: Int
owoifyChance  = 500
dadJokeChance = 5

owoifiedEmoji :: T.Text
owoifiedEmoji = "✅"

-- | Generates a random Int from 1 to n
roll :: Int -> IO Int
roll n = getStdRandom $ randomR (1, n)

-- | Selects an item from a list at random
select :: [a] -> IO a
select xs = (xs !!) . subtract 1 <$> roll (length xs)

rollCheck :: MonadIO m => Int -> m (Maybe T.Text)
rollCheck chance = (`toMaybe` "") <$> ((/=1) <$> liftIO (roll chance))

owoifyIfPossible :: (MonadDiscord m, MonadIO m) => Command m
owoifyIfPossible
    = requires (const $ rollCheck owoifyChance)
    $ regexCommand "[lLrR]|[nNmM][oO]"
    $ \m _ -> do
        sendReply m True $ owoify (messageText m)

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

godIsDead :: (MonadDiscord m, MonadIO m) => Command m
godIsDead = regexCommand "[gG]od *[iI]s *[dD]ead" $ \m _ -> do
    base <- liftIO assetDir
    contents <- liftIO (TIO.readFile $ base <> "nietzsche.txt")
    respond m $ owoify contents

thatcherRE :: T.Text
thatcherRE = "thatcher('s *| *[Ii]s) *"

thatcherIsDead :: (MonadDiscord m) => Command m
thatcherIsDead = regexCommand (thatcherRE <> "[Dd]ead") $ \m _ ->
    respond m "https://www.youtube.com/watch?v=ILvd5buCEnU"

thatcherIsAlive :: (MonadDiscord m, MonadIO m) => Command m
thatcherIsAlive = regexCommand (thatcherRE <> "[Aa]live") $ \m _ ->
    respondAsset m "god_help_us_all.mp4" "god_help_us_all.mp4"

dadJokeIfPossible :: (MonadDiscord m, MonadIO m) => Command m
dadJokeIfPossible =
    requires (const $ rollCheck dadJokeChance)
    $ regexCommand "^[iI] ?[aA]?'?[mM] +([a-zA-Z'*]+)([!;:.,?~-]+| *$)"
    $ \m (name:_) ->
        when (T.length name >= 3) $
            respond m $ owoify ("hello " <> name <> ", i'm owen")

fortune :: (MonadDiscord m, MonadIO m) => Command m
fortune = command "fortune" $ \m -> do
    cowText <- liftIO fortuneCow
    respond m $ "```" <> T.pack cowText <> "```"

fortuneProc :: IO String
fortuneProc = SP.readProcess "fortune" [] []

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
    base <- assetDir
    f <- T.pack <$> fortuneProc
    r <- roll $ length fortuneCowFiles
    let file = if r == 1
        then base <> "freddy.cow"
        else fortuneCowFiles !! (r - 1)
    SP.readProcess "cowsay" ["-f", file] . T.unpack $ owoify f

-- | "Joke" function to change owen's pronouns randomly in servers on startup,
-- cause owen is our favourite genderfluid icon.
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
    forM_ simplifiedMap (uncurry (`removeGuildMemberRole` userId u))

    chosenPronouns <- sequence $ do
        (gid, roles) <- guildPronounMap
        pure $ do
            Just role <- liftIO $ randomChoice roles
            pure (gid, role)

    forM_ chosenPronouns (uncurry (`addGuildMemberRole` userId u))

    where
        randomChoice :: [a] -> IO (Maybe a)
        randomChoice [] = return Nothing
        randomChoice l  = sequence $ Just $ (l !!) <$> randomRIO (0, length l - 1)

-- | List of magic 8-ball responses
-- (from https://en.wikipedia.org/wiki/Magic_8-Ball#Possible_answers)
ballAnswers :: [T.Text]
ballAnswers = map ("🎱 " <>)
    -- Positive responses
    [ "It is Certain."
    , "It is decidedly so."
    , "Without a doubt."
    , "Yes definitely."
    , "You may rely on it."
    , "As I see it, yes."
    , "Most likely."
    , "Outlook good."
    , "Yes."
    , "Signs point to yes."

    -- Non-committal responses
    , "Reply hazy, try again."
    , "Ask again later."
    , "Better not tell you now."
    , "Cannot predict now."
    , "Concentrate and ask again."

    -- Negative responses
    , "Don't count on it."
    , "My reply is no."
    , "My sources say no."
    , "Outlook not so good."
    , "Very doubtful. "
    ]

magic8ball :: (MonadDiscord m, MonadIO m) => Command m
magic8ball = regexCommand "^:8ball.*" $ \m _ -> liftIO (select ballAnswers) >>= respond m

texRender :: (MonadDiscord m) => Command m
texRender = prefix "=" $ command "tex" $ \m (Remaining text) ->
    respond m $ (<>) "https://chart.googleapis.com/chart" $ E.decodeUtf8 $ W.renderQuery True
        [ ("cht", Just "tx")
        , ("chl", Just $ E.encodeUtf8 text)
        , ("chs", Just "100")
        , ("chf", Just "bg,s,00000000")
        , ("chco", Just "d17b46")
        ]
