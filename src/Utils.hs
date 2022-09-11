{-# LANGUAGE OverloadedStrings #-}

{-|
    Module:     : Utils
    Description : A module containing all sorts of useful macros and functions. The Appendix of owenbot.
-}
module Utils
    ( emojiToUsableText
    , sendMessageChan
    , sendReply
    , sendMessageChanEmbed
    , sendMessageChanPingsDisabled
    , sendMessageDM
    , respond
    , sendFileChan
    , respondAsset
    , addReaction
    , messageFromReaction
    , pingUser
    , pingRole
    , pingAuthorOf
    , pingWithUsername
    , stripAllPings
    , linkChannel
    , getMessageLink
    , hasRoleByName
    , hasRoleByID
    , roleNameIn
    , modPerms
    , devPerms
    , sentInServer
    , assetDir
    , moveChannel
    , isEmojiValid
    , isRoleInGuild
    , toMaybe
    ) where

import Control.Exception (IOException)
import Control.Monad (join, unless, void)
import qualified Data.ByteString as B
import Data.Char (isDigit)
import Data.Function (on)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Data.Time.Format as TF
import Discord
import Discord.Requests
import Discord.Types
import System.Directory (XdgDirectory(XdgData), getXdgDirectory)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.Process as Process
import Text.Regex.TDFA ((=~))
import UnliftIO (liftIO)

import Command
import Config
import DB
import Owoifier (owoify, weakOwoify)

-- | The `FilePath` representing the location of the assets.
-- TODO: Move into a saner place than Utils
assetDir :: IO FilePath
assetDir = liftIO $ getXdgDirectory XdgData "owen/assets/"

toMaybe :: Bool -> a -> Maybe a
toMaybe cond a = if cond then Just a else Nothing

-- | `pingUser` constructs a minimal `Text` pinging the given user.
pingUser :: User -> T.Text
pingUser u = "<@" <> T.pack (show $ userId u) <> ">"

-- | `pingRole` constructs a minimal `Text` pinging the given role id.
pingRole :: RoleId -> T.Text
pingRole r = "<@&" <> T.pack (show r) <> ">"

-- | `pingAuthorOf` constructs a minimal `Text` pinging the author of a given message.
pingAuthorOf :: Message -> T.Text
pingAuthorOf = pingUser . messageAuthor

-- | `pingWithUsername` constructs a minimal `Text` pinging the the user with the given
-- username from the given guild. On failure, returns an empty Text. On multiple such
-- users, returns an empty Text.
pingWithUsername :: T.Text -> GuildId -> DiscordHandler T.Text
pingWithUsername uname gid = do
    let timing = GuildMembersTiming (Just 1000) Nothing
    -- number of users to fetch, maximum is 1000
    -- "Nothing" seems to default to (Just 1)
    membersM <- restCall $ ListGuildMembers gid timing
    let members = case membersM of
            Right ms -> ms
            Left  _  -> []
    let users             = fromJust . memberUser <$> members
    --_ <- liftIO $ print (userName <$> users)
    let usersWithUsername = filter ((uname ==) . userName) users
    --_ <- liftIO $ print usersWithUsername
    pure $ case usersWithUsername of
        [u] -> pingUser u
        _   -> ""

-- | `isUnicodeEmoji` determines whether a provided character is a unicode emoji.
-- Really weak check.
isUnicodeEmoji :: T.Text -> Bool
isUnicodeEmoji emojiT = all isInEmojiBlock (filter (/= ' ') $ T.unpack emojiT)
    where isInEmojiBlock c = c >= '\x00A9' && c <= '\x1FADF'

-- | `isRoleInGuild` determines whether a role containing the given text exists
-- in the guild (case insensitive). If it does, then it returns the role's ID.
-- Otherwise, `Nothing` is returned.
isRoleInGuild :: T.Text -> GuildId -> DiscordHandler (Maybe RoleId)
isRoleInGuild roleFragment gid = do
    roles <- call $ GetGuildRoles gid
    let matchingRoles = filter
            ((T.toUpper roleFragment `T.isInfixOf`) . T.toUpper . roleName)
            roles
    pure $ roleId <$> listToMaybe matchingRoles

-- | `discordEmojiTextToId` takes a Text ending in a Discord <::0-9> formatted emoji string
-- and extracts the ID. On unsuccessful extraction, returns 0. Given Text can be trailed by
-- spaces.
discordEmojiTextToId :: T.Text -> EmojiId
discordEmojiTextToId emojiT = case idT of
    ""  -> 0
    num -> read num
  where
    idT =
        T.unpack
            . T.reverse
            . T.takeWhile isDigit
            . T.drop 1
            . T.dropWhile (== ' ')
            $ T.reverse emojiT

-- | `isEmojiValid` determines whether an emoji (provided in Discord <::0-9> format) exists in the
-- guild (or is a default emoji). Case insensitive.
isEmojiValid :: T.Text -> GuildId -> DiscordHandler Bool
isEmojiValid emojiT gid = do
    guild <- call $ GetGuild gid
    let emojis         = guildEmojis guild
    let emojiID        = discordEmojiTextToId emojiT
    let matchingEmojis = filter ((emojiID ==) . fromJust . emojiId) emojis
    -- _ <- liftIO $ print matchingEmojis
    -- _ <- liftIO $ print emojiTID
    let isInvalid = case (emojiT, matchingEmojis) of
            ("", _ ) -> True
            (_ , []) -> not $ isUnicodeEmoji emojiT
            _        -> False
    pure $ not isInvalid

-- | `converge` applies a function to a variable until the result converges.
converge :: Eq a => (a -> a) -> a -> a
converge = (>>= (==)) >>= until

-- | `stripAllPings` removes all pings from a given `Text` message.
stripAllPings :: T.Text -> T.Text
stripAllPings = T.pack . converge stripOnePing . T.unpack
  where
    pingRE :: String
    pingRE = "^@[&!]?[0-9]{8,}>"
    stripOnePing :: String -> String
    stripOnePing []   = []
    stripOnePing [ch] = [ch]
    stripOnePing ('<' : xs) =
        if xs =~ pingRE then drop 1 $ dropWhile (/= '>') xs else '<' : xs
    stripOnePing (x : xs) = x : stripOnePing xs

-- | `linkChannel` constructs a minimal `Text` linking the channel with the provided ID.
linkChannel :: ChannelId -> T.Text
linkChannel c = "<#" <> T.pack (show c) <> ">"

-- | `getMessageLink` attempts to construct the Discord URL of the given message, as a `Text`.
getMessageLink :: Message -> DiscordHandler T.Text
getMessageLink m = do
    chan <- call $ GetChannel (messageChannelId m)
    -- the ID of the server containing the channel, as a `Text`
    let serverIDT  = T.pack . show $ channelGuild chan
    -- the ID of the channel the message was sent in, as a `Text`
    let channelIDT = T.pack . show $ messageChannelId m
    -- the messageID, as a `Text`
    let messageIDT = T.pack . show $ messageId m
    pure
        $ T.concat
            [ "https://discord.com/channels/"
            , serverIDT
            , "/"
            , channelIDT
            , "/"
            , messageIDT
            ]

-- | `emojiToUsableText` converts a given emoji to a text which can be used to display it in Discord.
emojiToUsableText :: Emoji -> T.Text
emojiToUsableText r = case emojiId r of
    Nothing -> name
    Just id -> "<:" <> name <> ":" <> T.pack (show id) <> ">"
    where name = emojiName r

-- | `sendMessageChan` attempts to send the given `Text` in the channel with the given
-- `channelID`. Surpesses any error message(s), returning `()`.
sendMessageChan :: ChannelId -> T.Text -> DiscordHandler ()
sendMessageChan c xs = void $ call $ CreateMessage c xs

-- | @respond@ responds to a message. It does not use the Reply feature. For that,
-- use 'sendReply'.
respond :: Message -> T.Text -> DiscordHandler ()
respond m = void . call . CreateMessage (messageChannelId m)

-- | `sendMessageChanPingsDisabled` acts in the same way as `sendMessageChan`, but disables
-- all pings (@everyone, @user, @role) pings from the message.
sendMessageChanPingsDisabled :: ChannelId -> T.Text -> DiscordHandler ()
sendMessageChanPingsDisabled cid t = void $ call $ CreateMessageDetailed
    cid
    def
        { messageDetailedContent         = t
        , messageDetailedAllowedMentions = Just $ def
            { mentionEveryone = False
            , mentionUsers    = False
            , mentionRoles    = False
            }
        }

-- | `sendReply` attempts to send a reply to the given `Message`. Suppresses any error
-- message(s), returning `()`.
sendReply :: Message -> Bool -> T.Text -> DiscordHandler ()
sendReply m mention xs = void $ call $ CreateMessageDetailed (messageChannelId m) $ def
    { messageDetailedContent = xs
    , messageDetailedReference = Just $ def { referenceMessageId = Just $ messageId m }
    , messageDetailedAllowedMentions = Just $ def { mentionRepliedUser = mention }
    }

-- | `sendMessageChanEmbed` attempts to send the given embed with the given `Text` in the
-- channel with the given `channelID`. Surpesses any error message(s), returning `()`.
sendMessageChanEmbed :: ChannelId -> T.Text -> CreateEmbed -> DiscordHandler ()
sendMessageChanEmbed c xs e = void $ call $ CreateMessageDetailed c $ def
    { messageDetailedContent = xs
    , messageDetailedEmbeds  = Just [e]
    }


-- | `sendMessageDM` attempts to send the given `Text` as a direct message to the user with the
-- given `UserId`. Surpresses any error message(s), returning `()`.
sendMessageDM :: UserId -> T.Text -> DiscordHandler ()
sendMessageDM u t = call (CreateDM u) >>= (flip sendMessageChan t . channelId)

-- | `sendFileChan` attempts to send the file at the provided `FilePath` in the channel with the
-- provided `ChannelId`. The file attachment is annotated by the given `Text`.
sendFileChan :: ChannelId -> T.Text -> FilePath -> DiscordHandler ()
sendFileChan c name fp = do
    bytes <- liftIO $ B.readFile fp
    void . call $ CreateMessageDetailed c $ def
        { messageDetailedFile = Just (name, bytes)
        }

-- | @respondAsset m name path@ responds to the message @m@ with the file at
-- @path@, with the name overridden as @name@.
respondAsset :: Message -> T.Text -> FilePath -> DiscordHandler ()
respondAsset m name path = do
    base <- liftIO assetDir
    sendFileChan (messageChannelId m) name (base <> path)

-- | `messageFromReaction` attempts to get the Message instance from a reaction.
messageFromReaction :: ReactionInfo -> DiscordHandler Message
messageFromReaction r =
    call $ GetChannelMessage (reactionChannelId r, reactionMessageId r)

-- | `addReaction` attempts to add a reaction to the given message ID. Supresses any
-- error message(s), returning `()`.
addReaction :: ChannelId -> MessageId -> T.Text -> DiscordHandler ()
addReaction c m t = void $ call $ CreateReaction (c, m) t

-- | @hasRoleBy f roles r@ checks whether @map f roles@ contains @r@.
hasRoleBy :: Eq a => (Role -> a) -> [Role] -> a -> Bool
hasRoleBy f roles r = any ((== r) . f) roles

hasRoleByName :: [Role] -> T.Text -> Bool
hasRoleByName = hasRoleBy roleName

hasRoleByID :: [Role] -> RoleId -> Bool
hasRoleByID = hasRoleBy roleId

getRoles :: Message -> DiscordHandler [Role]
getRoles m = case messageGuildId m of
    Nothing -> pure []
    Just g  -> getRolesOfUserInGuild (userId $ messageAuthor m) g

-- | Gets the list of dev roles from the db.
getDevs :: IO [RoleId]
getDevs = map (read . T.unpack) . owenConfigDevs <$> readConfig

-- | `isSenderDeveloper` checks whether the provided message's author is a dev.
isSenderDeveloper :: Message -> DiscordHandler Bool
isSenderDeveloper m = do
    d  <- liftIO getDevs
    rs <- getRoles m
    pure $ any (hasRoleByID rs) d

permCheck :: DiscordHandler Bool -> T.Text -> DiscordHandler (Either T.Text ())
permCheck check reason = do
    result <- check
    pure $ if result then Right () else Left reason

roleNameIn :: [T.Text] -> Requirement DiscordHandler ()
roleNameIn names = Requirement $ \msg -> do
    call $ TriggerTypingIndicator (messageChannelId msg)
    let check = getRoles msg >>= \rs -> pure $ any (hasRoleByName rs) names
    permCheck check $ "Need to have one of: " <> (T.pack . show) names

modPerms :: Requirement DiscordHandler ()
modPerms = roleNameIn ["Admin", "Mod", "Moderator"]

-- | Command requirement for sender being a registered developer.
devPerms :: Requirement DiscordHandler ()
devPerms =
    Requirement $ \msg -> permCheck (isSenderDeveloper msg) "Need to be an OwenDev"

sentInServer :: Requirement DiscordHandler ()
sentInServer = Requirement $ \msg ->
    pure $ maybe (Left "Need to be sent in server!") (const $ Right ()) $ messageGuildId
        msg


-- | `getRolesOfUserInGuild` fetches a list of roles partaining to the user with the given `UserId`
-- within the guild with the given `GuildId`.
getRolesOfUserInGuild :: UserId -> GuildId -> DiscordHandler [Role]
getRolesOfUserInGuild uid g = do
    allGuildRoles <- call $ GetGuildRoles g
    user          <- call $ GetGuildMember g uid
    pure $ filter ((`elem` memberRoles user) . roleId) allGuildRoles

-- | Moves channel position in guild
moveChannel :: GuildId -> ChannelId -> Int -> DiscordHandler ()
moveChannel guild chan location =
    void $ call $ ModifyGuildChannelPositions guild [(chan, location)]

