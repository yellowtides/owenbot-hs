{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module RoleSelfAssign ( reactionAddReceivers, reactionRemReceivers ) where

import           Discord                ( restCall
                                        , DiscordHandler
                                        )
import           Discord.Types          ( Emoji ( emojiName )
                                        , ReactionInfo ( reactionEmoji
                                                       , reactionUserId
                                                       , reactionMessageId
                                                       )
                                        , MessageId
                                        , Message
                                        , RoleId
                                        , GuildId
                                        , messageChannel
                                        )
import           Discord.Requests       ( GuildRequest ( RemoveGuildMemberRole
                                                       , AddGuildMemberRole
                                                       , GetGuildRoles
                                                       ) )
import           Control.Monad          ( guard
                                        , unless )
import           UnliftIO               ( liftIO )
import           Data.Bifunctor         ( first
                                        , bimap
                                        )
import           Data.Char              ( isDigit )
import           Data.Maybe             ( fromJust
                                        , isJust
                                        , isNothing
                                        )
import qualified Data.Text as T         ( toUpper
                                        , unpack
                                        , pack
                                        , Text
                                        , tail
                                        , unlines
                                        )
import          Data.Text.Encoding      ( encodeUtf8 )

import          Data.ByteString.Lazy    ( fromStrict )

import           Owoifier               ( owoify )
import           Utils                  ( sendMessageDM
                                        , newCommand
                                        , isEmojiValid
                                        , isRoleInGuild
                                        , sendMessageChan
                                        )
import           CSV                    ( readCSV
                                        , readSingleColCSV
                                        )

import          TemplateRE              ( accoladedArgRE
                                        , quotedArgRE
                                        , spaceRE
                                        )

import          Data.Map                ( Map
                                        , toList )
import          Data.Aeson              ( decode )

reactionAddReceivers :: [ReactionInfo -> DiscordHandler ()]
reactionAddReceivers = [ attemptRoleAssign ]

reactionRemReceivers :: [ReactionInfo -> DiscordHandler ()]
reactionRemReceivers = [ handleRoleRemove ]

serverID :: GuildId
serverID = 755798054455738489
-- the number is the fixed Guild/Server ID.
-- TODO: put the number in a config file.

createAssignStationSyntax :: T.Text
createAssignStationSyntax = "Syntax: `:createSelfAssign \"<prependText>\" \"<appendText>\" " <>
                            "{\"emoji1\": \"roleText1\", \"emoji2\": \"roleText2\", ...}`"

formatAssignStation :: T.Text -> T.Text -> [(T.Text, RoleId)] -> DiscordHandler T.Text
formatAssignStation prependT appendT options = do
    Right roles <- restCall $ GetGuildRoles serverID
    let optionsT = (\(emoji, roleID) ->
                        "[" <> emoji <> "] for " <> "`[" <> T.pack (show roleID) <> "]`")
                    <$> options
    pure $ T.unlines [
            prependT,
            "",
            T.unlines optionsT,
            "",
            appendT
        ]

createAssignStation :: Message -> DiscordHandler ()
createAssignStation m = newCommand m ("createSelfAssign " <> quotedArgRE <> spaceRE
                                                          <> quotedArgRE <> spaceRE
                                                          <> accoladedArgRE) $ \captures -> do
    -- Captures are [arg1, arg2, json]
    let [prependT, appendT, emojiRoleJson] = captures
    let emojiRoleMapM = decode . fromStrict $ encodeUtf8 emojiRoleJson :: Maybe (Map String String)
    case emojiRoleMapM of
        Nothing           -> sendMessageChan (messageChannel m) $
                                "Invalid JSON. " <> createAssignStationSyntax
        Just emojiRoleMap' -> do
            let emojiRoleMap = bimap T.pack T.pack <$> toList emojiRoleMap'
            doEmojisExist <- and <$> sequence ((\(emoji, _) -> isEmojiValid emoji serverID) <$> emojiRoleMap)
            unless doEmojisExist
                (sendMessageChan (messageChannel m)
                "One of the emojis provided is invalid. Perhaps you used one from another server?")
            -- Emojis are fine!
            emojiRoleIDMMap <- sequence $ (\(emoji, roleFragment) ->
                                (emoji, ) <$> isRoleInGuild roleFragment serverID) <$> emojiRoleMap
            let doRolesExist = not $ any (\(_, roleIDM) -> isNothing roleIDM) emojiRoleIDMMap
            unless doRolesExist
                (sendMessageChan (messageChannel m)
                "One of the roles provided is invalid. Please make sure you use the roles' names!")
            -- Roles are fine!
            -- Hence, the map is fine. Write the mapping to a file :)
            let emojiRoleIDMap = (\(emoji, Just roleID) -> (emoji, roleID)) <$> emojiRoleIDMMap
            assignStationT <- formatAssignStation prependT appendT emojiRoleIDMap
            sendMessageChan (messageChannel m) assignStationT

isOnAssignMessage :: ReactionInfo -> DiscordHandler Bool
isOnAssignMessage r = do
    validMessages <- liftIO getAssignMessageIds
    pure $ reactionMessageId r `elem` validMessages
    -- make sure the message being reacted is a role assignment message
    -- (prevents the config from being opened very often / every sent message)

-- | `attemptRoleAssign` handles role assignments.
attemptRoleAssign :: ReactionInfo -> DiscordHandler ()
attemptRoleAssign r = do
    validMsg <- isOnAssignMessage r
    guard validMsg

    assFileName <- liftIO $ getRoleListIndex r
    roleMap <- liftIO $ getRoleMap (fromJust assFileName)
    let desiredRole = lookup (T.toUpper . emojiName $ reactionEmoji r) roleMap
    guard $ isJust desiredRole
    -- sanity check the desired role (can't be anything other than those which
    -- are specified in the config; for example, emotePronounMap.csv)
    -- NOTE: make sure the emoji names in the config are uppercase.

    let newRoleId = fromJust desiredRole
    restCall $ AddGuildMemberRole serverID (reactionUserId r) newRoleId

    sendMessageDM (reactionUserId r)
        $ owoify "Added your desired role! Hurray!"

-- | TODO: remove the repetition in handleRoleAssign/handleRoleRemove by
-- modularizing thingies better (i.e., the sanity check).
handleRoleRemove :: ReactionInfo -> DiscordHandler ()
handleRoleRemove r = do
    validMessages <- liftIO getAssignMessageIds
    guard (reactionMessageId r `elem` validMessages)
    -- make sure the message being reacted is a role assignment message
    -- (prevents the config from being opened very often / every sent message)

    assFileName <- liftIO $ getRoleListIndex r
    roleMap <- liftIO $ getRoleMap (fromJust assFileName)
    let desiredRole = lookup (T.toUpper . emojiName $ reactionEmoji r) roleMap
    guard $ isJust desiredRole
    -- sanity check the desired role (can't be anything other than those which
    -- are specified in the config; for example, emotePronounMap.csv)
    -- NOTE: make sure the emoji names in the config are uppercase.

    let oldRoleId = fromJust desiredRole
    restCall $ RemoveGuildMemberRole serverID (reactionUserId r) oldRoleId

    sendMessageDM (reactionUserId r)
        $ owoify "Really sorry you didn't like the role, I went ahead and removed it."

-- | Given a Text @dir@, `getRoleMap` parses the file in "src/config",
-- converting it into a dictionary. One mapping per line. The map is indicated
-- by a comma separating the emoji name (in UPPERCASE) and the corresponding role
-- ID. This has to be wrapped in IO.
getRoleMap :: T.Text -> IO [(T.Text, RoleId)]
getRoleMap dir = do
    contents <- readCSV $ T.unpack dir
    pure $ do
        line <- contents
        let pair = (head line, (head . tail) line)
        pure $ fmap (read . T.unpack . T.tail) pair

-- | Given a reaction @r@, `getRoleListIndex` parses the file "src/config/idAssign".
-- Note: this is a special file that represents a dictionary. Again, one mapping per line,
-- with the map indicated by a comma separating the message ID and the corresponding role
-- config file in "src/config". It returns a Maybe type representing the existence of such
-- a config file for the message that is attached to the given reaction.
getRoleListIndex :: ReactionInfo -> IO (Maybe T.Text)
getRoleListIndex r = do
    contents <- readCSV "idAssign.csv"
    pure . lookup (reactionMessageId r) $ do
        line <- contents
        let pair = (head line, (head . tail) line)
        pure . fmap T.tail $ first (read . T.unpack) pair

-- | `getAssignMessageIds` returns a list of all message Snowflakes for all messages which
-- represent a self assignment station. All such messages are present in "src/config/idAssign",
-- which is also the only file name that mustn't be edited.
getAssignMessageIds :: IO [MessageId]
getAssignMessageIds = do
    lines <- readSingleColCSV "idAssign.csv"
    pure $ map (read . takeWhile isDigit . T.unpack) lines
