{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module RoleSelfAssign (reactionAddReceivers, reactionRemReceivers, commands) where

import Control.Monad (forM_, guard, unless)
import Data.Aeson (eitherDecode)
import Data.Bifunctor (bimap, first)
import Data.Char (isDigit)
import Data.Map (Map, toList)
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import UnliftIO (liftIO)

import Data.ByteString.Lazy (fromStrict)

import Discord
import Discord.Types

import Command
import DB
import Owoifier (owoify)
import Utils
    ( addReaction
    , devPerms
    , isEmojiValid
    , isRoleInGuild
    , modPerms
    , sendMessageChan
    , sendMessageDM
    , sentInServer
    )

type EmojiRoleMap = [(String, RoleId)]
-- emoji --> snowflake.
-- Note: Only supports default emojis.

type RoleStation = [(String, EmojiRoleMap, String)]
-- Prepended text, role mapping, appended text.

reactionAddReceivers :: [ReactionInfo -> DiscordHandler ()]
reactionAddReceivers = [attemptRoleAssign]

reactionRemReceivers :: [ReactionInfo -> DiscordHandler ()]
reactionRemReceivers = [handleRoleRemove]

commands :: [Command DiscordHandler]
commands = [createAssignStation, addRoleToStation]

serverID :: GuildId
serverID = 755798054455738489
-- the number is the fixed Guild/Server ID.
-- TODO: put the number in a config file.
-- Currently set to the testing server's.

-- | Warning: Unsafe, as it does not cover cases where the matrix is not two-dimensional.
twoDimMatrixToMap :: Functor f => f [a] -> f (a, a)
twoDimMatrixToMap = fmap (\[x, y] -> (x, y))

mapToMatrix :: Functor f => f (a, a) -> f [a]
mapToMatrix = fmap (\(x, y) -> [x, y])

assignOverviewDB :: DBTable
assignOverviewDB = GuildDB serverID "idAssign"

addRoleToStation :: Command DiscordHandler
addRoleToStation =
    requires (sentInServer <> (modPerms <|> devPerms))
        $ help
            (  "Syntax: `:addRoleToStation <prependText> <appendText>\" "
            <> "<stationID> <channelID> <emoji> <roleText>`"
            )
        $ command "addRoleToStation"
        $ \m prependT appendT stationId channelId emoji role -> do
            doesEmojiExist <- isEmojiValid emoji serverID
            unless
                doesEmojiExist
                (respond
                    m
                    "The emoji provided is invalid. Perhaps you used one from another server?"
                )
            guard doesEmojiExist
            -- Emoji's fine!

            roleIDM <- isRoleInGuild role serverID
            case roleIDM of
                Nothing -> respond
                    m
                    "The role provided is invalid. Please make sure you use the role's name!"
                Just roleID -> do
                    let assignFilePath = getAssignFile' (show stationId)

                    -- The old emote role mapping, read from the CSV.
                    roleEmoteMatrix <- liftIO $ readDB assignOverviewDB
                    -- The new emote role map! Cons the emoji and role at the front.
                    let emojiRoleIDMap = (emoji, roleID) : map
                            (read . T.unpack <$>)
                            (twoDimMatrixToMap roleEmoteMatrix)

                    -- Edit said message to the new one.
                    assignStationT <- formatAssignStation
                        prependT
                        appendT
                        emojiRoleIDMap
                    _ <- editMessage (channelId, stationId) assignStationT Nothing

                    -- Write the new mapping to the old CSV
                    _ <-
                        liftIO
                        .   writeDB (GuildDB serverID "idAssign")
                        .   mapToMatrix
                        $   fmap (T.pack . show)
                        <$> emojiRoleIDMap

                    -- React!
                    addReaction channelId stationId emoji

getAssignFile :: MessageId -> FilePath
getAssignFile = getAssignFile' . show

getAssignFile' :: String -> FilePath
getAssignFile' mid = mid <> ".selfAssign"

roleIdToRole :: RoleId -> [Role] -> T.Text
roleIdToRole rid roles = roleName . head $ filter (\r -> roleId r == rid) roles

formatAssignStation :: T.Text -> T.Text -> [(T.Text, RoleId)] -> DiscordHandler T.Text
formatAssignStation prependT appendT options = do
    roles <- getGuildRoles serverID
    let roleTextOptions =
            (\(emojiT, roleID) -> (emojiT, roleIdToRole roleID roles)) <$> options
    let optionsT =
            (\(emoji, roleName) ->
                    "`[`" <> emoji <> "`]` for " <> "`[`" <> roleName <> "`]`"
                )
                <$> roleTextOptions
    pure $ T.unlines [prependT, "", T.unlines optionsT, appendT]

jsonTxtToMap :: T.Text -> Either String (Map String String)
jsonTxtToMap = eitherDecode . fromStrict . encodeUtf8

createAssignStation :: Command DiscordHandler
createAssignStation =
    requires (sentInServer <> (modPerms <|> devPerms))
        $ help
            (  "Syntax: `:createSelfAssign <prependText> <appendText> "
            <> "{\"emoji1\": \"roleText1\", \"emoji2\": \"roleText2\", ...}`"
            )
        $ command "createSelfAssign"
        $ \m prependT appendT (Remaining emojiRoleJson) -> do
            let emojiRoleMapM = jsonTxtToMap emojiRoleJson
            case emojiRoleMapM of
                Left reason -> do
                    respond m $ "Invalid JSON. " <> T.pack reason
                Right emojiRoleMap' -> do
                    let emojiRoleMap = bimap T.pack T.pack <$> toList emojiRoleMap'
                    doEmojisExist <-
                        and
                            <$> sequence
                                    (   (\(emoji, _) -> isEmojiValid emoji serverID)
                                    <$> emojiRoleMap
                                    )
                    unless
                        doEmojisExist
                        (respond
                            m
                            "One of the emojis provided is invalid. Perhaps you used one from another server?"
                        )
                    guard doEmojisExist
                    -- Emojis are fine!

                    emojiRoleIDMMap <-
                        sequence
                        $   (\(emoji, roleFragment) ->
                              (emoji, ) <$> isRoleInGuild roleFragment serverID
                            )
                        <$> emojiRoleMap
                    let
                        doRolesExist = not $ any
                            (\(_, roleIDM) -> isNothing roleIDM)
                            emojiRoleIDMMap
                    unless
                        doRolesExist
                        (respond
                            m
                            "One of the roles provided is invalid. Please make sure you use the roles' names!"
                        )
                    guard doRolesExist
                    -- Roles are fine!

                    -- The map below is sanitised and perfectly safe to use.
                    let
                        emojiRoleIDMap =
                            (\(emoji, Just roleID) -> (emoji, roleID))
                                <$> emojiRoleIDMMap
                    handleRoleMapping prependT appendT m emojiRoleIDMap

handleRoleMapping
    :: T.Text -> T.Text -> Message -> [(T.Text, RoleId)] -> DiscordHandler ()
handleRoleMapping prependT appendT m emojiRoleIDMap = do
    -- Post the assignment station text.
    assignStationT <- formatAssignStation prependT appendT emojiRoleIDMap
    newMessage     <- createMessage (messageChannel m) assignStationT
    let assignStationID = messageId newMessage

    -- Hence, the map is fine. Write the mapping to the idAssign file :)
    let newFileName     = getAssignFile assignStationID
    -- Write the mapping to the newly added CSV
    _ <-
        liftIO
        .   writeDB (GuildDB serverID newFileName)
        $   (\(key, value) -> [key, T.pack $ show value])
        <$> emojiRoleIDMap

    -- Make Owen react to the self-assignment station.
    let emojiList = fst <$> emojiRoleIDMap
    forM_ emojiList (addReaction (messageChannel newMessage) assignStationID)

    -- Add the new assign file to the CSV
    liftIO $ appendDB
        assignOverviewDB
        [[T.pack $ show assignStationID, T.pack newFileName]]

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
    roleMap     <- liftIO $ getRoleMap (fromJust assFileName)
    let desiredRole = lookup (T.toUpper . emojiName $ reactionEmoji r) roleMap
    guard $ isJust desiredRole
    -- sanity check the desired role (can't be anything other than those which
    -- are specified in the config; for example, emotePronounMap.csv)
    -- NOTE: make sure the emoji names in the config are uppercase.

    let newRoleId = fromJust desiredRole
    addGuildMemberRole serverID (reactionUserId r) newRoleId

    sendMessageDM (reactionUserId r) $ owoify "Added your desired role! Hurray!"

-- | TODO: remove the repetition in handleRoleAssign/handleRoleRemove by
-- modularizing thingies better (i.e., the sanity check).
handleRoleRemove :: ReactionInfo -> DiscordHandler ()
handleRoleRemove r = do
    validMessages <- liftIO getAssignMessageIds
    guard (reactionMessageId r `elem` validMessages)
    -- make sure the message being reacted is a role assignment message
    -- (prevents the config from being opened very often / every sent message)

    assFileName <- liftIO $ getRoleListIndex r
    roleMap     <- liftIO $ getRoleMap (fromJust assFileName)
    let desiredRole = lookup (T.toUpper . emojiName $ reactionEmoji r) roleMap
    guard $ isJust desiredRole
    -- sanity check the desired role (can't be anything other than those which
    -- are specified in the config; for example, emotePronounMap.csv)
    -- NOTE: make sure the emoji names in the config are uppercase.

    let oldRoleId = fromJust desiredRole
    removeGuildMemberRole serverID (reactionUserId r) oldRoleId

    sendMessageDM (reactionUserId r) $ owoify
        "Really sorry you didn't like the role, I went ahead and removed it."

-- | Given a Text @dir@, `getRoleMap` parses the file in "src/config",
-- converting it into a dictionary. One mapping per line. The map is indicated
-- by a comma separating the emoji name (in UPPERCASE) and the corresponding role
-- ID. This has to be wrapped in IO.
getRoleMap :: T.Text -> IO [(T.Text, RoleId)]
getRoleMap dir = do
    contents <- readDB (GuildDB serverID $ T.unpack dir)
    pure $ do
        line <- contents
        pure (head line, (read . T.unpack . head . tail) line)

-- | Given a reaction @r@, `getRoleListIndex` parses the file "src/config/idAssign".
-- Note: this is a special file that represents a dictionary. Again, one mapping per line,
-- with the map indicated by a comma separating the message ID and the corresponding role
-- config file in "src/config". It returns a Maybe type representing the existence of such
-- a config file for the message that is attached to the given reaction.
getRoleListIndex :: ReactionInfo -> IO (Maybe T.Text)
getRoleListIndex r = do
    contents <- readDB assignOverviewDB
    pure . lookup (reactionMessageId r) $ do
        line <- contents
        let pair = (head line, (head . tail) line)
        pure $ first (read . T.unpack) pair

-- | `getAssignMessageIds` returns a list of all message Snowflakes for all messages which
-- represent a self assignment station. All such messages are present in "src/config/idAssign",
-- which is also the only file name that mustn't be edited.
getAssignMessageIds :: IO [MessageId]
getAssignMessageIds = do
    lines <- readDB assignOverviewDB
    pure $ map (read . takeWhile isDigit . T.unpack . head) lines
