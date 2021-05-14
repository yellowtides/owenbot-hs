{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module RoleSelfAssign ( reactionAddReceivers, reactionRemReceivers, receivers ) where

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
                                        , messageId
                                        , RoleId
                                        , Role ( roleName, roleId )
                                        , GuildId
                                        , messageChannel
                                        )
import           Discord.Requests       ( GuildRequest ( RemoveGuildMemberRole
                                                       , AddGuildMemberRole
                                                       , GetGuildRoles
                                                       )
                                        , ChannelRequest ( CreateMessage
                                                         , EditMessage )
                                        )
import           Control.Monad          ( guard
                                        , unless
                                        , forM_ )
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
                                        , newModCommand
                                        , isEmojiValid
                                        , isRoleInGuild
                                        , sendMessageChan
                                        , addReaction
                                        )
import           CSV                    ( readCSV
                                        , readSingleColCSV
                                        , addToCSV
                                        , writeCSV
                                        )

import          TemplateRE              ( accoladedArgRE
                                        , quotedArgRE
                                        , spaceRE
                                        )

import          Data.Map                ( Map
                                        , toList )
import          Data.Aeson              ( decode )

type EmojiRoleMap = [(String, RoleId)]
-- emoji --> snowflake.
-- Note: Only supports default emojis.

type RoleStation = [(String, EmojiRoleMap, String)]
-- Prepended text, role mapping, appended text.

reactionAddReceivers :: [ReactionInfo -> DiscordHandler ()]
reactionAddReceivers = [ attemptRoleAssign ]

reactionRemReceivers :: [ReactionInfo -> DiscordHandler ()]
reactionRemReceivers = [ handleRoleRemove ]

receivers :: [Message -> DiscordHandler ()]
receivers =
    [ createAssignStation
    , addRoleToStation
    ]

assignFilePath :: FilePath
assignFilePath = "idAssign.csv"

serverID :: GuildId
serverID = 768810076201811989
-- the number is the fixed Guild/Server ID.
-- TODO: put the number in a config file.
-- Currently set to the testing server's.

createAssignStationSyntax :: T.Text
createAssignStationSyntax = "Syntax: `:createSelfAssign \"<prependText>\" \"<appendText>\" " <>
                            "{\"emoji1\": \"roleText1\", \"emoji2\": \"roleText2\", ...}`"

addRoleToStationSyntax :: T.Text
addRoleToStationSyntax = "Syntax: `:addRoleToStation \"<prependText>\" \"<appendText>\" " <>
                            "\"<stationID>\" \"<channelID>\" \"<emoji>\" \"<roleText>\"`"

-- | Warning: Unsafe, as it does not cover cases where the matrix is not two-dimensional.
twoDimMatrixToMap :: [[a]] -> [(a, a)]
twoDimMatrixToMap = map (\[x, y] -> (x, y))

mapToMatrix :: [(a, a)] -> [[a]]
mapToMatrix = map (\(x, y) -> [x, y])

addRoleToStation :: Message -> DiscordHandler ()
addRoleToStation m = newModCommand m ("addRoleToStation" <> spaceRE             -- command name
                                          <> quotedArgRE <> spaceRE             -- prepended text
                                          <> quotedArgRE <> spaceRE             -- appended text
                                          <> quotedArgRE <> spaceRE             -- station ID
                                          <> quotedArgRE <> spaceRE             -- channel ID
                                          <> quotedArgRE <> spaceRE             -- emoji
                                          <> quotedArgRE) $ \captures -> do     -- role
    let [prependT, appendT, stationID, channelID, emoji, role] = captures
    doesEmojiExist <- isEmojiValid emoji serverID
    unless doesEmojiExist
        (sendMessageChan (messageChannel m)
        "The emoji provided is invalid. Perhaps you used one from another server?")
    guard doesEmojiExist
    -- Emoji's fine!

    roleIDM <- isRoleInGuild role serverID
    let doesRoleExist = isJust roleIDM
    unless doesRoleExist
        (sendMessageChan (messageChannel m)
        "The role provided is invalid. Please make sure you use the role's name!")
    guard doesRoleExist
    -- Role's fine!

    let (stationIDStr, channelIDStr) = (T.unpack stationID, T.unpack channelID)
    let (stationIDNum, channelIDNum) = (read stationIDStr, read channelIDStr)
    let Just roleID = roleIDM
    let assignFilePath = getAssignFile' stationIDStr

    -- The old emote role mapping, read from the CSV.
    roleEmoteMatrix <- liftIO $ readCSV assignFilePath
    -- The new emote role map! Cons the emoji and role at the front.
    let emojiRoleIDMap = (emoji, roleID) : map (read . T.unpack <$>) (twoDimMatrixToMap roleEmoteMatrix)

    -- Edit said message to the new one.
    assignStationT <- formatAssignStation prependT appendT emojiRoleIDMap
    _ <- restCall $ EditMessage (channelIDNum, stationIDNum) assignStationT Nothing

    -- Write the new mapping to the old CSV
    _ <- liftIO . writeCSV assignFilePath . mapToMatrix $ fmap (T.pack . show) <$> emojiRoleIDMap

    -- React!
    addReaction channelIDNum stationIDNum emoji

getAssignFile :: MessageId -> FilePath
getAssignFile = getAssignFile' . show

getAssignFile' :: String -> FilePath
getAssignFile' mid = mid <> ".selfAssign"

roleIdToRole :: RoleId -> [Role] -> T.Text
roleIdToRole rid roles = roleName . head $ filter (\r -> roleId r == rid) roles

formatAssignStation :: T.Text -> T.Text -> [(T.Text, RoleId)] -> DiscordHandler T.Text
formatAssignStation prependT appendT options = do
    Right roles <- restCall $ GetGuildRoles serverID
    let roleTextOptions = (\(emojiT, roleID) -> (emojiT, roleIdToRole roleID roles)) <$> options
    let optionsT = (\(emoji, roleName) ->
                        "`[`" <> emoji <> "`]` for " <> "`[`" <> roleName <> "`]`")
                    <$> roleTextOptions
    pure $ T.unlines [
            prependT,
            "",
            T.unlines optionsT,
            appendT
        ]

createAssignStation :: Message -> DiscordHandler ()
createAssignStation m = newModCommand m ("createSelfAssign" <> spaceRE                  -- command name
                                             <> quotedArgRE <> spaceRE                  -- prepended text
                                             <> quotedArgRE <> spaceRE                  -- appended text
                                             <> accoladedArgRE) $ \captures -> do       -- json
    -- Captures are [arg1, arg2, json]
    let [prependT, appendT, emojiRoleJson] = captures
    let emojiRoleMapM = decode . fromStrict $ encodeUtf8 emojiRoleJson :: Maybe (Map String String)
    case emojiRoleMapM of
        Nothing           -> do
            sendMessageChan (messageChannel m) $
                "Invalid JSON. " <> createAssignStationSyntax
        Just emojiRoleMap' -> do
            let emojiRoleMap = bimap T.pack T.pack <$> toList emojiRoleMap'
            doEmojisExist <- and <$> sequence ((\(emoji, _) -> isEmojiValid emoji serverID) <$> emojiRoleMap)
            unless doEmojisExist
                (sendMessageChan (messageChannel m)
                "One of the emojis provided is invalid. Perhaps you used one from another server?")
            guard doEmojisExist
            -- Emojis are fine!

            emojiRoleIDMMap <- sequence $ (\(emoji, roleFragment) ->
                                (emoji, ) <$> isRoleInGuild roleFragment serverID) <$> emojiRoleMap
            let doRolesExist = not $ any (\(_, roleIDM) -> isNothing roleIDM) emojiRoleIDMMap
            unless doRolesExist
                (sendMessageChan (messageChannel m)
                "One of the roles provided is invalid. Please make sure you use the roles' names!")
            guard doRolesExist
            -- Roles are fine!

            -- The map below is sanitised and perfectly safe to use.
            let emojiRoleIDMap = (\(emoji, Just roleID) -> (emoji, roleID)) <$> emojiRoleIDMMap
            handleRoleMapping prependT appendT m emojiRoleIDMap

handleRoleMapping :: T.Text -> T.Text -> Message -> [(T.Text, RoleId)] -> DiscordHandler ()
handleRoleMapping prependT appendT m emojiRoleIDMap = do
    -- Post the assignment station text.
    assignStationT <- formatAssignStation prependT appendT emojiRoleIDMap
    Right newMessage <- restCall $ CreateMessage (messageChannel m) assignStationT
    let assignStationID = messageId newMessage

    -- Hence, the map is fine. Write the mapping to the idAssign file :)
    let newFileName = getAssignFile assignStationID
    -- Write the mapping to the newly added CSV
    _ <- liftIO . writeCSV newFileName $ (\(key, value) -> [key, T.pack $ show value]) <$> emojiRoleIDMap

    -- Make Owen react to the self-assignment station.
    let emojiList = fst <$> emojiRoleIDMap
    forM_ emojiList (addReaction (messageChannel newMessage) assignStationID)

    -- Add the new assign file to the CSV
    _ <- liftIO $ addToCSV assignFilePath [[T.pack $ show assignStationID, T.pack newFileName]]
    pure ()

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
        pure (head line, (read . T.unpack . head . tail) line)

-- | Given a reaction @r@, `getRoleListIndex` parses the file "src/config/idAssign".
-- Note: this is a special file that represents a dictionary. Again, one mapping per line,
-- with the map indicated by a comma separating the message ID and the corresponding role
-- config file in "src/config". It returns a Maybe type representing the existence of such
-- a config file for the message that is attached to the given reaction.
getRoleListIndex :: ReactionInfo -> IO (Maybe T.Text)
getRoleListIndex r = do
    contents <- readCSV assignFilePath
    pure . lookup (reactionMessageId r) $ do
        line <- contents
        let pair = (head line, (head . tail) line)
        pure $ first (read . T.unpack) pair

-- | `getAssignMessageIds` returns a list of all message Snowflakes for all messages which
-- represent a self assignment station. All such messages are present in "src/config/idAssign",
-- which is also the only file name that mustn't be edited.
getAssignMessageIds :: IO [MessageId]
getAssignMessageIds = do
    lines <- readCSV assignFilePath
    pure $ map (read . takeWhile isDigit . T.unpack . head) lines