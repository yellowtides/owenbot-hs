{-# LANGUAGE OverloadedStrings #-}
module RoleSelfAssign ( reactionAddReceivers, reactionRemReceivers ) where

import           Discord                ( restCall
                                        , DiscordHandler
                                        , RestCallErrorCode
                                        )
import           Discord.Types          ( Emoji ( emojiName )
                                        , ReactionInfo ( reactionEmoji
                                                       , reactionUserId
                                                       , reactionMessageId
                                                       )
                                        , MessageId
                                        , RoleId
                                        , Message
                                        )
import           Discord.Requests       ( GuildRequest ( RemoveGuildMemberRole
                                                       , AddGuildMemberRole
                                                       ) )
import           Control.Monad          ( guard )
import           UnliftIO               ( liftIO )
import           Data.Bifunctor         ( first )
import           Data.Char              ( isDigit
                                        , toUpper
                                        )
import           Data.Maybe             ( fromJust
                                        , fromMaybe
                                        , isJust
                                        )
import qualified Data.Text as T         ( toUpper
                                        , unpack
                                        , Text
                                        , splitOn
                                        , breakOn
                                        , pack
                                        , tail
                                        )

import           Owoifier               ( owoify )
import           Utils                  ( sendMessageChan
                                        , pingAuthorOf
                                        , sendMessageChanEmbed
                                        , sendMessageDM
                                        )
import           CSV                    ( readCSV
                                        , readSingleColCSV
                                        )

reactionAddReceivers :: [ReactionInfo -> DiscordHandler ()]
reactionAddReceivers = [ attemptRoleAssign ]

reactionRemReceivers :: [ReactionInfo -> DiscordHandler ()]
reactionRemReceivers = [ handleRoleRemove ]

isOnAssignMessage :: ReactionInfo -> DiscordHandler Bool
isOnAssignMessage r = do
    validMessages <- liftIO getAssignMessageIds
    pure $ reactionMessageId r `elem` validMessages
    -- make sure the message being reacted is a role assignment message
    -- (prevents the config from being opened very often / every sent message)

-- `attemptRoleAssign` handles role assignments.
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
    restCall $ AddGuildMemberRole 755798054455738489 (reactionUserId r) newRoleId
    -- the number is the fixed Guild/Server ID. 
    -- TODO: put the number in a config file.

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
    restCall $ RemoveGuildMemberRole 755798054455738489 (reactionUserId r) oldRoleId
    -- the number is the fixed Guild/Server ID. 
    -- TODO: put the number in a config file.

    sendMessageDM (reactionUserId r)
        $ owoify "Really sorry you didn't like the role, I went ahead and removed it."

-- | Given a Text @dir@, `getRoleMap` parses the file in "src/config",
-- converting it into a dictionary. One mapping per line. The map is indicated
-- by a comma seperating the emoji name (in UPPERCASE) and the corresponding role
-- ID. This has to be wrapped in IO.
getRoleMap :: T.Text -> IO [(T.Text, RoleId)]
getRoleMap dir = do
    contents <- readCSV $ T.unpack dir
    pure $ do
        line <- contents
        let pair = (head line, (head . tail) line)
        pure $ fmap (read . T.unpack . T.tail) pair

-- | Given a reaction @r@, `getRoleListIndex` parses the file "src/config/idAssign".
-- Note: this is a special file that represents a dictionry. Again, one mapping per line,
-- with the map indicated by a comma seperating the message ID and the corresponsning role
-- config file in "src/config". It returns a Maybe type representing the existence of such
-- a config file for the message that is attached to the given reaction.
getRoleListIndex :: ReactionInfo -> IO (Maybe T.Text)
getRoleListIndex r = do
    contents <- readCSV "idAssign.csv"
    pure . lookup (reactionMessageId r) $ do
        line <- contents
        let pair = (head line, (head . tail) line)
        pure . fmap T.tail $ first (read . T.unpack) pair

-- | `getAssignMessageIds` returns a list of all message Snowflakes for all messages wnich
-- represent a self assignment station. All such messages are present in "src/config/idAssign",
-- which is also the only file name that mustn't be edited.
getAssignMessageIds :: IO [MessageId]
getAssignMessageIds = do
    lines <- readSingleColCSV "idAssign.csv"
    pure $ map (read . takeWhile isDigit . T.unpack) lines