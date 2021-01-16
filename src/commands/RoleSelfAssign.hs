{-# LANGUAGE OverloadedStrings #-}
module RoleSelfAssign ( handleRoleRemove, handleRoleAssign ) where

import Data.List.Split (splitOn)
import Discord ( restCall, DiscordHandler, RestCallErrorCode )
import Discord.Types
    ( Emoji(emojiName),
      ReactionInfo(reactionEmoji, reactionUserId, reactionMessageId),
      MessageId,
      RoleId,
      Message )
import Discord.Requests
    ( GuildRequest(RemoveGuildMemberRole, AddGuildMemberRole) )

import Data.Bifunctor ( first )
import Data.Char ( isDigit, toUpper )

import UnliftIO ( liftIO )
import Control.Monad ( guard )
import Data.Maybe ( fromJust, fromMaybe, isJust )
import qualified Data.Text as T 
    ( toUpper, unpack, Text, splitOn, 
      breakOn, pack, tail, )
import qualified Data.Text.IO as TIO (readFile)

import Owoifier (owoify)

import Utils
    (sendMessageChan, isRole,  pingAuthorOf,
      linkChannel,
      getMessageLink,
      sendMessageChanEmbed,
      getTimestampFromMessage,
      openCSV,
      addToCSV,
      rmFuncText,
      sendMessageDM )

-- `handleRoleAssign` handles role assignments.
handleRoleAssign :: ReactionInfo -> DiscordHandler (Either RestCallErrorCode Message)
handleRoleAssign r = do
    validMessages <- liftIO getAssignMessageIds
    guard (reactionMessageId r `elem` validMessages)
    -- make sure the message being reacted is a role assignment message
    -- (prevents the config from being opened very often / every sent message)

    assFileName <- liftIO $ getRoleListIndex r
    roleMap <- liftIO $ getRoleMap (fromJust assFileName)
    let desiredRole = lookup (T.toUpper . emojiName $ reactionEmoji r) roleMap
    guard $ isJust desiredRole
    -- sanity check the desired role (can't be anything other than those which
    -- are specified in the config; for example, emotePronounMap.conf)
    -- NOTE: make sure the emoji names in the config are uppercase.

    let newRoleId = fromJust desiredRole
    restCall $ AddGuildMemberRole 768810076201811989 (reactionUserId r) newRoleId
    -- the number is the fixed Guild/Server ID. 
    -- TODO: put the number in a config file.

    sendMessageDM (reactionUserId r) 
        $ owoify "Added your desired role! Hurray!"
    
-- | TODO: remove the repetition in handleRoleAssign/handleRoleRemove by
-- modularizing better (i.e., the sanity check).
handleRoleRemove :: ReactionInfo -> DiscordHandler (Either RestCallErrorCode Message)
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
    -- are specified in the config; for example, emotePronounMap.conf)
    -- NOTE: make sure the emoji names in the config are uppercase.

    let oldRoleId = fromJust desiredRole
    restCall $ RemoveGuildMemberRole 768810076201811989 (reactionUserId r) oldRoleId
    -- the number is the fixed Guild/Server ID. 
    -- TODO: put the number in a config file.

    sendMessageDM (reactionUserId r)
        $ owoify "Really sorry you didn't like the role, I went ahead and removed it."

-- | Given a Text @dir@, `getRoleMap` parses the file in "src/config",
-- converting it into a dictionary. One mapping per line. The map is indicated
-- by a space seperating the emoji name (in UPPERCASE) and the corresponding role
-- ID. This has to be wrapped in IO.
getRoleMap :: T.Text -> IO [(T.Text, RoleId)]
getRoleMap dir = do
    file <- TIO.readFile . T.unpack $ "src/config/" <> dir
    let lines = T.splitOn "\n" file
    pure $ do
        line <- lines
        let pair = T.breakOn " " line
        pure $ fmap (read . T.unpack . T.tail) pair

-- | Given a reaction @r@, `getRoleListIndex` parses the file "src/config/idAssign".
-- Note: this is a special file that represents a dictionry. Again, one mapping per line,
-- with the map indicated by a space seperating the message ID and the corresponsning role
-- config file in "src/config". It returns a Maybe type representing the existence of such
-- a config file for the message that is attached to the given reaction.
getRoleListIndex :: ReactionInfo -> IO (Maybe T.Text)
getRoleListIndex r = do
    file <- TIO.readFile "src/config/idAssign.conf"
    let lines = T.splitOn "\n" file
    pure . lookup (reactionMessageId r) $ do
        line <- lines
        let pair = T.breakOn " " line
        pure . fmap T.tail $ first (read . T.unpack) pair

-- | `getAssignMessageIds` returns a list of all message Snowflakes for all messages wnich
-- represent a self assignment station. All such messages are present in "src/config/idAssign",
-- which is also the only file name that mustn't be edited.
getAssignMessageIds :: IO [MessageId]
getAssignMessageIds = do
    file <- TIO.readFile "src/config/idAssign.conf"
    let lines = T.splitOn "\n" file
    pure $ map (read . takeWhile isDigit . T.unpack) lines