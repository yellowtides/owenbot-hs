{-# LANGUAGE OverloadedStrings #-}

{-|
    Module:     : Utils
    Description : A module containing all sorts of useful macros and functions. The Appendix of owenbot.
-}
module Utils ( sendMessageChan
             , sendReply
             , sendMessageChanEmbed
             , sendMessageChanPingsDisabled
             , sendMessageDM
             , sendFileChan
             , addReaction
             , messageFromReaction
             , pingUser
             , pingRole
             , pingAuthorOf
             , pingWithUsername
             , stripAllPings
             , newCommand
             , newDevCommand
             , linkChannel
             , getMessageLink
             , hasRoleByName
             , hasRoleByID
             , isMod
             , devIDs
             , (=~=)
             , getTimestampFromMessage
             , captureCommandOutput
             , restart
             , update
             ) where

import qualified Discord.Requests as R
import           Discord.Types
import           Discord
import           Control.Exception      ( catch
                                        , IOException
                                        )
import           Control.Monad          ( guard
                                        , unless
                                        , when
                                        , join
                                        , liftM
                                        , void
                                        )
import qualified Data.ByteString as B
import           Data.Char              ( isSpace
                                        , isAlpha
                                        )
import           Data.Function          ( on )
import           Data.List.Split        ( splitOn )
import qualified Data.Text as T
import qualified Data.Time.Format as TF

import           System.IO as Sys
import           System.Process as Process
import           System.Exit            ( ExitCode ( ExitSuccess, ExitFailure ) )
import           System.Posix.Process   ( getProcessID )

import           Text.Regex.TDFA        ( (=~) )
import           UnliftIO               ( liftIO
                                        , UnliftIO ( unliftIO )
                                        , stringException
                                        )

import           Owoifier               ( owoify
                                        , weakOwoify
                                        )
import           TemplateRE             ( trailingWS )
import           CSV                    ( readSingleColCSV )

import           Data.Either            ( isRight
                                        , fromRight )
import           Data.Maybe             ( fromMaybe )

-- | The `FilePath` to the configuration file listing OwenDev role IDs.
devIDs :: FilePath
devIDs = "devs.csv"

-- | The `FilePath` representing the repo for the bot (TODO: chuck in a config file)
repoDir :: FilePath
repoDir = "$HOME/owenbot-hs/"

-- | The `(=~=)` function matches a given `Text` again a regex. Case-less in terms of owoifying.
(=~=) :: T.Text -> T.Text -> Bool
(=~=) = (=~) `on` weakOwoify

-- | `pingUser` constructs a minimal `Text` pinging the given user.
pingUser :: User -> T.Text
pingUser u =  "<@" <> T.pack (show $ userId u) <> ">"

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
    let timing = R.GuildMembersTiming (Just 1000) Nothing
    -- number of users to fetch, maximum is 1000
    -- "Nothing" seems to default to (Just 1)
    membersM <- restCall $ R.ListGuildMembers gid timing 
    let members = case membersM of
            Right success -> success
            Left  _       -> []
    let users = memberUser <$> members
    _ <- liftIO $ print (userName <$> users)
    let usersWithUsername = filter (\u -> userName u == uname) users
    _ <- liftIO $ print usersWithUsername
    pure $ case usersWithUsername of
        []  -> ""
        [u] -> pingUser u
        _   -> ""

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
        stripOnePing []           = []
        stripOnePing [ch]         = [ch]
        stripOnePing ('<':xs) = if xs =~ pingRE
                                    then drop 1 $ dropWhile (/= '>') xs
                                    else '<':xs
        stripOnePing (x:xs)       = x : stripOnePing xs

-- | `linkChannel` constructs a minimal `Text` linking the channel with the provided ID.
linkChannel :: ChannelId  -> T.Text
linkChannel c = "<#" <> T.pack (show c) <> ">"

-- | `getMessageLink` attempts to construct the Discord URL of the given message, as a `Text`.
getMessageLink :: Message -> DiscordHandler (Either RestCallErrorCode T.Text)
getMessageLink m = do
    chanM <- restCall $ R.GetChannel (messageChannel m)
    case chanM of
        Right chan -> do
            -- the ID of the server containing the channel, as a `Text`
            let serverIDT  = T.pack . show $ channelGuild chan
            -- the ID of the channel the message was sent in, as a `Text`
            let channelIDT = T.pack . show $ messageChannel m
            -- the messageID, as a `Text`
            let messageIDT = T.pack . show $ messageId m
            pure . Right $ T.concat [ "https://discord.com/channels/",
                                      serverIDT, "/",
                                      channelIDT, "/",
                                      messageIDT ]
        Left err -> pure $ Left err

-- | `sendMessageChan` attempts to send the given `Text` in the channel with the given
-- `channelID`. Surpesses any error message(s), returning `()`.
sendMessageChan :: ChannelId -> T.Text -> DiscordHandler ()
sendMessageChan c xs = do
    void $ restCall $ R.CreateMessage c xs

sendMessageChanPingsDisabled :: ChannelId -> T.Text -> DiscordHandler () 
sendMessageChanPingsDisabled cid t = do
    let opts = def { R.messageDetailedContent = t
                    , R.messageDetailedAllowedMentions = Just
                        $ def { R.mentionEveryone = False
                              , R.mentionUsers    = False
                              , R.mentionRoles    = False
                              }
                   }
    void $ restCall (R.CreateMessageDetailed cid opts)

-- | `sendReply` attempts to send a reply to the given `Message`. Suppresses any error
-- message(s), returning `()`.
sendReply :: Message -> Bool -> T.Text -> DiscordHandler ()
sendReply m mention xs = do
    void $ restCall $ R.CreateMessageDetailed (messageChannel m)
        $ def { R.messageDetailedContent = xs
              , R.messageDetailedReference = Just
                $ def { referenceMessageId = Just $ messageId m }
              , R.messageDetailedAllowedMentions = Just
                $ def { R.mentionRepliedUser = mention }
              }

-- | `sendMessageChanEmbed` attempts to send the given embed with the given `Text` in the
-- channel with the given `channelID`. Surpesses any error message(s), returning `()`.
sendMessageChanEmbed :: ChannelId -> T.Text -> CreateEmbed -> DiscordHandler ()
sendMessageChanEmbed c xs e = do
    void $ restCall $ R.CreateMessageEmbed c xs e

-- | `sendMessageDM` attempts to send the given `Text` as a direct message to the user with the
-- given `UserId`. Surpresses any error message(s), returning `()`.
sendMessageDM :: UserId -> T.Text -> DiscordHandler ()
sendMessageDM u t = do
    chanM <- restCall $ R.CreateDM u
    case chanM of
        Right chan -> sendMessageChan (channelId chan) t
        Left  err  -> pure ()

-- | `sendFileChan` attempts to send the file at the provided `FilePath` in the channel with the
-- provided `ChannelId`. The file attachment is annotated by the given `Text`. Surpresses any error
-- message(s), returning `()`.
sendFileChan :: ChannelId -> T.Text -> FilePath -> DiscordHandler ()
sendFileChan c name fp = do
    mFileContent <- liftIO $ safeReadFile fp
    case mFileContent of
        Nothing          -> sendMessageChan c $ owoify "The file cannot be found!"
        Just fileContent -> do
            void $ restCall $ R.CreateMessageUploadFile c name fileContent

-- | `safeReadFile` attempts to convert the file at the provided `FilePath` into a `ByteString`,
-- wrapped in a `Maybe` monad. On reading failure, this function returns `Nothing`.
safeReadFile :: FilePath -> IO (Maybe B.ByteString)
safeReadFile path = catch (Just <$> B.readFile path) putNothing
            where
                putNothing :: IOException -> IO (Maybe B.ByteString)
                putNothing = const $ pure Nothing

-- | `messageFromReaction` attempts to get the Message instance from a reaction.
messageFromReaction :: ReactionInfo -> DiscordHandler (Either RestCallErrorCode Message)
messageFromReaction r = restCall
    $ R.GetChannelMessage (reactionChannelId r, reactionMessageId r)

-- | `addReaction` attempts to add a reaction to the given message ID. Supresses any
-- error message(s), returning `()`.
addReaction :: ChannelId -> MessageId -> T.Text -> DiscordHandler ()
addReaction c m t = restCall (R.CreateReaction (c, m) t) >> pure ()

-- | `isMod` checks whether the provided message was sent by a user with the `Moderator` role.
isMod :: Message -> DiscordHandler Bool
isMod m = hasRoleByName m "Moderator"

-- | `hasRoleByName` checks whether the provided message was sent by a user that has a role matching
-- the provided `Text` exactly.
hasRoleByName :: Message -> T.Text -> DiscordHandler Bool
hasRoleByName m r = case messageGuild m of
    Nothing -> pure False
    Just g -> do
        filtered <- getRolesOfUserInGuild (userId $ messageAuthor m) g
        return $ r `elem` map roleName filtered

-- | `isNotMutExWith` checks whether the two given lists are not mutually exclusive. That is, if the
-- two given lists contain at least one common element (with equality being determined by their `Eq`
-- class instantiation).
isNotMutExWith :: Eq a => [a] -> [a] -> Bool
isNotMutExWith x y = or $ (==) <$> x <*> y
-- the cartesian product of two lists, but constructed with pairwise `(==)` instead of `(,)`.

-- | `hasRoleByID` checks whether the provided message was sent by a user that has a role matching
-- the provided `RoleId`.
hasRoleByID :: Message -> RoleId -> DiscordHandler Bool
hasRoleByID m r = case messageGuild m of
    Nothing -> pure False
    Just g -> do
        filtered <- getRolesOfUserInGuild (userId $ messageAuthor m) g
        return $ r `elem` map roleId filtered

-- | `checkAllIDs` checks every role of the provided message's author against every role in the
-- global `devIDs` file, returning an exhaustive list of booleans as a result.
checkAllIDs :: Message -> IO [DiscordHandler Bool]
checkAllIDs m = do
    devFile <- readSingleColCSV devIDs
    let devRoleIDs = ((read . T.unpack) :: (T.Text -> RoleId)) <$> devFile
    pure $ map (hasRoleByID m) devRoleIDs

-- | `isSenderDeveloper` checks whether the provided message's author is a developer.
isSenderDeveloper :: Message -> DiscordHandler Bool
isSenderDeveloper m = fmap or . join . liftIO $ sequence <$> checkAllIDs m

-- | `getRolesOfUserInGuild` fetches a list of roles partaining to the user with the given `UserId`
-- within the guild with the given `GuildId`.
getRolesOfUserInGuild :: UserId -> GuildId -> DiscordHandler [Role]
getRolesOfUserInGuild uid g = do
    Right allGuildRoles <- restCall $ R.GetGuildRoles g
    Right user <- restCall $ R.GetGuildMember g uid
    let userRolesInGuild = filter (\x -> roleId x `elem` memberRoles user) allGuildRoles
    pure userRolesInGuild

-- | `getTimestampFromMessages` returns the given message's timestamp as `Text`, in the format
-- `yyyy-mm-dd | hh:mm:ss`.
getTimestampFromMessage :: Message -> T.Text
getTimestampFromMessage m = T.pack $ TF.formatTime TF.defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" (messageTimestamp m)

-- | `captureCommandOutput` creates a new process from the desired command provided as a `String`.
-- Then, it waits for the command to finish executing, returning its output as a `Text`.
captureCommandOutput :: String -> IO T.Text
captureCommandOutput command = do
    let (executable:args) = splitOn " " command
    output <- Process.readCreateProcess ((Process.proc executable args) {
        cwd = Just "."
    }) ""
    return $ T.pack output

-- | `restart` calls a shell script to restart the bot.
restart :: IO ()
restart = do
    Process.spawnCommand "owenbot-exe"
    pid <- getProcessID
    Process.callCommand $ "kill " <> show pid

-- | `update` calls a shell script that updates the bot's repo
update :: IO Bool
update = do
    process <- Process.spawnCommand ("cd " <> repoDir <> " && git reset --hard @{u} && git pull && stack install")
    exitcode <- Process.waitForProcess process
    pure $ case exitcode of
         ExitSuccess   -> True
         ExitFailure _ -> False

-- | `newCommand` should be used in the creation of a new Owen command. Given a `T.Text` command regex
-- (lacking the `:` prefix and the trailing whitespace), along with a function that can handle the
-- regex captures, the command can be used to create `Message -> DiscordHandler ()` message receivers.
newCommand :: Message                               -- ^ a message that needs to be handled
              -> T.Text                             -- ^ the new command regex
              -> ([T.Text] -> DiscordHandler ())    -- ^ a function used to handle each message portion
                                                    -- captured by the command regex
              -> DiscordHandler ()                  -- ^ the over-all result of handling the message
newCommand msg cmd funct = unless (shouldNotBeEmpty == "") $ funct captures
  where
    match :: ( T.Text
             , T.Text   -- the first match of the regex against the message
             , T.Text
             , [T.Text] -- every message portion identified by the regex capture groups
             )
    match@(_, shouldNotBeEmpty, _, captures) = messageText msg =~ ("^:" <> cmd <> trailingWS)

-- | `newDevCommand` should be used in the creation of a new Owen dev command. Acts in the same way as `newCommand`,
-- with the distinction that it constructs handlers that require the message author to be a developer. If they
-- are not, the message author is messaged directly and reprimanded so harshly that they will never attempt to use a
-- dev command ever again.
newDevCommand :: Message
                -> T.Text
                -> ([T.Text] -> DiscordHandler ())
                -> DiscordHandler ()
newDevCommand msg cmd fun = newCommand msg cmd $ \captures -> do
    isDev <- isSenderDeveloper msg
    if isDev
        then fun captures
        else sendMessageDM (userId $ messageAuthor msg) $ owoify "Insufficient privileges!"
