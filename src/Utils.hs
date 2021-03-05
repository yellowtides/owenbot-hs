{-# LANGUAGE OverloadedStrings #-}

module Utils ( sendMessageChan
             , sendMessageChanEmbed
             , sendMessageDM
             , sendFileChan
             , pingAuthorOf
             , newCommand
             , linkChannel
             , getMessageLink
             , hasRoleByName
             , hasRoleByID
             , isMod
             , isSenderDeveloper
             , devIDs
             , (=~=)
             , getTimestampFromMessage
             , captureCommandOutput
             , strToSnowflake
             , restart
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
import           Text.Regex.TDFA        ( (=~) )
import           UnliftIO               ( liftIO
                                        , UnliftIO ( unliftIO )
                                        , stringException
                                        )

import           Owoifier               ( owoify )
import           TemplateRE             ( trailingWS )
import           CSV                    ( readSingleColCSV )

devIDs :: FilePath
devIDs = "devs.csv"

-- | (=~=) is owoify-less (case-less in terms of owoifying)
(=~=) :: T.Text -> T.Text -> Bool
(=~=) = (=~) `on` T.dropEnd 4 . owoify

pingAuthorOf :: Message -> T.Text
pingAuthorOf m = "<@" <> T.pack (show . userId $ messageAuthor m) <> ">"

linkChannel :: ChannelId  -> T.Text
linkChannel c = "<#" <> T.pack (show c) <> ">"

getMessageLink :: Message -> DiscordHandler (Either RestCallErrorCode T.Text)
getMessageLink m = do
    chanM <- restCall $ R.GetChannel (messageChannel m)
    case chanM of
        Right chan -> pure $ Right ("https://discord.com/channels/" <> T.pack (show $ channelGuild chan) <> "/" <> T.pack (show $ messageChannel m) <> "/" <> T.pack (show $ messageId m))
        Left err -> pure $ Left err

sendMessageChan :: ChannelId -> T.Text -> DiscordHandler ()
sendMessageChan c xs = restCall (R.CreateMessage c xs) >> pure ()

sendMessageChanEmbed :: ChannelId -> T.Text -> CreateEmbed -> DiscordHandler ()
sendMessageChanEmbed c xs e = restCall (R.CreateMessageEmbed c xs e) >> pure ()

sendMessageDM :: UserId -> T.Text -> DiscordHandler ()
sendMessageDM u t = do
    chanM <- restCall $ R.CreateDM u
    case chanM of
        Right chan -> sendMessageChan (channelId chan) t
        Left  err  -> pure () -- sorry, ignoring errors

sendFileChan :: ChannelId -> T.Text -> FilePath -> DiscordHandler ()
sendFileChan c t f = do
    mFileContent <- liftIO $ safeReadFile f
    case mFileContent of
        Nothing          -> sendMessageChan c "iw cannow be foun uwu"
        Just fileContent -> restCall (R.CreateMessageUploadFile c t fileContent) >> pure ()

safeReadFile :: FilePath -> IO (Maybe B.ByteString)
safeReadFile path = catch (Just <$> B.readFile path) putNothing
            where
                putNothing :: IOException -> IO (Maybe B.ByteString)
                putNothing = const $ pure Nothing

isMod :: Message -> DiscordHandler Bool
isMod m = hasRoleByName m "Moderator"

hasRoleByName :: Message -> T.Text -> DiscordHandler Bool
hasRoleByName m r = case messageGuild m of
    Nothing -> pure False
    Just g -> do
        filtered <- getUser'sRolesInGuild (userId $ messageAuthor m) g
        return $ r `elem` map roleName filtered

exists :: Eq a => [a] -> [a] -> Bool
exists x y = or $ (==) <$> x <*> y

hasRoleByID :: Message -> Snowflake -> DiscordHandler Bool
hasRoleByID m r = case messageGuild m of
    Nothing -> pure False
    Just g -> do
        filtered <- getUser'sRolesInGuild (userId $ messageAuthor m) g
        return $ r `elem` map roleId filtered

checkAllIDs :: Message -> IO [DiscordHandler Bool]
checkAllIDs m = do
    devFile <- readSingleColCSV devIDs
    let snow = (\x -> (read . T.unpack) x :: Snowflake) <$> devFile
    pure $ Prelude.map (hasRoleByID m) $ snow

isSenderDeveloper :: Message -> DiscordHandler Bool
isSenderDeveloper m = liftM or $ join $ liftIO $ sequence <$> checkAllIDs m

strToSnowflake :: String -> Snowflake
strToSnowflake = read

getUser'sRolesInGuild :: UserId -> GuildId -> DiscordHandler [Role]
getUser'sRolesInGuild i g = do
    Right allRole <- restCall $ R.GetGuildRoles g
    Right userG <- restCall $ R.GetGuildMember g i
    let filtered = filter (\x -> roleId x `elem` memberRoles userG) allRole
    return filtered

getTimestampFromMessage :: Message -> T.Text
getTimestampFromMessage m = T.pack $ TF.formatTime TF.defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" (messageTimestamp m)

captureCommandOutput :: String -> IO T.Text
captureCommandOutput command = do
    let (executable:args) = splitOn " " command
    output <- Process.readCreateProcess ((Process.proc executable args) {
        cwd = Just "."
    }) ""
    return $ T.pack output

restart :: IO ()
restart = Process.callCommand "~/owenbot-hs/.restartWithin.sh"

newCommand :: Message -> T.Text -> ([T.Text] -> DiscordHandler ()) -> DiscordHandler ()
newCommand msg cmd fun =
  let
    match :: (T.Text, T.Text, T.Text, [T.Text])
    match@(_, shouldNotBeEmpty, _, captures) = messageText msg =~ ("^:" <> cmd <> trailingWS)
  in
    do
        unless (shouldNotBeEmpty == "") $ fun captures