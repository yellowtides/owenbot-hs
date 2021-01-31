{-# LANGUAGE OverloadedStrings #-}

module Utils (sendMessageChan, sendMessageChanEmbed, sendMessageDM, sendFileChan,
              pingAuthorOf, linkChannel, getMessageLink, isMod, isRole, (=~=),
               getTimestampFromMessage, openCSV, addToCSV, rmFuncText, captureCommandOutput, restart) where

import qualified Discord.Requests as R
import Discord.Types
import Discord
import Control.Monad (guard, unless, when)
import qualified Data.ByteString as B
import System.IO as Sys
import System.Process as Process
import qualified Data.Text as T
import Data.Function (on)
import Text.Regex.TDFA ((=~))
import Control.Exception (catch, IOException)
import UnliftIO (liftIO)
import Owoifier (owoify)
import qualified Data.Time.Format as TF
import Data.List.Split ( splitOn )
import Data.Char (isSpace, isAlpha)

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

sendMessageChan :: ChannelId -> T.Text -> DiscordHandler (Either RestCallErrorCode Message)
sendMessageChan c xs = restCall (R.CreateMessage c xs)

sendMessageChanEmbed :: ChannelId -> T.Text -> CreateEmbed -> DiscordHandler (Either RestCallErrorCode Message)
sendMessageChanEmbed c xs e = restCall (R.CreateMessageEmbed c xs e)

sendMessageDM :: UserId -> T.Text -> DiscordHandler (Either RestCallErrorCode Message)
sendMessageDM u t = do
    chanM <- restCall $ R.CreateDM u
    case chanM of
        Right chan -> sendMessageChan (channelId chan) t
        Left  err  -> pure $ Left err

sendFileChan :: ChannelId -> T.Text -> FilePath -> DiscordHandler (Either RestCallErrorCode Message)
sendFileChan c t f = do
    mFileContent <- liftIO $ safeReadFile f
    case mFileContent of
        Nothing          -> sendMessageChan c "iw cannow be foun uwu"
        Just fileContent -> restCall (R.CreateMessageUploadFile c t fileContent)

safeReadFile :: FilePath -> IO (Maybe B.ByteString)
safeReadFile path = catch (Just <$> B.readFile path) putNothing
            where
                putNothing :: IOException -> IO (Maybe B.ByteString)
                putNothing = const $ pure Nothing

isMod :: Message -> DiscordHandler Bool
isMod m = isRole m "Moderator"

isRole :: Message -> T.Text -> DiscordHandler Bool 
isRole m r = case (messageGuild m) of
               Nothing -> pure False
               Just g -> do 
                   let Just g = messageGuild m
                   Right userRole <- restCall $ R.GetGuildMember g (userId $ messageAuthor m)
                   filtered <- toRoles (userId $ messageAuthor m) g 
                   return $ r `elem` map roleName filtered

toRoles :: UserId -> GuildId -> DiscordHandler [Role]
toRoles i g = do
    Right allRole <- restCall $ R.GetGuildRoles g
    Right userG <- restCall $ R.GetGuildMember g i
    let filtered = filter (\x -> roleId x `elem` memberRoles userG) allRole
    return filtered

getTimestampFromMessage :: Message -> T.Text
getTimestampFromMessage m = T.pack $ TF.formatTime TF.defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" (messageTimestamp m)

openCSV :: FilePath -> IO [String]
openCSV f = do
    mFileContent <- safeCsvRead f
    case mFileContent of
        Nothing          -> writeFile f "" >> return []
        Just fileContent -> return $ parseCSV fileContent
        where
            parseCSV x  = splitOn ", " x

addToCSV :: FilePath -> String -> IO ()
addToCSV = appendFile

safeCsvRead :: FilePath -> IO (Maybe String)
safeCsvRead path = catch (Just <$> Sys.readFile path) putNothing
            where
                putNothing :: IOException -> IO (Maybe String)
                putNothing = const $ pure Nothing

rmFuncText :: T.Text -> T.Text
rmFuncText = T.dropWhile isAlpha . T.tail

captureCommandOutput :: String -> IO T.Text
captureCommandOutput command = do
    let (executable:args) = splitOn " " command
    output <- Process.readCreateProcess ((Process.proc executable args) {
        cwd = Just "."
    }) ""
    return $ T.pack output

restart :: IO()
restart = Process.callCommand "~/owenbot-hs/.restartWithin.hs"
