{-# LANGUAGE OverloadedStrings #-}

module Senders (sendMessageChan, sendMessageDM, sendFileChan) where

import qualified Discord.Requests as R
import Discord.Types
import Discord

import qualified Data.ByteString as B
import qualified Data.Text as T
import Control.Exception (catch, IOException)
import UnliftIO (liftIO)

sendMessageChan :: ChannelId -> T.Text -> DiscordHandler (Either RestCallErrorCode Message)
sendMessageChan c xs = restCall (R.CreateMessage c xs)

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
        Just fileContent -> restCall (R.CreateMessageUploadFile c t $ fileContent)

safeReadFile :: FilePath -> IO (Maybe B.ByteString)
safeReadFile path = catch (Just <$> B.readFile path) putNothing
            where
                putNothing :: IOException -> IO (Maybe B.ByteString)
                putNothing = const $ pure Nothing