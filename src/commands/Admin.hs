{-# LANGUAGE OverloadedStrings #-}

module Admin (receivers) where

import qualified Data.Text as T hiding (head, tail)
import Discord.Types (ChannelId, Message(messageChannel, messageAuthor), User(userId), Channel(channelId))
import Discord ( DiscordHandler, RestCallErrorCode )
import UnliftIO(liftIO)
import Data.Char (isSpace)
import Control.Monad (guard)
import Utils (wrapCommand, sendMessageChan, sendMessageDM, isRole, captureCommandOutput, restart)
import Status (updateStatus, editStatusFile)

receivers :: [Message -> DiscordHandler (Either RestCallErrorCode Message)]
receivers = [thatcherIsDead]

rstrip :: T.Text -> T.Text
rstrip = T.reverse . T.dropWhile isSpace . T.reverse

thatcherIsDead :: Message -> DiscordHandler (Either RestCallErrorCode Message)
thatcherIsDead m = wrapCommand m "thatcher is (dead)" $ \captures -> do 
  _ <- liftIO $ putStrLn "hewwo"
  sendMessageChan (messageChannel m) "owo"



-- randomOwoify :: Message -> DiscordHandler (Either RestCallErrorCode Message)
-- randomOwoify m = sendMessageChan (messageChannel m) (pingAuthorOf m <> ": " <> owoify (messageText m))