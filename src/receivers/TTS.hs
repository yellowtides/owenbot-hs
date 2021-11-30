{-# LANGUAGE OverloadedStrings #-}
module TTS (commands) where

import Control.Exception ( handle )
import Control.Monad ( void )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding ( encodeUtf8 )
import qualified Data.Text as T
import Network.HTTP.Simple
    ( httpBS
    , getResponseBody
    , parseRequest
    , HttpException
    , setRequestQueryString
    )

import Discord.Types
import Discord
import Command
import Owoifier

commands :: [Command DiscordHandler]
commands =
    [ tts
    ]

tts :: (MonadDiscord m, MonadIO m) => Command m
tts = command "tts" $ \m (Remaining input) -> do
    audioData <- liftIO $ downloadTTS input
    case audioData of
        Nothing -> respond m "Failed to download TTS audio :("
        Just audio -> do
            let filename = (<> ".wav") . owoify $ "Incredible audio from " <> userName (messageAuthor m)
            void $ createMessageUploadFile (messageChannel m) filename audio


-- | Download the TTS audio.
downloadTTS :: T.Text -> IO (Maybe B.ByteString)
downloadTTS ttsText   =
    handle (\x -> (putStrLn $ show (x :: HttpException)) >> pure Nothing) $ do
        initReq <- parseRequest "https://tts.cyzon.us/tts"
        let req = setRequestQueryString [("text", Just $ encodeUtf8 ttsText)] initReq
        Just . getResponseBody <$> httpBS req
