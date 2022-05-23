{-# LANGUAGE OverloadedStrings #-}
module TTS (commands) where

import Control.Exception (handle)
import Control.Monad (void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)
import Network.HTTP.Simple
    ( HttpException
    , getResponseBody
    , httpBS
    , parseRequest
    , setRequestBodyJSON
    , setRequestMethod
    , setRequestQueryString
    )

import Command
import Discord
import Discord.Types
import Owoifier

commands :: [Command DiscordHandler]
commands = [tts]

tts :: Command DiscordHandler
tts =
    help "Speak stuff outwoud. Enclose with / to use IPA.\nUsage: `:tts <stuff>`"
        . command "tts"
        $ \m (Remaining input) -> do
        -- we are guaranteed input is non-empty by the parser + head/last are O(1)
            let isIPA = T.head input == '/' && T.last input == '/'
            audioData <- case isIPA of
                False -> liftIO $ downloadTTS input
                True  -> liftIO $ downloadIPA input
            case audioData of
                Nothing    -> respond m "Failed to download TTS audio :("
                Just audio -> do
                    let filename =
                            owoify $ "Amazing sounds by " <> userName (messageAuthor m)
                    let extension = if isIPA then ".mp3" else ".wav"
                    void $ createMessageUploadFile
                        (messageChannelId m)
                        (filename <> extension)
                        audio

-- | Download the TTS audio.
downloadTTS :: T.Text -> IO (Maybe B.ByteString)
downloadTTS ttsText =
    handle (\x -> (putStrLn $ show (x :: HttpException)) >> pure Nothing) $ do
        initReq <- parseRequest "https://tts.cyzon.us/tts"
        let req = setRequestQueryString [("text", Just $ encodeUtf8 ttsText)] initReq
        Just . getResponseBody <$> httpBS req

-- | Download the pronunciation audio for an IPA.
downloadIPA :: T.Text -> IO (Maybe B.ByteString)
downloadIPA ipaText =
    handle (\x -> (putStrLn $ show (x :: HttpException)) >> pure Nothing) $ do
        initReq <- parseRequest
            "https://iawll6of90.execute-api.us-east-1.amazonaws.com/production"
        let body = HM.fromList [("text" :: String, ipaText), ("voice", "Emma")]
        let req  = setRequestMethod "POST" $ setRequestBodyJSON body $ initReq
        -- Response contains the base64 encoded audio data, wrapped in
        -- quotes. So we take strip those off using init and tail, which each
        -- take O(1) with ByteStrings.
        result <- getResponseBody <$> httpBS req
        -- Response also contains some escaped newlines, like \\n. So we convert
        -- the ByteString to a String, read it as a String, then convert it back
        -- and then filter out the newline characters.
        let result' = B.filter (not . isNewLine) . BC.pack . read . BC.unpack $ result
        case B64.decodeBase64 result' of
            Left  e     -> print result >> print e >> pure Nothing
            Right audio -> pure $ Just audio

isNewLine :: Word8 -> Bool
isNewLine = (== 0x0a)
