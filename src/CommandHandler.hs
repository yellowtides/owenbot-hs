{-# LANGUAGE OverloadedStrings #-}

module CommandHandler (handleCommand, isCommand) where

import qualified Discord.Requests as R
import Discord.Types
import Discord
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Text.Regex.TDFA ((=~))
import Data.Char (isAlpha, isSpace)
import Control.Exception (catch, IOException)
import Data.Maybe (fromJust)

import OwenRegex

-- map through all the regexes and see if any of them match
isCommand :: T.Text -> Bool
isCommand m = any (m =~) commandREs 

handleCommand :: Message -> DiscordHandler (Either RestCallErrorCode Message)
handleCommand m
    | content =~ thmRE        = sendImage channel ("Theorem " <> T.pack (getVal content)) ("./src/assets/theorems/" ++  (getVal content))
    | content =~ defRE        = sendImage channel ("Definition " <> T.pack (getVal content)) ("./src/assets/definitions/" ++  (getVal content))
    | content =~ lemmaRE      = sendImage channel ("Lemma " <> T.pack (getVal content)) ("./src/assets/lemmas/" ++  (getVal content))
    | content =~ textbookRE   = restCall(R.TriggerTypingIndicator channel) >> sendImage channel "Textbook.pdf" "./src/assets/textbook/nichol.pdf"
    | content =~ syllogismsRE = sendImage channel "Id-smash-aristotle.png" "./src/assets/cl/syllogisms.png"
    | content =~ booleanRE    = sendImage channel "literally-satan.png" "./src/assets/cl/Bool.png"
    | content =~ hoogleInfRE  = sendMessage channel "test"
    | content =~ helpRE       = sendMessage channel "test"
    where
        content = messageText m
        channel = messageChannel m
            
sendMessage :: ChannelId -> T.Text -> DiscordHandler (Either RestCallErrorCode Message)
sendMessage c xs = restCall (R.CreateMessage c xs)

sendImage :: ChannelId -> T.Text -> FilePath -> DiscordHandler (Either RestCallErrorCode Message)
sendImage c t f 
    | fileContent == Nothing = sendMessage c "iw cannow be foun uwu"
    | otherwise              = restCall (R.CreateMessageUploadFile c t . fromJust $ unsafeFileOpen f)
    where
        fileContent = unsafeFileOpen f

unsafeFileOpen :: FilePath -> Maybe B.ByteString
unsafeFileOpen = unsafePerformIO . safeRead

safeRead :: FilePath -> IO (Maybe B.ByteString)
safeRead path = catch (Just <$> B.readFile path) putNothing
            where
                putNothing :: IOException -> IO (Maybe B.ByteString) 
                putNothing = const $ pure Nothing

getVal :: T.Text -> String
getVal xs = (dropWhile (\x -> isAlpha x || isSpace x) . drop 1 $ T.unpack xs) ++ ".png"