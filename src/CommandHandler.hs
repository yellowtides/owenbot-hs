{-# LANGUAGE OverloadedStrings #-}

module CommandHandler (handleCommand, isCommand) where

import qualified Discord.Requests as R
import Discord.Types
import Discord
import qualified Data.ByteString as B
import qualified Data.Text as T
import Text.Regex.TDFA ((=~))
import Data.Char (isAlpha, isSpace)
import Control.Exception (catch, IOException)
import Control.Monad (when)
import Data.Maybe (fromJust)
import UnliftIO (liftIO)

import OwenRegex

-- map through all the regexes and see if any of them match
isCommand :: T.Text -> Bool
isCommand m = any (m =~) commandREs 

handleCommand :: Message -> DiscordHandler (Either RestCallErrorCode Message)
handleCommand m
    | content =~ thmRE        = sendFile channel ("Theorem "                  <> T.pack (getVal content)) 
                                                 ("./src/assets/theorems/"    ++ getVal content)
    | content =~ defRE        = sendFile channel ("Definition "               <> T.pack (getVal content)) 
                                                 ("./src/assets/definitions/" ++ getVal content)
    | content =~ lemmaRE      = sendFile channel ("Lemma "                    <> T.pack (getVal content)) 
                                                 ("./src/assets/lemmas/"      ++ getVal content)
    | content =~ textbookRE   = simTyping $ 
                                sendFile channel "Textbook.pdf"               "./src/assets/textbook/nichol.pdf"
    | content =~ syllogismsRE = sendFile channel "Id-smash-aristotle.png"     "./src/assets/cl/syllogisms.png"
    | content =~ booleanRE    = sendFile channel "literally-satan.png"        "./src/assets/cl/Bool.png"
    | content =~ hoogleInfRE  = sendMessage channel "test"
    | content =~ helpRE       = sendMessage channel "test"
    where
        content   = messageText m
        channel   = messageChannel m
        simTyping = (>>) $ restCall (R.TriggerTypingIndicator channel)

sendMessage :: ChannelId -> T.Text -> DiscordHandler (Either RestCallErrorCode Message)
sendMessage c xs = restCall (R.CreateMessage c xs)

sendFile :: ChannelId -> T.Text -> FilePath -> DiscordHandler (Either RestCallErrorCode Message)
sendFile c t f = do
    mFileContent <- liftIO $ safeRead f
    case mFileContent of 
        Nothing          -> sendMessage c "iw cannow be foun uwu"
        Just fileContent -> restCall (R.CreateMessageUploadFile c t $ fileContent)

safeRead :: FilePath -> IO (Maybe B.ByteString)
safeRead path = catch (Just <$> B.readFile path) putNothing
            where
                putNothing :: IOException -> IO (Maybe B.ByteString) 
                putNothing = const $ pure Nothing

getVal :: T.Text -> String
getVal xs = (dropWhile (\x -> isAlpha x || isSpace x) . drop 1 $ T.unpack xs) ++ ".png"