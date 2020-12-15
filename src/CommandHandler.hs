module CommandHandler (handleCommand, isCommand) where

import qualified Discord.Requests as R
import Discord.Types
import Discord

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Text.Regex.TDFA ((=~))
import Data.Char (isAlpha, isSpace)
import Control.Exception (catch, IOException)

import UnliftIO (liftIO)

import OwenRegex
import qualified ILA    (sendThmChan, sendDefChan, sendLemChan, sendTextbookChan) as ILA
import qualified Inf1A  (sendHDocChan, sendBoolChan, sendTextbookChan, sendSylChan) as I1A
import qualified Helpme (sendHelpDM)  as HLP

-- map through all the regexes and see if any of them match
isCommand :: T.Text -> Bool
isCommand m = any (m =~) commandREs

handleCommand :: Message -> DiscordHandler (Either RestCallErrorCode Message)
handleCommand m
    | content =~ thmRE        = ILA.sendThmChan channel cmdText
    | content =~ defRE        = ILA.sendDefChan channel cmdText
    | content =~ lemmaRE      = ILA.sendLemChan channel cmdText
    | content =~ textbookRE   = simTyping $ ILA.sendTextbookChan channel
    | content =~ syllogismsRE = I1A.sendSylChan channel
    | content =~ booleanRE    = I1A.sendBoolChan channel
    | content =~ hoogleInfRE  = I1A.sendHDocChan channel
    | content =~ helpRE       = (liftIO $ TIO.readFile "./src/assets/help.txt") >>= sendDM (userId user)
    where
        cmdText   = messageText m
        channel   = messageChannel m
        simTyping = (>>) $ restCall (R.TriggerTypingIndicator channel)
        user      = messageAuthor m

sendMessageChan :: ChannelId -> T.Text -> DiscordHandler (Either RestCallErrorCode Message)
sendMessageChan c xs = restCall (R.CreateMessage c xs)

sendMessageDM :: UserId -> T.Text -> DiscordHandler (Either RestCallErrorCode Message)
sendMessageDM u t = do
    let call = restCall $ R.CreateDM u
    case call of
        Right chan -> sendMessage (channelId chan) t
        Left  err  -> err

sendFileChan :: ChannelId -> T.Text -> FilePath -> DiscordHandler (Either RestCallErrorCode Message)
sendFileChan c t f = do
    mFileContent <- liftIO $ safeReadFile f
    case mFileContent of
        Nothing          -> sendMessage c "iw cannow be foun uwu"
        Just fileContent -> restCall (R.CreateMessageUploadFile c t $ fileContent)

safeReadFile :: FilePath -> IO (Maybe B.ByteString)
safeReadFile path = catch (Just <$> B.readFile path) putNothing
            where
                putNothing :: IOException -> IO (Maybe B.ByteString)
                putNothing = const $ pure Nothing

getVal :: T.Text -> String
getVal xs = (dropWhile (\x -> isAlpha x || isSpace x) . drop 1 $ T.unpack xs) ++ ".png"