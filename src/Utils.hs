{-# LANGUAGE OverloadedStrings #-}

module Utils (sendMessageChan, sendMessageDM, sendFileChan,
              pingAuthorOf, (=~=)) where

import qualified Discord.Requests as R
import Discord.Types
import Discord

import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Function (on)
import Text.Regex.TDFA ((=~))
import Control.Exception (catch, IOException)
import UnliftIO (liftIO)
import Owoifier (owoify)

-- | (=~=) is owoify-less (case-less in terms of owoifying)
(=~=) :: T.Text -> T.Text -> Bool
(=~=) = (=~) `on` T.dropEnd 4 . owoify

pingAuthorOf :: Message -> T.Text
pingAuthorOf m = "<@" <> T.pack (show . userId $ messageAuthor m) <> ">"

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