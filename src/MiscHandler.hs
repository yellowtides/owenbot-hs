{-# LANGUAGE OverloadedStrings #-}

module MiscHandler (isOwoifiable, handleOwoify,
                    isNietzsche, handleNietzsche) where

import qualified Discord.Requests as R
import Discord.Types
import Discord

import qualified Data.Maybe as M
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import UnliftIO (liftIO)
import Text.Regex.TDFA
import System.IO as S (readFile)

import Utils (sendMessageChan, sendMessageChanEmbed, sendFileChan, pingAuthorOf, linkChannel, getMessageLink, (=~=))
import Owoifier (owoify)

isOwoifiable :: T.Text -> Bool
isOwoifiable = (=~= ("[lLrR]|[nNmM][oO]" :: T.Text))

handleOwoify :: Message -> DiscordHandler (Either RestCallErrorCode Message)
handleOwoify m = sendMessageChan (messageChannel m) (pingAuthorOf m <> ": " <> owoify (messageText m))

isNietzsche :: T.Text -> Bool
isNietzsche = (=~= ("[gG]od *[iI]s *[dD]ead" :: T.Text))

handleNietzsche :: Message -> DiscordHandler (Either RestCallErrorCode Message)
handleNietzsche m = liftIO (TIO.readFile "./src/assets/nietzsche.txt") >>= sendMessageChan (messageChannel m) . owoify
