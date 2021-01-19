{-# LANGUAGE OverloadedStrings #-}

module MiscHandler (isOwoifiable, handleOwoify,
                    isNietzsche, handleNietzsche,
                    isDadJoke, handleDadJoke ) where

import qualified Discord.Requests as R
import Discord.Types
import Discord

import qualified Data.Maybe as M
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import UnliftIO (liftIO)
import Text.Regex.TDFA
import System.IO as S (readFile)

import Data.Char ( isAlpha )

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

isDadJoke :: T.Text -> Maybe T.Text
isDadJoke t = let match = t =~ ("[iI]'?[Mm] +[a-zA-Z]+[, ]*" :: T.Text) in
              case match of
                  "" -> Nothing
                  e  -> Just . T.takeWhile (\x -> x `elem` ("'*" :: [Char]) || isAlpha x) 
                             . T.dropWhile (== ' ') $ T.dropWhile (/= ' ') match

handleDadJoke :: Message -> T.Text -> DiscordHandler (Either RestCallErrorCode Message)
handleDadJoke m t = sendMessageChan (messageChannel m) $ owoify ("hello " <> t <> ", i'm owen")