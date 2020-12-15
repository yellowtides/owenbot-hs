module Helpme (sendHelpDM) where

import Discord.Types
import Discord
import qualified Data.Text.IO as TIO
import UnliftIO (liftIO)
import Data.Bifunctor (first)
import Data.Char (isAlpha, isSpace)

import Senders (sendMessageDM)

sendHelpDM :: User -> DiscordHandler (Either RestCallErrorCode Message)
sendHelpDM user = liftIO (TIO.readFile "./src/assets/help.txt") >>= sendMessageDM (userId user)