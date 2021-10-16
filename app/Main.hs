{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Discord
    ( DiscordHandler
    , RunDiscordOpts(discordOnEvent, discordOnLog, discordOnStart, discordToken)
    , def
    , restCall
    , runDiscord
    )
import Discord.Types
import System.Directory (createDirectoryIfMissing)

import Admin (sendGitInfoChan, sendInstanceInfoChan)
import Command
import Config
import DB (initGlobalDatabase)
import EventHandler (handleEvent)
import Misc (changePronouns)
import Status (setStatusFromFile)
import UnliftIO
import Utils (sendMessageChan)

-- | UWU
owen :: OwenConfig -> IO ()
owen cfg = do
    userFacingError <- runDiscord $ def
        { discordToken   = owenConfigToken cfg
        , discordOnStart = startHandler cfg
        , discordOnEvent = handleEvent
        , discordOnLog   = \s -> putStrLn ("[Info] " ++ T.unpack s)
        }
    TIO.putStrLn userFacingError

startHandler :: OwenConfig -> DiscordHandler ()
startHandler cfg = do
    let startupChan = owenConfigStartupChan cfg
    owenId <- getCurrentUser
    createMessage startupChan $ T.pack "Hewwo, I am bawck! UwU"
    sendGitInfoChan startupChan
    sendInstanceInfoChan startupChan
    changePronouns
    liftIO $ putStrLn $ "[Info] UserName: " <> T.unpack (userName owenId)
    void setStatusFromFile

main :: IO ()
main = do
    putStrLn "starting Owen"
    cfg <- readConfig
    initGlobalDatabase
    TIO.putStrLn $ "[Info] Token: " <> owenConfigToken cfg
    owen cfg
