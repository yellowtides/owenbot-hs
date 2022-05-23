{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (SomeException)
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)

import Discord
import Discord.Requests
import Discord.Types

import Admin (sendGitInfoChan, sendInstanceInfoChan)
import Command
import Config
import DB (initGlobalDatabase)
import EventHandler (handleEvent)
import Misc (changePronouns)
import Status (setStatusFromFile)
import UnliftIO
import Utils (sendMessageChan)
import Quiz (randomQuizScheduler)

-- | UWU
owen :: OwenConfig -> IO ()
owen cfg = do
    userFacingError <- runDiscord $ def
        { discordToken   = owenConfigToken cfg
        , discordOnStart = handle handleStartErrors (startHandler cfg)
        , discordOnEvent = handleEvent
        , discordOnLog   = \s -> putStrLn ("[Info] " ++ T.unpack s)
        }
    TIO.putStrLn userFacingError

handleStartErrors :: SomeException -> DiscordHandler ()
handleStartErrors e = do
    liftIO
        $  putStrLn
        $  "[Info] Encountered error during startup, ignoring: "
        <> show e

startHandler :: OwenConfig -> DiscordHandler ()
startHandler cfg = do
    let startupChan = owenConfigStartupChan cfg
    owenId <- call $ GetCurrentUser
    createMessage startupChan $ T.pack "Hewwo, I am bawck! UwU"
    sendGitInfoChan startupChan
    sendInstanceInfoChan startupChan
    changePronouns
    liftIO $ putStrLn $ "[Info] Username: " <> T.unpack (userName owenId)
    void setStatusFromFile

main :: IO ()
main = do
    putStrLn "starting Owen"
    cfg <- readConfig
    initGlobalDatabase
    TIO.putStrLn $ "[Info] Token: " <> owenConfigToken cfg
    -- launch a thread to schedule and send quizzes
    id <- liftIO . forkIO $ randomQuizScheduler cfg
    owen cfg
    -- make sure we kill the random quiz scheduler, otherwise it'll keep running after spawning more owens
    killThread id
