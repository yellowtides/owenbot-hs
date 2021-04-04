module Main where

import qualified Data.Text as T
import           Control.Monad
import           Discord.Requests as R
import           Discord                ( runDiscord
                                        , def
                                        , DiscordHandler
                                        , RunDiscordOpts ( discordToken
                                                         , discordOnStart
                                                         , discordOnEvent
                                                         , discordOnLog
                                                         )
                                        )
import           Discord.Types          ( ChannelId )
import           System.Directory       ( createDirectoryIfMissing )

import           CSV                    ( configDir )
import           EventHandler           ( handleEvent )
import           Admin                  ( sendGitInfoChan )
import           Status                 ( setStatusFromFile )
import           Utils                  ( sendMessageChan )

-- | Channel to post startup message into
startupChan :: ChannelId
startupChan = 801763198792368129

-- | UWU
owen :: String -> IO ()
owen t = do
    userFacingError <- runDiscord $ def { discordToken   = T.pack t
                                        , discordOnStart = startHandler
                                        , discordOnEvent = handleEvent
                                        , discordOnLog = \s ->
                                            putStrLn ("[Info] " ++ T.unpack s)}
    putStrLn (T.unpack userFacingError)

startHandler :: DiscordHandler ()
startHandler = do
    sendMessageChan startupChan (T.pack "Hewwo, I am bawck! UwU")
    _ <- sendGitInfoChan startupChan
    void setStatusFromFile

main :: IO ()
main = do
    putStrLn "starting Owen"
    base <- configDir
    createDirectoryIfMissing True base
    tok <- readFile (base <> "token.txt")
    putStrLn ("[Info] Token: " ++ tok)
    owen tok
