module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Control.Monad
import Discord.Requests as R ( UserRequest(GetCurrentUser) )
import           Discord                ( runDiscord
                                        , def
                                        , DiscordHandler
                                        , RunDiscordOpts ( discordToken
                                                         , discordOnStart
                                                         , discordOnEvent
                                                         , discordOnLog
                                                         ), restCall
                                        )
import           Discord.Types          ( ChannelId, User (userName) )
import           System.Directory       ( createDirectoryIfMissing )

import           CSV                    ( configDir )
import           DB                     ( dbDir )
import           EventHandler           ( handleEvent )
import           Admin                  ( sendGitInfoChan )
import           Status                 ( setStatusFromFile )
import           Utils                  ( sendMessageChan )
import UnliftIO

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
    Right owenId <- restCall GetCurrentUser
    _ <- liftIO $ putStrLn $ "UserName: " <> T.unpack (userName owenId)
    _ <- sendGitInfoChan startupChan
    void setStatusFromFile

main :: IO ()
main = do
    putStrLn "starting Owen"
    cfg <- configDir
    db <- dbDir
    createDirectoryIfMissing True cfg
    createDirectoryIfMissing True db
    tok <- readFile (cfg <> "token.txt")
    putStrLn ("[Info] Token: " ++ tok)
    owen tok
