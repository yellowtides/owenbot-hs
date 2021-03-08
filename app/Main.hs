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

import           EventHandler           ( handleEvent )
import           Admin                  ( sendGitInfoChan )
import           Status                 ( setStatusFromFile )
import           Utils                  ( sendMessageChan )

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
    --this channel is the bot-start channel on the test server, change to point towards your channel.
    let chan = 801763198792368129 :: ChannelId
    sendMessageChan chan (T.pack "Hewwo, I am bawck! UwU")
    _ <- sendGitInfoChan chan
    _ <- setStatusFromFile
    pure ()

main :: IO ()
main = do
    putStrLn "starting Owen"
    createDirectoryIfMissing True ".owen"
    tok <- readFile ".token.txt"  
    putStrLn ("Token: " ++ tok)
    owen tok
