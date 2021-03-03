module Main where

import qualified Data.Text as T
import           Control.Monad 
import           Discord.Requests as R
import           Discord                ( restCall
                                        , runDiscord
                                        , def
                                        , DiscordHandler
                                        , RunDiscordOpts ( discordToken
                                                         , discordOnStart
                                                         , discordOnEvent
                                                         , discordOnLog
                                                         )
                                        )
import           Discord.Types          ( ChannelId
                                        , Channel ( ChannelText )
                                        )
import           EventHandler           ( handleEvent )
import           Status                 ( setStatusFromFile )
import           Admin                  ( sendGitInfoChan )

-- | UWU
owen :: String -> IO ()
owen t = do
    userFacingError <- runDiscord $ def { discordToken   = T.pack t
                                        , discordOnStart = startHandler
                                        , discordOnEvent = handleEvent
                                        , discordOnLog = \s -> 
                                            putStrLn (T.unpack s) >> putStrLn ""}
    putStrLn (T.unpack userFacingError)

startHandler :: DiscordHandler ()
startHandler = do
    --this channel is the bot-start channel on the test server, change to point towards your channel.
    let chan = 801763198792368129 :: ChannelId
    _ <- restCall $ R.CreateMessage chan (T.pack "Hewwo, I am bawck! UwU")
    _ <- sendGitInfoChan chan
    _ <- setStatusFromFile
    pure ()

isTextChannel :: Channel -> Bool
isTextChannel ChannelText {} = True
isTextChannel _ = False

main :: IO ()
main = do
    putStrLn "starting Owen"
    tok <- readFile ".token.txt"  
    putStrLn ("Token: " ++ tok)
    owen tok
