{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad 
import Discord.Requests as R
import Discord
    ( restCall,
      runDiscord,
      def,
      DiscordHandler,
      RunDiscordOpts(discordToken, discordOnStart, discordOnEvent,
                     discordOnLog) )
import Discord.Types ( ChannelId, Channel(ChannelText) )
import EventHandler (handleEvent)
import Admin (sendInstanceInfo)

-- | UWU
owen :: T.Text -> IO ()
owen t = do
    userFacingError <- runDiscord $ def { discordToken   = t
                                        , discordOnStart = startHandler
                                        , discordOnEvent = handleEvent
                                        , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn "" }
    TIO.putStrLn userFacingError

startHandler :: DiscordHandler ()
startHandler = do
    let chan = 801763198792368129 ::ChannelId --this channel is the bot-start channel on the test server, change to point towards your channel.
    _ <- restCall $ R.CreateMessage chan "@here Hewwo, I am bawck! UwU"
    _ <- sendInstanceInfo chan
    pure ()

isTextChannel :: Channel -> Bool
isTextChannel ChannelText {} = True
isTextChannel _ = False

main :: IO ()
main = do
    putStrLn "starting Owen"
    tok <- TIO.readFile ".token.txt"  
    putStrLn ("Token:" ++ T.unpack tok)
    owen tok
