{-# LANGUAGE OverloadedStrings #-}

module Misc ( receivers ) where

import qualified Discord.Requests as R
import           Discord.Types
import           Discord
import qualified Data.Maybe as M
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import           UnliftIO               ( liftIO )
import           Text.Regex.TDFA        ( (=~) )
import           System.IO as S         ( readFile )
import qualified System.Exit as SE
import qualified System.Process as SP
import           Control.Monad          ( when )
import           System.Random          ( randomR
                                        , getStdRandom
                                        )

import           Utils                  ( sendMessageChan
                                        , pingAuthorOf
                                        , newCommand
                                        , (=~=)
                                        )
import           Owoifier               ( owoify )
import           ADAPriceFetcher        ( fetchADADetails )

receivers :: [Message -> DiscordHandler ()]
receivers =
    [ owoifyIfPossible
    , godIsDead
    , thatcherIsDead
    , dadJokeIfPossible
    , handleFortune
    , handleAda24h
    ]

roll :: Int -> IO Int
roll n = getStdRandom $ randomR (1, n)

owoifyIfPossible :: Message -> DiscordHandler ()
owoifyIfPossible m = do
    roll500 <- liftIO $ roll 500
    let isOwoifiable = messageText m =~= "[lLrR]|[nNmM][oO]"
    when (roll500 == 1 && isOwoifiable) $ do
        sendMessageChan (messageChannel m)
            $ pingAuthorOf m <> ": " <> owoify (messageText m)

godIsDead :: Message -> DiscordHandler ()
godIsDead m = do
    let isMatch = messageText m =~= "[gG]od *[iI]s *[dD]ead"
    when isMatch $ do
        liftIO (TIO.readFile "./src/assets/nietzsche.txt")
            >>= sendMessageChan (messageChannel m) . owoify

thatcherIsDead :: Message -> DiscordHandler ()
thatcherIsDead m = do
  when (messageText m =~= "thatcher *[Ii]s *[Dd]ead") $ do 
    sendMessageChan (messageChannel m)
        $ "https://www.youtube.com/watch?v=ILvd5buCEnU"

dadJokeIfPossible :: Message -> DiscordHandler ()
dadJokeIfPossible m = do
    let name = attemptParseDadJoke (messageText m)
    when (M.isJust name) $ do
        let Just n = name 
        roll10 <- liftIO $ roll 20
        when (roll10 == 1 && (T.length n) >= 3) $ do
            sendMessageChan (messageChannel m)
                $ owoify ("hello " <> n <> ", i'm owen")

attemptParseDadJoke :: T.Text -> Maybe T.Text
attemptParseDadJoke t = 
    case captures of
        [] -> Nothing
        e  -> Just (head captures :: T.Text)
  where
    match :: (T.Text, T.Text, T.Text, [T.Text])
    match@(_, _, _, captures) =
        t =~ ("^[iI] ?[aA]?'?[mM] +([a-zA-Z'*]+)([!;:.,?~-]+| *$)" :: T.Text)

handleFortune :: Message -> DiscordHandler ()
handleFortune m = newCommand m "fortune" $ \_ -> do
    cowText <- liftIO fortuneCow 
    sendMessageChan (messageChannel m)
        $ "```" <> T.pack cowText <> "```"

fortune :: IO String
fortune = SP.readProcess "fortune" [] []

fortuneCow :: IO String
fortuneCow = do
    f <- T.pack <$> fortune
    SP.readProcess "cowsay" [] . T.unpack $ owoify f

handleAda24h :: Message -> DiscordHandler ()
handleAda24h m = newCommand m "ada24h" $ \_ -> do
    adaAnnouncementM <- liftIO fetchADADetails
    case adaAnnouncementM of
        Left err -> 
            liftIO (putStrLn $ "Cannot fetch ADA details from Binance: " ++ err)
                >> pure () 
        Right announcement ->
            sendMessageChan (messageChannel m)
                $ owoify $ T.pack announcement