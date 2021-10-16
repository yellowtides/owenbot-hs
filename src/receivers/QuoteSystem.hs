{-# LANGUAGE OverloadedStrings #-}

module QuoteSystem (commands) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Discord
import Discord.Types
import UnliftIO (liftIO)

import Command
import DB
import Owoifier (owoify)
import Text.Parsec (anyChar, many1)
import Utils (modPerms, sendMessageChan)

commands :: [Command DiscordHandler]
commands = [receiveQuote, receiveQuoteShorthand, addQuote, rmQuote, listQuotes]

quotesTable :: DBTable
quotesTable = GlobalDB "registeredQuotes"

maxNameLen :: Int
maxNameLen = 32

nameRE :: T.Text
nameRE = "(.{1," <> T.pack (show maxNameLen) <> "})"

storeQuote :: T.Text -> T.Text -> IO ()
storeQuote name content = appendDB quotesTable [[name, content]]

fetchQuote :: T.Text -> IO (Maybe T.Text)
fetchQuote name = HM.lookup name <$> readHashMapDB quotesTable

removeQuote :: T.Text -> IO ()
removeQuote name = do
    newTable <- HM.delete name <$> readHashMapDB quotesTable
    writeHashMapDB quotesTable newTable

receiveQuoteShorthand :: (MonadDiscord m, MonadIO m) => Command m
receiveQuoteShorthand = prefix "::" $ parsecCommand (many1 anyChar) $ \m name -> do
    runCommand receiveQuote $ m { messageText = ":quote " <> T.pack name }

receiveQuote :: (MonadDiscord m, MonadIO m) => Command m
receiveQuote = command "quote" $ \m (Remaining name) -> do
    textM <- liftIO $ fetchQuote name
    respond m $ case textM of
        Nothing ->
            owoify
                $ T.concat
                    [ "Nope, nothing there. "
                    , "Maybe consider `:addquote [quote] [quote_message]`"
                    ]
        Just text -> text

addQuote :: (MonadDiscord m, MonadIO m) => Command m
addQuote = command "addquote" $ \m name (Remaining content) -> case messageGuild m of
    Nothing -> respond m "Only possible in a server!"
    Just _  -> do
        textM <- liftIO $ fetchQuote name
        case textM of
            Nothing -> if T.length name <= maxNameLen
                -- do a length check because this is no longer regex
                then do
                    liftIO $ storeQuote name content
                    respond m $ "New quote registered under `:quote " <> name <> "`."
                else respond m "Please make the name less than 32 chars!"
            Just _ ->
                respond m
                    .  owoify
                    $  "Quote already exists my dude, try `:quote "
                    <> name
                    <> "`."

rmQuote :: (MonadDiscord m, MonadIO m) => Command m
rmQuote = requires modPerms $ command "rmquote" $ \m name -> do
    textM <- liftIO $ fetchQuote name
    case textM of
        Nothing -> respond m $ owoify "Cannot remove that which doesn't exist."
        Just _  -> do
            liftIO $ removeQuote name
            respond m
                .  owoify
                $  "All done! Forgot all about `"
                <> name
                <> "`, was super bad anyways."

listQuotes :: (MonadDiscord m, MonadIO m) => Command m
listQuotes = help "Lists all quotes" $ command "listQuotes" $ \m -> do
    quoteNames <- liftIO $ HM.keys <$> readHashMapDB quotesTable
    respond m $ T.unlines $ map (\x -> "`" <> x <> "`") quoteNames
