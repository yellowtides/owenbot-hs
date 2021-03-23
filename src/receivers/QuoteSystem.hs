{-# LANGUAGE OverloadedStrings #-}

module QuoteSystem ( receivers ) where

import Discord.Types            ( Message(messageChannel) )
import Discord                  ( DiscordHandler )
import Utils                    ( sendMessageChan
                                , newCommand
                                , newDevCommand
                                )

import UnliftIO                 ( liftIO )

import Owoifier                 ( owoify )

import CSV                      ( addToCSV
                                , readCSV
                                , writeHashMapToCSV
                                )

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

receivers :: [Message -> DiscordHandler ()]
receivers = [ receiveQuote, addQuote, rmQuote ]

quotePath :: FilePath
quotePath = "registeredQuotes.csv"

maxNameLen :: T.Text
maxNameLen = T.pack $ show 32

-- | `quoteTable` maps quotes to their text.
quoteTable :: IO (HM.HashMap T.Text T.Text)
quoteTable = do
    quote2DArray <- liftIO $ readCSV quotePath
    let compatibleLines = filter (\line -> length line == 2) quote2DArray
    let listMap = map (\[key, value] -> (key, value)) compatibleLines
    pure $ HM.fromList listMap

storeQuote :: T.Text -> [T.Text] -> IO ()
storeQuote name contents = addToCSV quotePath [[name], contents]

fetchQuote :: T.Text -> IO (Maybe T.Text)
fetchQuote name = HM.lookup name <$> quoteTable

removeQuote :: T.Text -> IO ()
removeQuote name = do
    newTable <- HM.delete name <$> quoteTable
    writeHashMapToCSV quotePath newTable

receiveQuote :: Message -> DiscordHandler ()
receiveQuote msg = newCommand msg ("quote +(.{1,"<>maxNameLen<>"})") $ \quote -> do
    let name = head quote
    textM <- liftIO $ fetchQuote name
    sendMessageChan (messageChannel msg) $ case textM of
        Nothing   -> owoify $ T.concat [
                                       "Nope, nothing there. ",
                                       "Maybe consider `:addQuote [quote] [quote_message]`"
                                       ]
        Just text -> text

addQuote :: Message -> DiscordHandler ()
addQuote msg = newDevCommand msg ("addquote +(.{1,"<>maxNameLen<>"}) +(.{1,})") $ \quote -> do
    let name = head quote
    textM <- liftIO $ fetchQuote name
    case textM of
        Nothing -> do
                   liftIO $ storeQuote name (tail quote)
                   let successMessage = "New quote registered under `:quote " <> name <> "`."
                   sendMessageChan (messageChannel msg) successMessage
        Just _  -> sendMessageChan (messageChannel msg) . owoify
                       $ "Quote already exists my dude, try `:quote " <> name <> "`."

rmQuote :: Message -> DiscordHandler ()
rmQuote msg = newDevCommand msg ("rmquote +(.{1,"<>maxNameLen<>"})") $ \quote -> do
    let name = head quote
    textM <- liftIO $ fetchQuote name
    case textM of
        Nothing -> sendMessageChan (messageChannel msg)
                       $ owoify "Cannot remove that which doesn't exist."
        Just _  -> do
                   liftIO $ removeQuote name
                   sendMessageChan (messageChannel msg) . owoify
                       $ "All done! Forgot all about `" <> name <> "`, was super bad anyways."
