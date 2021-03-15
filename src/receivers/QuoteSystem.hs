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

quotePath :: FilePath
quotePath = "registeredQuotes.csv"

maxNameLen :: T.Text  -- TODO: keep as int and cast later
maxNameLen = "32"

-- | `quoteTable` maps quotes to their text.
quoteTable :: IO (HM.HashMap T.Text T.Text)
quoteTable  = do
    quote2DArray <- liftIO $ readCSV quotePath
    let compatibleLines = filter (\line -> length line == 2) quote2DArray
    let listMap = map (\[key, value] -> (key, value)) compatibleLines
    pure $ HM.fromList listMap

fetchQuote :: T.Text -> IO (Maybe T.Text)
fetchQuote quote = HM.lookup quote <$> quoteTable

removeQuote :: T.Text -> IO ()
removeQuote quote = do
    newTable <- HM.delete quote <$> quoteTable
    writeHashMapToCSV quotePath newTable 

receiveQuote :: Message -> DiscordHandler ()
receiveQuote msg = newCommand msg "quote +(.{1,"<>maxNameLen<>"})" $ \quoteCapture -> do
    let quote = head quoteCapture
    quoteTextM <- liftIO $ fetchQuote quote
    sendMessageChan (messageChannel msg) $ case quoteTextM of
        Nothing        -> owoify $ T.concat [
                                       "Nope, nothing there. ",
                                       "Maybe consider `:addQuote [quote] [quote_message]`"
                                   ]
        Just quoteText -> quoteText

addQuote :: Message -> DiscordHandler ()
addQuote msg = newDevCommand msg "addquote +(.{1,"<>maxNameLen<>"}) +(.{1,})" $ \quoteCapture -> do
    let quote = head quoteCapture 
    quoteTextM <- liftIO $ fetchQuote quote
    case quoteTextM of
        Nothing        -> do
                            liftIO $ addToCSV quotePath [quoteCapture]
                            let successMessage = owoify 
                                    $ "New quote registered under `:quote " <> quote <> "`."
                            sendMessageChan (messageChannel msg) successMessage
        Just quoteText -> sendMessageChan (messageChannel msg) . owoify
                              $ "Quote already exists my dude, try `:quote " <> quote <> "`."

rmQuote :: Message -> DiscordHandler ()
rmQuote msg = newDevCommand msg "rmquote +(.{1,10})" $ \quoteCapture -> do
    let quote = head quoteCapture
    quoteTextM <- liftIO $ fetchQuote quote
    case quoteTextM of
        Nothing         -> sendMessageChan (messageChannel msg) 
                              $ owoify "Cannot remove that which doesn't exist."
        Just _          -> do
            liftIO $ removeQuote quote
            sendMessageChan (messageChannel msg) . owoify
                $ "All done! Forgot all about `" <> quote <> "`, was super bad anyways." 

receivers :: [Message -> DiscordHandler ()]
receivers = [ receiveQuote, addQuote, rmQuote ]
