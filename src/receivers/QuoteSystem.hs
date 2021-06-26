{-# LANGUAGE OverloadedStrings #-}

module QuoteSystem ( receivers ) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import           Discord.Types
import           Discord
import           UnliftIO       ( liftIO )

import           CSV            ( addToCSV
                                , readCSV
                                , writeHashMapToCSV
                                )
import           Owoifier       ( owoify )
import           Utils          ( sendMessageChan
                                , modPerms
                                )
import           Command
import           Text.Parsec    ( many1
                                , anyChar
                                )

receivers :: [Message -> DiscordHandler ()]
receivers = map runCommand [ receiveQuote, receiveQuoteShorthand, addQuote, rmQuote ]

quotePath :: FilePath
quotePath = "registeredQuotes.csv"

maxNameLen :: Int
maxNameLen = 32

nameRE :: T.Text
nameRE = "(.{1," <> T.pack (show maxNameLen) <> "})"

-- | `quoteTable` maps quotes to their text.
quoteTable :: IO (HM.HashMap T.Text T.Text)
quoteTable = do
    quote2DArray <- readCSV quotePath
    let compatibleLines = filter (\line -> length line == 2) quote2DArray
    let listMap = map (\[key, value] -> (key, value)) compatibleLines
    return $ HM.fromList listMap

storeQuote :: T.Text -> T.Text -> IO ()
storeQuote name content = addToCSV quotePath [[name, content]]

fetchQuote :: T.Text -> IO (Maybe T.Text)
fetchQuote name = HM.lookup name <$> quoteTable

removeQuote :: T.Text -> IO ()
removeQuote name = do
    newTable <- HM.delete name <$> quoteTable
    writeHashMapToCSV quotePath newTable

receiveQuoteShorthand :: (MonadDiscord m, MonadIO m) => Command m
receiveQuoteShorthand = prefix "::" $ parsecCommand (many1 anyChar) $ \m name -> do
    runCommand receiveQuote $ m { messageText = ":quote " <> T.pack name }


receiveQuote :: (MonadDiscord m, MonadIO m) => Command m
receiveQuote = command "quote" $ \m name -> do
    textM <- liftIO $ fetchQuote name
    respond m $ case textM of
        Nothing   -> owoify $ T.concat [
                "Nope, nothing there. ",
                "Maybe consider `:addquote [quote] [quote_message]`"
            ]
        Just text -> text

addQuote :: (MonadDiscord m, MonadIO m) => Command m
addQuote = command "addquote" $ \m name (Remaining content) ->
    case messageGuild m of
        Nothing -> respond m "Only possible in a server!"
        Just _  -> do
            textM <- liftIO $ fetchQuote name
            case textM of
                Nothing -> if T.length name <= maxNameLen
                    -- do a length check because this is no longer regex
                    then do
                        liftIO $ storeQuote name content
                        respond m $ "New quote registered under `:quote " <> name <> "`."
                    else
                        respond m "Please make the name less than 32 chars!"
                Just _  -> respond m . owoify
                    $ "Quote already exists my dude, try `:quote " <> name <> "`."

rmQuote :: (MonadDiscord m, MonadIO m) => Command m
rmQuote = requires modPerms $ command "rmquote" $ \m name -> do
    textM <- liftIO $ fetchQuote name
    case textM of
        Nothing -> respond m $ owoify "Cannot remove that which doesn't exist."
        Just _  -> do
            liftIO $ removeQuote name
            respond m . owoify
                $ "All done! Forgot all about `" <> name <> "`, was super bad anyways."
