{-# LANGUAGE OverloadedStrings #-}

module QuoteSystem (commands) where

import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as T
import Discord
import Discord.Types
import UnliftIO (liftIO)

import Command
import DB
import Owoifier (owoify)
import Text.Parsec (anyChar, many1)
import Utils (modPerms, sendMessageChan, sentInServer)

commands :: [Command DiscordHandler]
commands = [receiveQuote, receiveQuoteShorthand, addQuote, rmQuote, listQuotes]

quotesTable :: DBTable
quotesTable = GlobalDB "registeredQuotes"

maxNameLen :: Int
maxNameLen = 32

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
receiveQuote =
    help "Call a registered quote. Usage: `:quote <name>` or alias `::<name>`"
        . command "quote"
        $ \m (Remaining name) -> do
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
addQuote =
    requires sentInServer
        . help
            (  "Register a new quote. Up to one attachment accepted. "
            <> "Usage: `:addquote <name> [content|attachment]` "
            )
        . command "addquote"
        $ \m name mbContent -> do
            textM <- liftIO $ fetchQuote name
            case textM of
                Nothing -> if T.length name > maxNameLen
                    then respond m "Please make the name less than 32 chars!"
                    else if isNothing mbContent && null (messageAttachments m)
                        then respond
                            m
                            "You need to provide a quote content or an attachment!"
                        else do
                            -- There is either an attachment, a content, or both guaranteed
                            let (Remaining content) =
                                    fromMaybe (Remaining "") mbContent
                                attachmentUrlPart = if null (messageAttachments m)
                                    then ""
                                    else
                                        "\n" <> attachmentUrl
                                            (head (messageAttachments m))
                            liftIO $ storeQuote name $ content <> attachmentUrlPart
                            respond m
                                $  "New quote registered under `:quote "
                                <> name
                                <> "`."
                Just _ ->
                    respond m
                        .  owoify
                        $  "Quote already exists my dude, try `:quote "
                        <> name
                        <> "`."

rmQuote :: (MonadDiscord m, MonadIO m) => Command m
rmQuote =
    requires modPerms
        . help "Removes a quote. Usage: `:rmquote <name>`"
        . command "rmquote"
        $ \m name -> do
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
