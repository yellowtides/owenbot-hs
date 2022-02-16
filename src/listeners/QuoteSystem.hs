{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

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
import Utils (devPerms, modPerms, sendMessageChan, sentInServer)

commands :: [Command DiscordHandler]
commands = [quote, quoteShorthand, quoteInText, addQuote, rmQuote, listQuotes]

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

-- | for quotes that appear mid-sentence, with a required space before it.
-- only for single-word quotes. This shorthand will only trigger for the last
-- occurrence of the regex.
quoteInText :: (MonadDiscord m, MonadIO m) => Command m
quoteInText = regexCommand "(.+) ::([^ \n]+)" $ \m (_ : name : _) ->
    (liftIO . fetchQuote) name >>= \case
        Nothing   -> pure ()
        Just text -> respond m text

-- | the double-colon alias for quotes, has to be in its own message to use
-- multi-word quotes. for quotes mid-sentence, the regex one is matched.
quoteShorthand :: (MonadDiscord m, MonadIO m) => Command m
quoteShorthand = prefix "::" $ parsecCommand (many1 anyChar) $ \m name ->
    runCommand quote $ m { messageContent = ":quote " <> T.pack name }

quote :: (MonadDiscord m, MonadIO m) => Command m
quote =
    help
            ("Call a registered quote.\nUsage: `:quote <name>`."
            <> "The alias for the command is `::<name>`. The alias can also be used "
            <> "inline for single-word quotes but will fail silently if it doesn't exist."
            )
        . command "quote"
        $ \m (Remaining name) -> do
            textM <- liftIO $ fetchQuote name
            respond m $ case textM of
                Nothing ->
                    owoify
                        $ mconcat
                            [ "Nope, nothing there. "
                            , "Maybe consider `:addquote [quote] [quote_message]`"
                            ]
                Just text -> text

addQuote :: (MonadDiscord m, MonadIO m) => Command m
addQuote =
    requires sentInServer
        . help
            (  "Register a new quote. Up to one attachment accepted.\n"
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
    requires (modPerms <|> devPerms)
        . help "Removes a quote.\nUsage: `:rmquote <name>`"
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
    respond m $ T.intercalate ", " $ map (\x -> "`" <> x <> "`") quoteNames
