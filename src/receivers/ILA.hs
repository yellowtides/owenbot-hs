{-# LANGUAGE OverloadedStrings #-}

module ILA ( receivers ) where

import           Discord.Types      ( ChannelId
                                    , Message ( messageChannel )
                                    )
import           Discord            ( DiscordHandler )
import qualified Data.Text as T
import           Data.Bifunctor     ( first )
import           Data.Char          ( isAlpha
                                    , isSpace
                                    )

import           Utils              ( sendMessageChan
                                    , sendFileChan
                                    , newCommand
                                    )
import           TemplateRE         ( oneDot
                                    , twoDot
                                    , trailingWS
                                    , thmRE
                                    , defRE
                                    , lemRE
                                    , textbookRE
                                    )

receivers :: [Message -> DiscordHandler ()]
receivers =
    [ sendThm
    , sendDef
    , sendLem
    , sendTextbook
    ]

ilathmRE, iladefRE, ilalemmaRE, ilatextbookRE :: T.Text
ilathmRE      = thmRE      <> "ila (" <> twoDot <> ")"
iladefRE      = defRE      <> "ila (" <> oneDot <> ")"
ilalemmaRE    = lemRE      <> "ila (" <> twoDot <> ")"
ilatextbookRE = textbookRE <> "ila"

sendThm :: Message -> DiscordHandler ()
sendThm m = newCommand m ilathmRE $ \captures -> do
    let content = (head . tail) captures -- first match is (eore) from th(eore)m
    sendFileChan (messageChannel m) ("Theorem "                      <> parse content)
                                    ("./src/assets/ila/theorems/"    ++ parseStr content)

sendDef :: Message -> DiscordHandler ()
sendDef m = newCommand m iladefRE $ \captures -> do
    let content = (head . tail) captures
    sendFileChan (messageChannel m) ("Definition "                   <> parse content)
                                    ("./src/assets/ila/definitions/" ++ parseStr content)

sendLem :: Message -> DiscordHandler ()
sendLem m = newCommand m ilalemmaRE $ \captures -> do
    let content = (head . tail) captures
    sendFileChan (messageChannel m) ("Lemma "                        <> parse content)
                                    ("./src/assets/ila/lemmas/"      ++ parseStr content)

sendTextbook :: Message -> DiscordHandler ()
sendTextbook m = newCommand m ilatextbookRE $ \_ -> do
    sendFileChan (messageChannel m) "ila-textbook.pdf" "./src/assets/textbooks/ila-textbook.pdf"

parseStr :: T.Text -> String
parseStr = T.unpack . parse

parse :: T.Text -> T.Text
parse = (<> ".png") . uncurry (<>) . first (padZeroes 2) . T.breakOn "." .
        T.intercalate "." . map rmZeroes . T.splitOn "."

rmZeroes :: T.Text -> T.Text
rmZeroes digits = case T.dropWhile (== '0') digits of
    "" -> "0"
    d' -> d'

padZeroes :: Int -> T.Text -> T.Text
padZeroes newLen digits = T.replicate (newLen - T.length digits) "0" <> digits
