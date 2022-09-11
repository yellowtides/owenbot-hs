{-# LANGUAGE OverloadedStrings #-}

module Academic (commands) where

import Control.Exception.Safe (catchAny)
import Control.Monad (void)
import Data.Char (isDigit)
import Data.List (intercalate)
import qualified Data.Text as T
import Text.Parsec (char, digit, label, many1, sepBy1, try)

import Discord
import Discord.Types

import Command
import Command.Parser
import Utils (respond, respondAsset)

commands :: [Command DiscordHandler]
commands = [textbook, theorem, definition, lemma, syllogisms, booleans]

-- | datatype representing possible textbook asset number formats
data TextbookAssetNumber
    = TwoDotSeparated Int Int Int
    | OneDotSeparated Int Int
    | UnknownSeparation [Int]

-- | used to create the file name
instance Show TextbookAssetNumber where
    show (TwoDotSeparated a b c) = intercalate "." $ padFirst $ map show [a, b, c]
    show (OneDotSeparated a b  ) = intercalate "." $ padFirst $ map show [a, b]
    show (UnknownSeparation xs ) = show xs

instance ParsableArgument TextbookAssetNumber where
    parserForArg = do
        a <- flip label "an asset number" $ try $ sepBy1 parserForArg (char '.')
        pure $ case a of
            [x, y]    -> OneDotSeparated x y
            [x, y, z] -> TwoDotSeparated x y z
            xs        -> UnknownSeparation xs

padFirst :: [String] -> [String]
padFirst []       = []
padFirst (x : xs) = padZeroes 2 x : xs

padZeroes :: Int -> String -> String
padZeroes newLen digits = replicate (newLen - length digits) '0' <> digits

-- | Theorem.
theorem :: Command DiscordHandler
theorem =
    alias "thm"
        . help "Call an ILA theorem.\nUsage: `:theorem <theorem number>`"
        . command "theorem"
        $ \m number ->
    -- When some subject in the future needs to be added (so far we have had none),
    -- then, add a `subject` argument: `:theorem <subject> <theorem number>`
                       case number of
            x@TwoDotSeparated{} -> do
                let path = "ila/theorems/" <> show x <> ".png"
                let name = show x <> ".png"
                catchAny (respondAsset m ("Theorem " <> T.pack name) path) $ \e -> do
                    respond m "Theorem not found."
            _ -> respond m "ILA theorems have the format: XX.YY.ZZ!"

-- | Definition.
definition :: Command DiscordHandler
definition =
    alias "def"
        . help "Call an ILA definition.\nUsage: `:definition <def number>`"
        . command "definition"
        $ \m number -> do
            case number of
                x@OneDotSeparated{} -> do
                    let path = "ila/definitions/" <> show x <> ".png"
                    let name = show x <> ".png"
                    catchAny (respondAsset m ("Definition " <> T.pack name) path)
                        $ \e -> do
                            respond m "Definition not found."
                _ -> respond m "ILA definitions have the format: XX.YY!"

-- | Lemma.
lemma :: Command DiscordHandler
lemma =
    alias "lem"
        . help "Call an ILA lemma.\nUsage: `:lemma <lemma number>`"
        . command "lemma"
        $ \m number -> do
            case number of
                x@TwoDotSeparated{} -> do
                    let path = "ila/lemmas/" <> show x <> ".png"
                    let name = show x <> ".png"
                    catchAny (respondAsset m ("Lemma " <> T.pack name) path) $ \e -> do
                        respond m "Lemma not found."
                _ -> respond m "ILA lemmas have the format: XX.YY.ZZ!"

-- | Textbook.
textbook :: Command DiscordHandler
textbook =
    alias "tb"
        . help
            (  "Find the online textbooks for various subjects.\n"
            <> "Usage: `:textbook <subject>`"
            )
        . command "textbook"
        $ \m subject -> case () of
            _ | subject `elem` ["i1a", "inf1a"] ->
                respondAsset m "the-holy-bible-2.png" "textbooks/i1a-textbook.pdf"

            _ | subject `elem` ["calc", "cap"] ->
                respond m
                    $ "The textbook can be found here:\n"
                    <> "||http://gen.lib.rus.ec/book/index.php?md5=13ecb7a2ed943dcb4a302080d2d8e6ea||"

            _ | subject == "ila" ->
                respondAsset m "ila-textbook.pdf" "textbooks/ila-textbook.pdf"

            _ | subject == "dmp" ->
                respond m
                    $ "The textbook can be found here:\n"
                    <> "||https://gen.lib.rus.ec/book/index.php?md5=45624cd7153af9bd024f64305e8b7be0||"

            _ | subject == "inf2c" ->
                respond m
                    $ "Someone has posted the PDF here:\n"
                    <> "https://discord.com/channels/755798054455738489/887640828128952330/889898467852427345\n"
                    <> "And the EPUB here:\n"
                    <> "https://discord.com/channels/755798054455738489/887640828128952330/889898536852934676"
            _ -> respond m $ "No textbook registered for `" <> subject <> "`!"

-- | Syllogisms.
syllogisms :: Command DiscordHandler
syllogisms =
    alias "syl"
        . help "See the list of aristotle syllogisms!"
        . command "syllogisms"
        $ \m -> respondAsset m "id-smash-aristotle.png" "cl/syllogisms.png"

-- | Booleans.
booleans :: Command DiscordHandler
booleans =
    alias "bool"
        . help "See the list of boolean algebra rules!"
        . command "booleans"
        $ \m -> respondAsset m "literally-satan.png" "cl/Bool.png"
