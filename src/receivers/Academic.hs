{-# LANGUAGE OverloadedStrings #-}

module Academic (commands) where

import Control.Monad (void)
import Data.Char (isDigit)
import qualified Data.Text as T
import Data.List (intercalate)
import Text.Parsec (label, many1, sepBy1, digit, char, try)

import Discord.Types
import Discord

import Command.Parser
import Command
import Utils (respondAsset)

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
theorem :: (MonadDiscord m, MonadIO m) => Command m
theorem = alias "thm" $ command "theorem" $ \m subject number -> do
    case (subject :: T.Text) of
        "ila" -> case number of
            x@TwoDotSeparated{} -> do
                let path = "ila/theorems/" <> show x <> ".png"
                let name = show x <> ".png"
                respondAsset m ("Theorem " <> T.pack name) path
            _ -> respond m "ILA theorems have the format: XX.YY.ZZ!"
        _ -> respond m "No theorems found for subject!"

-- | Definition.
definition :: (MonadDiscord m, MonadIO m) => Command m
definition = alias "def" $ command "definition" $ \m subject number -> do
    case (subject :: T.Text) of
        "ila" -> case number of
            x@OneDotSeparated{} -> do
                let path = "ila/definitions/" <> show x <> ".png"
                let name = show x <> ".png"
                respondAsset m ("Definition " <> T.pack name) path
            _ -> respond m "ILA definitions have the format: XX.YY!"

        _ -> respond m "No definitions found for subject!"

-- | Lemma.
lemma :: (MonadDiscord m, MonadIO m) => Command m
lemma = alias "lem" $ command "lemma" $ \m subject number -> do
    case (subject :: T.Text) of
        "ila" -> case number of
            x@TwoDotSeparated{} -> do
                let path = "ila/lemmas/" <> show x <> ".png"
                let name = show x <> ".png"
                respondAsset m ("Lemma " <> T.pack name) path
            _ -> respond m "ILA lemmas have the format: XX.YY.ZZ!"
        _ -> respond m "No lemmas found for subject!"

-- | Textbook.
textbook :: (MonadDiscord m, MonadIO m) => Command m
textbook = alias "tb" $ command "textbook" $ \m subject -> case () of
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

    _ -> respond m $ "No textbook registered for `" <> subject <> "`!"

-- | Syllogisms.
syllogisms :: (MonadDiscord m, MonadIO m) => Command m
syllogisms = alias "syl" $ command "syllogisms" $ \m ->
    respondAsset m "id-smash-aristotle.png" "cl/syllogisms.png"

-- | Booleans.
booleans :: (MonadDiscord m, MonadIO m) => Command m
booleans = alias "bool" $ command "booleans" $ \m ->
    respondAsset m "literally-satan.png" "cl/Bool.png"
