{-# LANGUAGE OverloadedStrings #-}

module Academic where

import           Data.Bifunctor             ( first )
import           Control.Monad              ( void )
import qualified Data.Text as T
import           Data.List                  ( intercalate )
import           Text.Parsec.Combinator
import           Text.Parsec                ( digit
                                            , (<|>)
                                            , try
                                            , char
                                            )

import           Discord.Types
import           Discord

import           Command.Parser
import           Command
import           Utils                      ( sendAssetChan )

receivers :: [Message -> DiscordHandler ()]
receivers =
    [ runCommand textbook
    , runCommand theorem
    , runCommand definition
    , runCommand lemma
    , runCommand syllogisms
    , runCommand booleans 
    ]

-- | @respondAsset m name path@ responds to the message @m@ with the file at
-- @path@, with the name overridden as @name@.
respondAsset :: (MonadDiscord m, MonadIO m) => Message -> T.Text -> FilePath -> m ()
respondAsset m name path = sendAssetChan (messageChannel m) name path

-- | datatype representing possible textbook asset number formats
data TextbookAssetNumber
    = TwoDotSeparated Int Int Int
    | OneDotSeparated Int Int

instance Show TextbookAssetNumber where
    show (TwoDotSeparated a b c) = intercalate "." $ padFirst $ map show [a, b, c]
    show (OneDotSeparated a b) = intercalate "." $ padFirst $ map show [a, b]

padFirst :: [String] -> [String]
padFirst (x:xs) = (padZeroes 2 x):xs

padZeroes :: Int -> String -> String
padZeroes newLen digits = replicate (newLen - length digits) '0' <> digits

instance ParsableArgument TextbookAssetNumber where
    parserForArg m = do
        a <- read <$> many1 digit
        void $ char '.'
        b <- read <$> many1 digit
        parsed <- try (do
            void $ char '.'
            c <- read <$> many1 digit
            pure $ TwoDotSeparated a b c
            ) <|> (pure $ OneDotSeparated a b)
        endOrSpaces
        pure parsed

-- | @thmDefLemErrorHandler@ returns the usage on argument error, and silences
-- any other types of command errors (like "File not found" for theorems that
-- do not exist).
thmDefLemErrorHandler
    :: (MonadDiscord m) 
    => T.Text
    -- ^ Usage text 
    -> Message
    -- ^ Message that triggered it
    -> CommandError
    -> m ()
thmDefLemErrorHandler usage m (ArgumentParseError reason) =
    respond m $ "Required format: `" <> usage <> "`"
thmDefLemErrorHandler _ _ _ = pure ()

-- | Theorem.
theorem :: (MonadDiscord m, MonadIO m) => Command m
theorem
    = onError (thmDefLemErrorHandler ":theorem XX.YY.ZZ")
    $ alias "thm" 
    $ command "theorem" $ \m subject number -> do
        case (subject :: T.Text) of
            "ila" -> case number of
                OneDotSeparated{} ->
                    respond m "Theorem numbers in ILA need two dots!"
                TwoDotSeparated{} -> do
                    let path = "ila/theorems/" <> show number <> ".png"
                    let name = show number <> ".png"
                    respondAsset m ("Theorem " <> (T.pack name)) path
            _ -> 
                respond m "No theorems found for subject!"

-- | Definition.
definition :: (MonadDiscord m, MonadIO m) => Command m
definition
    = onError (thmDefLemErrorHandler ":definition XX.YY")
    $ alias "def"
    $ command "definition" $ \m subject number -> do
        case (subject :: T.Text) of
            "ila" -> case number of
                TwoDotSeparated{} ->
                    respond m "Definition numbers in ILA need one dot!"
                OneDotSeparated{} -> do
                    let path = "ila/definitions/" <> show number <> ".png"
                    let name = show number <> ".png"
                    respondAsset m ("Definition " <> (T.pack name)) path
            _ ->
                respond m "No definitions found for subject!"

-- | Lemma.
lemma :: (MonadDiscord m, MonadIO m) => Command m
lemma
    = onError (thmDefLemErrorHandler ":lemma XX.YY.ZZ")
    $ alias "lem"
    $ command "lemma" $ \m subject number -> do
        case (subject :: T.Text) of
            "ila" -> case number of
                OneDotSeparated{} ->
                    respond m "Lemma numbers in ILA need two dots!"
                TwoDotSeparated{} -> do
                    let path = "ila/lemmas/" <> show number <> ".png"
                    let name = show number <> ".png"
                    respondAsset m ("Lemma " <> (T.pack name)) path
            _ ->
                respond m "No lemmas found for subject!"

-- | Textbook.
textbook :: (MonadDiscord m, MonadIO m) => Command m
textbook = alias "tb" $ command "textbook" $ \m subject ->
    case () of
        _ | subject `elem` ["i1a", "inf1a"] ->
            respondAsset m "the-holy-bible-2.png" "textbooks/i1a-textbook.pdf"

        _ | subject `elem` ["calc", "cap"] ->
            respond m $ "The textbook can be found here:\n" <> 
                "http://gen.lib.rus.ec/book/index.php?md5=13ecb7a2ed943dcb4a302080d2d8e6ea"

        _ | subject == "ila" ->
            respondAsset m "ila-textbook.pdf" "textbooks/ila-textbook.pdf"

        otherwse ->
            respond m $ "No textbook registered for `" <> subject <> "`!"

-- | Syllogisms.
syllogisms :: (MonadDiscord m, MonadIO m) => Command m
syllogisms = alias "syl" $ command "syllogisms" $ \m ->
    respondAsset m "id-smash-aristotle.png" "cl/syllogisms.png"

-- | Booleans.
booleans :: (MonadDiscord m, MonadIO m) => Command m
booleans = alias "bool" $ command "booleans" $ \m ->
    respondAsset m "literally-satan.png" "cl/Bool.png"

