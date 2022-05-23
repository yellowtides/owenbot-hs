{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Quiz where

import Control.Concurrent
import Control.Monad (void)
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Aeson
import Data.Function ((&))
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as T (toStrict)
import qualified Data.Text.Lazy.Builder as T
import Data.Time.Clock
import Data.Time.Format
import HTMLEntities.Decoder
import Network.HTTP.Conduit (simpleHttp)
import System.Random (getStdRandom, randomR)
import Text.Printf (printf)

import Discord
import Discord.Interactions
import Discord.Requests
import Discord.Types

import Command
import Config
import DB
import Owoifier

randomQuizScheduler :: OwenConfig -> IO ()
randomQuizScheduler cfg = do
    -- once every 1 - 4 days, random
    minutes <- getStdRandom $ randomR (1440 * 1, 1440 * 4)
    threadDelay $ minutes * 60 * 10 ^ (6 :: Int)
    -- check time is not in the first 6 hours of UTC, since no one's awake
    UTCTime d t <- getCurrentTime
    if t < (6 * 3600) then threadDelay $ 6 * 3600 * 10 ^ (6 :: Int) else pure ()
    quiz <- getQuiz
    let quizChannel = owenConfigQuizChan cfg
    case quiz of
        Left s -> print $ "Error while getting scheduled quiz: " <> s
        Right quiz ->
            runReaderT (sendQuiz quizChannel quiz) (Auth $ owenConfigToken cfg)
    randomQuizScheduler cfg

interactionReceivers :: [Interaction -> DiscordHandler ()]
interactionReceivers = [selectListener]


-- This is an absolutely blasphemous function that abuses so many things that I
-- (Yuto) am too embarrassed to look over, plus it's a goddamn monolith, but it
-- works so i'm not gonna touch it.
--
-- Assumptions:
--   there is only one quiz running at any time (hence the use of GlobalDB)
--   state is not tracked with mvars (hence the abuse of DB and dropdown values)
--   quiz is only ever sent in the inf server, nowhere else
--
-- if we ever rewrite owen to have a central mvar threading through, maintaining
-- a Set of userids or even HashMap<GuildId, Set<UserId>> is a much better
-- solution than this.
selectListener :: Interaction -> DiscordHandler ()
selectListener i@(InteractionComponent{}) = case componentData i of
    SelectMenuData "uwu_quiz" [chosenOption] -> do
        -- get user id regardless of guild or not (mostly for easier debugging)
        let idOfUser = case interactionUser i of
                MemberOrUser (Left  gm) -> userId $ fromJust $ memberUser gm
                MemberOrUser (Right u ) -> userId u
        -- get the users who answered the ongoing quiz already
        users <- liftIO $ readListDB (GlobalDB "quiz_answerers")
        if (T.pack $ show idOfUser) `elem` users
            then
                call $ CreateInteractionResponse (interactionId i) (interactionToken i)
                $ InteractionResponseChannelMessage
                $ (interactionResponseMessageBasic
                    ":rage: You've already answered this quiz!"
                  )
                    { interactionResponseMessageFlags =
                        Just $ InteractionResponseMessageFlags
                            [InteractionResponseMessageFlagEphermeral]
                    }
            else do
                liftIO $ writeListDB
                    (GlobalDB "quiz_answerers")
                    ((T.pack $ show idOfUser) : users)
                -- now move onto actually handling the answer
                case T.take 9 chosenOption of
                    "incorrect" -> do
                        call $ CreateInteractionResponse (interactionId i) (interactionToken i)
                            $ InteractionResponseChannelMessage
                            $ (interactionResponseMessageBasic "٩(× ×)۶ Wrong answer!!")
                                { interactionResponseMessageFlags =
                                    Just $ InteractionResponseMessageFlags
                                        [InteractionResponseMessageFlagEphermeral]
                                }
                    _ -> do
                        let currentTime = snowflakeCreationDate $ unId $ interactionId i
                        let ogContent   = messageContent $ interactionMessage i
                        let ogTime      = messageTimestamp $ interactionMessage i
                        let diffTime    = diffUTCTime currentTime ogTime
                        let timeTaken =
                                (realToFrac diffTime :: Double) & printf "%.2f seconds"
                        call $ CreateInteractionResponse (interactionId i) (interactionToken i)
                            $ InteractionResponseUpdateMessage
                            $ InteractionResponseMessage
                                { interactionResponseMessageTTS             = Nothing
                                , interactionResponseMessageContent = Just $ mconcat
                                    [ ogContent
                                    , "\n"
                                    , chosenOption
                                    , " - <@"
                                    , T.pack $ show idOfUser
                                    , "> correctly answered in "
                                    , T.pack timeTaken
                                    , "! (๑˃ᴗ˂)ﻭ"
                                    ]
                                , interactionResponseMessageEmbeds          = Nothing
                                , interactionResponseMessageAllowedMentions = Nothing
                                , interactionResponseMessageFlags           = Nothing
                                , interactionResponseMessageComponents      = Just []
                                , interactionResponseMessageAttachments     = Nothing
                                }
    _ -> pure ()
selectListener _ = pure ()

-- | A datatype for a quiz.
data Quiz = Quiz
    { quizQuestion         :: T.Text
    , quizIncorrectAnswers :: [T.Text]
    , quizCorrectAnswer    :: T.Text
    }

-- | how it parses the JSON from the API
instance FromJSON Quiz where
    parseJSON = withObject "Whole Result" $ \o -> do
        response_code <- o .: "response_code"
        case (response_code :: Int) of
            0 -> do
                action <- o .: "results"
                case action of
                    [result] -> do
                        question         <- result .: "question"
                        incorrectAnswers <- result .: "incorrect_answers"
                        correctAnswer    <- result .: "correct_answer"
                        pure $ Quiz question incorrectAnswers correctAnswer
                    _ -> fail "Expected one result"
            _ -> fail "Expected response_code 0"

-- | Generates a random Int from 0 to n inclusive
roll :: Int -> IO Int
roll n = getStdRandom $ randomR (0, n)

-- | Insert an element at a random position within a list.
-- This isn't as effective as a random permutation but it's straightforward.
insertRandomly :: a -> [a] -> IO [a]
insertRandomly x xs = do
    i <- roll $ length xs
    pure $ take i xs ++ [x] ++ drop i xs

-- | Decode things like &#1234; into the actual character.
decodeHTMLChars :: T.Text -> T.Text
decodeHTMLChars = T.toStrict . T.toLazyText . htmlEncodedText

-- | Send a given quiz to the given channel.
sendQuiz :: (MonadDiscord m, MonadIO m) => ChannelId -> Quiz -> m ()
sendQuiz channelId q = do
    -- label all of the incorrect answers as "incorrect0", "incorrect1", etc.
    let incorrects = zip (quizIncorrectAnswers q) [0 ..]
            & map (\(a, i) -> mkSelectOption a $ "incorrect" <> (T.pack . show) i)
    -- the correct answer has the correct answer as its value as well, so we can
    -- show it later.
    let correct = mkSelectOption (quizCorrectAnswer q) (quizCorrectAnswer q)
    answers <- liftIO $ insertRandomly correct incorrects

    let numberedAnswers = zipWith
            (\o e -> o { selectOptionEmoji = Just $ mkEmoji e })
            answers
            ["1️⃣", "2️⃣", "3️⃣", "4️⃣", "5️⃣"]

    liftIO $ writeListDB (GlobalDB "quiz_answerers") []
    void $ call $ CreateMessageDetailed channelId $ def
        { messageDetailedContent    = "**Random Trivia: " <> quizQuestion q <> "**"
        , messageDetailedComponents = Just
            [ActionRowSelectMenu $ mkSelectMenu "uwu_quiz" numberedAnswers]
        }

-- | Download a quiz with some tendency towards certain difficulties or categories.
getQuiz :: IO (Either String Quiz)
getQuiz = do
    i <- roll 9
    -- 20% chance of Science: Mathematics, 30% chance of Science: Computers,
    -- and 50% chance of general trivia.
    let category =
            if i < 2 then "&category=19" else if i < 5 then "&category=18" else ""
    j <- roll 9
    -- 50% chance of medium difficulty, 40% of hard, and 10% of easy.
    let difficulty = if j < 5
            then "&difficulty=medium"
            else if i < 9 then "&difficulty=hard" else "&difficulty=easy"
    r <-
        simpleHttp
        $  "https://opentdb.com/api.php?amount=1&type=multiple"
        <> category
        <> difficulty
    case eitherDecode r of
        Right Quiz {..} -> pure $ Right $ Quiz
            { quizQuestion         = decodeHTMLChars quizQuestion
            , quizCorrectAnswer    = decodeHTMLChars quizCorrectAnswer
            , quizIncorrectAnswers = map decodeHTMLChars quizIncorrectAnswers
            }
        x -> pure x
