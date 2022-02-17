{-# LANGUAGE OverloadedStrings #-}
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

selectListener :: Interaction -> DiscordHandler ()
selectListener i@(InteractionComponent{}) = case interactionDataComponent i of
    InteractionDataComponentSelectMenu "uwu_quiz" [chosenOption] -> do
        let idOfUser = case interactionUser i of
                MemberOrUser (Left  gm) -> userId $ fromJust $ memberUser gm
                MemberOrUser (Right u ) -> userId u
        case T.take 9 chosenOption of
            "incorrect" -> createInteractionResponse
                (interactionId i)
                (interactionToken i)
                InteractionResponseDeferUpdateMessage
            _ -> do
                let ogContent = messageContent $ interactionMessage i
                let ogTime    = messageTimestamp $ interactionMessage i
                diffTime <- (flip diffUTCTime ogTime) <$> liftIO getCurrentTime
                let timeTaken = (realToFrac diffTime :: Double) & printf "%.2f seconds"
                createInteractionResponse (interactionId i) (interactionToken i)
                    $ InteractionResponseUpdateMessage
                    $ InteractionResponseMessage
                        { interactionResponseMessageTTS             = Nothing
                        , interactionResponseMessageContent         = Just $ mconcat
                            [ ogContent
                            , "\n"
                            , chosenOption
                            , weakOwoify " - Correctly answered by <@"
                            , T.pack $ show idOfUser
                            , "> in "
                            , T.pack timeTaken
                            ]
                        , interactionResponseMessageEmbeds          = Nothing
                        , interactionResponseMessageAllowedMentions = Nothing
                        , interactionResponseMessageFlags           = Nothing
                        , interactionResponseMessageComponents      = Just []
                        , interactionResponseMessageAttachments     = Nothing
                        }
    _ -> pure ()

data Quiz = Quiz
    { quizQuestion         :: T.Text
    , quizIncorrectAnswers :: [T.Text]
    , quizCorrectAnswer    :: T.Text
    }

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

-- | Generates a random Int from 0 to n
roll :: Int -> IO Int
roll n = getStdRandom $ randomR (0, n)

insertRandomly :: a -> [a] -> IO [a]
insertRandomly x xs = do
    i <- roll $ length xs
    pure $ take i xs ++ [x] ++ drop i xs

decodeHTMLChars :: T.Text -> T.Text
decodeHTMLChars = T.toStrict . T.toLazyText . htmlEncodedText

sendQuiz :: ChannelId -> Quiz -> ReaderT Auth IO ()
sendQuiz channelId q = do
    -- label all of the incorrect answers as "incorrect0", "incorrect1", etc.
    let
        incorrects =
            zip (quizIncorrectAnswers q) [0 ..]
                & map
                    (\(a, i) ->
                        mkSelectOption (decodeHTMLChars a)
                            $  "incorrect"
                            <> (T.pack . show) i
                    )
    -- the correct answer has the correct answer as its value as well, so we can
    -- show it later.
    let correct = mkSelectOption (quizCorrectAnswer q) (quizCorrectAnswer q)
    answers <- liftIO $ insertRandomly correct incorrects

    let decodedQ = decodeHTMLChars $ quizQuestion q
    void $ createMessageDetailed channelId $ def
        { messageDetailedContent    = "**Random Trivia: " <> weakOwoify decodedQ <> "**"
        , messageDetailedComponents = Just
            [ComponentActionRowSelectMenu $ mkSelectMenu "uwu_quiz" answers]
        }

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
    pure $ eitherDecode r
