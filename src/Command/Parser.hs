{-# LANGUAGE FlexibleInstances #-} -- allow arbitrary nested types in instance declarations
{-# LANGUAGE OverloadedStrings #-} -- for T.Text overloading
{-|
Module      : Command.Parser
License     : BSD (see the LICENSE file)
Description : Parsers for commands.

Parser component for Commands.
Its usage is mainly internal, to be used from "Command.Command".

If you want to add your own parser argument type, this is the module.

-}
module Command.Parser
    ( ParsableArgument(..)
    , manyTill1
    ) where

import           Control.Monad              ( void
                                            , guard
                                            )
import qualified Data.Text as T
import           Text.Parsec.Combinator
import qualified Text.Parsec.Text as T
import           Text.Parsec

import           Discord.Types

-- | @manyTill1 p end@ is a parser that applies parser @p@ /one/ or more times
-- until parser @end@ succeeds. This is a variation on @manyTill@ from Parsec.
manyTill1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
manyTill1 p end = do { x <- p; xs <- manyTill p end; return (x:xs) }

-- | A @ParsableArgument@ is a dataclass that can be parsed from a message. Any
-- datatype that is an instance of this dataclass can be used as function
-- arguments for a handler.
class ParsableArgument a where
    -- | @parserForArg msg@ returns a parser that contains the parsed element
    -- and the remaining input.
    parserForArg :: Message -> T.Parser (a, T.Text)

instance ParsableArgument Message where
    parserForArg msg = do
        remaining <- getInput
        pure (msg, remaining)

instance ParsableArgument T.Text where
    parserForArg msg =
        (flip label) "any text" $ do
            -- consume at least one character that is not a space or eof
            parsed <- manyTill1 anyChar (void (lookAhead space) <|> eof)
            -- consume end of input or one or more spaces
            (eof <|> void (many1 space))
            -- return remaining together with consume value
            remaining <- getInput
            pure (T.pack parsed, remaining)

instance ParsableArgument [T.Text] where
    parserForArg msg = do
        -- consume at least one of any character, separated by spaces
        rest <- (many1 anyChar) `sepBy` space
        remaining <- getInput
        pure (T.pack <$> rest, remaining)

instance ParsableArgument UpdateStatusType where
    parserForArg msg = do
        -- consume either of the following:
        parsed <- choice
            [ string "online" >> pure UpdateStatusOnline
            , string "dnd" >> pure UpdateStatusDoNotDisturb
            , string "idle" >> pure UpdateStatusAwayFromKeyboard
            , string "invisible" >> pure UpdateStatusInvisibleOffline
            ]
        -- consume end of input or at least one space.
        (eof <|> void (many1 space))
        remaining <- getInput
        pure (parsed, remaining)

instance ParsableArgument ActivityType where
    parserForArg msg = do
        -- consume either of the following:
        parsed <- choice
            [ string "playing" >> pure ActivityTypeGame
            , string "streaming" >> pure ActivityTypeStreaming
            , string "listening to" >> pure ActivityTypeListening
            , string "competing in" >> pure ActivityTypeCompeting
            ]
        -- consume end of input or at least one space.
        (eof <|> void (many1 space))
        remaining <- getInput
        pure (parsed, remaining)

