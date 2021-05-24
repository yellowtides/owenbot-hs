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
    , RemainingText(..)
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
-- until parser @end@ succeeds. This is a variation on 'manyTill' from Parsec.
manyTill1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
manyTill1 p end = do { x <- p; xs <- manyTill p end; return (x:xs) }

-- | A @ParsableArgument@ is a dataclass that can be parsed from a message. Any
-- datatype that is an instance of this dataclass can be used as function
-- arguments for a handler.
class ParsableArgument a where
    -- | @parserForArg msg@ returns a parser that contains the parsed element
    -- and the remaining input.
    parserForArg :: Message -> T.Parser (a, T.Text)

-- | The message that invoked the command.
instance ParsableArgument Message where
    parserForArg msg = do
        remaining <- getInput
        pure (msg, remaining)

-- | Wrapper for the 'String' ParsableArgument, as no one uses String nowadays.
instance ParsableArgument T.Text where
    parserForArg msg = do
        (result, remaining) <- parserForArg msg
        pure (T.pack result, remaining)

-- | Any number of non-space characters. If quoted, spaces are allowed.
-- Quotes in quoted phrases can be escaped with a backslash. The following is
-- parsed as a single string: 
-- @\"He said, \\\"Lovely\\\".\"@
--
-- This should _NOT_ be used, use 'T.Text'.
instance ParsableArgument String where
    parserForArg msg =
        (flip label) "word or a quoted phrase" $ do
            -- try quoted text first. if it failed, then normal word
            parsed <- quotedText <|> word
            -- consume end of input or one or more spaces
            (eof <|> void (many1 space))
            -- return remaining together with consumed value
            remaining <- getInput
            pure (parsed, remaining)
      where
        quotedText = do
            -- consume quotes
            char '"'
            -- consume everything but quotes, unless it is escaped
            content <- many $ try (string "\\\"" >> pure '"') <|> noneOf "\"" 
            -- consume closing quote
            char '"'
            pure content
        word =
            -- consume at least one character that is not a space or eof
            manyTill1 anyChar (void (lookAhead space) <|> eof)

-- | Zero or more texts. Each one could be quoted or not.
instance ParsableArgument [T.Text] where
    parserForArg msg =
        -- if it's the end, return empty (base case).
        (eof >> pure ([], "")) <|> do
            -- do the usual text parsing (which consumes any trailing spaces)
            (word, remaining) <- parserForArg msg :: T.Parser (T.Text, T.Text)
            -- recursively do this and append
            (rest, _) <- parserForArg msg :: T.Parser ([T.Text], T.Text)
            pure (word:rest, "")

data RemainingText = Remaining { getEm :: T.Text }

-- | The rest of the arguments. Spaces and quotes are preserved as-is, unlike
-- with @Text@. At least one character is required.
instance ParsableArgument RemainingText where
    parserForArg msg = do
        -- Make sure at least one character is present
        -- This is not a space, because previous parsers consume trailing spaces.
        firstChar <- anyChar
        -- Get the rest of the input.
        -- This is more convenient than calling [T.Text]'s parser and concatting
        -- it, as it preserves spaces. For speed, both T.concat and T.cons are
        -- O(n) so it does not matter.
        remaining <- getInput
        pure (Remaining $ T.cons firstChar remaining, "")

-- | Integer. TODO
-- instance ParsableArgument Int where
--     parserForArg msg =

-- | Float. TODO don't even know if we need this.
-- instance ParsableArgument Float where
--     parserForArg msg =


-- | Parses "online" "dnd" "idle" and "invisible" as 'UpdateStatusType's
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

-- | Parses "playing", "streaming", "listening to" and "competing in" as
-- 'ActivityType's.
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

