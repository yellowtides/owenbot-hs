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
    ) where

import           Control.Applicative        ( (<|>) )
import           Control.Monad              ( void
                                            , guard
                                            )
import           Data.Attoparsec.Text
import qualified Data.Text as T

import           Discord.Types

-- | @manyTill1 p end@ is a parser that applies parser @p@ /one/ or more times
-- until parser @end@ succeeds. This is a variation on 'manyTill' from Parsec.
-- manyTill1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
-- manyTill1 p end = do { x <- p; xs <- manyTill p end; return (x:xs) }

-- | A @ParsableArgument@ is a dataclass that represents arguments that can be
-- parsed from a message. Any datatype that is an instance of this dataclass can
-- be used as function arguments for a command handler in 'command'.
class ParsableArgument a where
    -- | @parserForArg msg@ returns a parser that contains the parsed element.
    parserForArg :: Message -> Parser a

-- | The message that invoked the command.
instance ParsableArgument Message where
    parserForArg msg = pure msg

-- | Any number of non-space characters. If quoted, spaces are allowed.
-- Quotes in quoted phrases can be escaped with a backslash. The following is
-- parsed as a single string: 
-- @\"He said, \\\"Lovely\\\".\"@
instance ParsableArgument T.Text where
    parserForArg msg = do
        -- try quoted text first. if it failed, then normal word
        parsed <- quotedText <|> word
        -- consume end of input or one or more spaces
        (endOfInput <|> void (many1 space))
        pure parsed
      where
        quotedText = do
            -- consume quotes
            char '"'
            -- consume everything but quotes, unless it is escaped
            content <- many1 $ (string "\\\"" >> pure '"') <|> notChar '\"'
            -- consume closing quote
            char '"'
            pure $ T.pack content
        word =
            -- consume at least one character that is not a space or eof
            takeWhile1 (not . isHorizontalSpace)

-- | Zero or more texts. Each one could be quoted or not.
instance ParsableArgument [T.Text] where
    parserForArg msg =
        -- if it's the end, return empty (base case).
        (endOfInput >> pure []) <|> do
            -- do the usual text parsing (which consumes any trailing spaces)
            word <- parserForArg msg :: Parser T.Text
            -- recursively do this and append
            rest <- parserForArg msg :: Parser [T.Text]
            pure $ word:rest

-- | Datatype wrapper for the remaining text in the input. Handy for capturing
-- everything remaining. The accessor function @getEm@ isn't really meant to be
-- used since pattern matching can do everything. Open to renaming.
--
-- Example usage:
--
-- @
-- setStatus =
--     command "status"
--     $ \\msg newStatus newType (Remaining newName) -> do
--         ...
-- @
data RemainingText = Remaining { getEm :: T.Text }

-- | The rest of the arguments. Spaces and quotes are preserved as-is, unlike
-- with @Text@. At least one character is required.
instance ParsableArgument RemainingText where
    parserForArg msg = do
        -- Get the rest of the input, with at least one character.
        remaining <- takeWhile1 (const True)
        pure $ Remaining remaining

-- Integer. TODO
-- instance ParsableArgument Int where
--     parserForArg msg =

-- Float. TODO don't even know if we need this.
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
        (endOfInput <|> void (many1 space))
        pure parsed

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
        (endOfInput <|> void (many1 space))
        pure parsed
