{-# LANGUAGE FlexibleInstances #-} -- allow arbitrary nested types in instance declarations
{-# LANGUAGE OverloadedStrings #-} -- for T.Text overloading
{-|
Module      : Command.Parser
License     : BSD (see the LICENSE file)
Description : Parsers for commands.

Parser component for Commands.
Its usage is mainly internal, to be used from "Command.Command".

If you want to add your own parser argument type, you can either add it here
(to be reused across multiple modules), or in your receive module (if it is only
used there). Either works really, but sometimes you need the other to prevent
looping imports.

-}
module Command.Parser
    ( ParsableArgument(..)
    , RemainingText(..)
    , manyTill1
    , endOrSpaces
    ) where

import Control.Monad (void)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Combinator
import qualified Text.Parsec.Text as T

import Discord.Types

-- | @manyTill1 p end@ is a parser that applies parser @p@ /one/ or more times
-- until parser @end@ succeeds. This is a variation on 'manyTill' from Parsec.
manyTill1
    :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
manyTill1 p end = do
    x  <- p
    xs <- manyTill p end
    return (x : xs)

-- | @eofOrSpaces@ is a parser that parses an end of command, or at least one
-- space and skips the result.
endOrSpaces :: T.Parser ()
endOrSpaces = eof <|> skipMany1 space <?> "at least one space between arguments"

-- | A @ParsableArgument@ is a dataclass that represents arguments that can be
-- parsed from a message text. Any datatype that is an instance of this dataclass
-- can be used as function arguments for a command handler in 'command'.
class ParsableArgument a where
    -- | @parserForArg@ is a parser that returns the parsed element.
    parserForArg :: T.Parser a

----------------------------------------------------------------------------
    -- Ubiquitous types not related to Discord
----------------------------------------------------------------------------

-- | Any number of non-space characters. If quoted, spaces are allowed.
-- Quotes in quoted phrases can be escaped with a backslash. The following is
-- parsed as a single string:
-- @\"He said, \\\"Lovely\\\".\"@
instance ParsableArgument String where
    parserForArg = do
        -- try quoted text first. if it failed, then normal word
        (quotedText <?> "a quoted phrase") <|> (word <?> "a word")
      where
        quotedText = try $ do -- backtrack if failed, parse as normal word
            -- consume opening quote
            char '"'
            -- consume everything but quotes, unless it is escaped
            content <- many1 $ try (string "\\\"" >> pure '"') <|> noneOf "\""
            -- consume closing quote
            char '"'
            pure content
        word =
            -- consume at least one character that is not a space or eof
            manyTill1 anyChar (void (lookAhead space) <|> eof)

-- | Wrapper for the String version, since Text is the trend nowadays.
-- Both are provided so that it can easily be used for arguments in other
-- functions that only accept one of the types.
instance ParsableArgument T.Text where
    parserForArg = T.pack <$> parserForArg

-- | Zero or more texts. Each one could be quoted or not.
instance ParsableArgument [T.Text] where
    parserForArg =
        -- if it's the end, return empty (base case).
                   (eof >> pure []) <|> do
            -- do the usual text parsing
        word <- parserForArg :: T.Parser T.Text
        endOrSpaces
        -- recursively do this and append
        rest <- parserForArg :: T.Parser [T.Text]
        pure $ word : rest

-- Integer.
instance ParsableArgument Int where
    parserForArg = read <$> (many1 digit <?> "a number")

-- Float. TODO don't even know if we need this.
-- instance ParsableArgument Float where
--     parserForArg msg =

-- | Datatype wrapper for the remaining text in the input. Handy for capturing
-- everything remaining. The accessor function @getDeez@ isn't really meant to be
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
newtype RemainingText = Remaining { getDeez :: T.Text }

-- | The rest of the arguments. It may be quoted in its entirety. Spaces are
-- preserved as-is, unlike concatting after parsing [@Text@].
-- At least one character is required within the quotes, or on its own.
instance ParsableArgument RemainingText where
    parserForArg =
        -- try quoted text first. if it failed, then return input
        Remaining <$> ((quotedText <?> "some quoted") <|> (normal <?> "unquoted text"))
      where
        quotedText = try $ do -- backtrack if failed
            char '"'
            -- consume everything but quotes, unless it is escaped
            content <- many1 $ try (string "\\\"" >> pure '"') <|> noneOf "\""
            char '"'
            -- ensure it is at eof
            -- this means `"ababababa" cdcdcd` is treated as a normal string
            -- without eof, it will throw an error about an unexpected cdcdcd
            eof
            pure $ T.pack content
        normal = do
            -- First char is guaranteed to not be a space, because previous parsers
            -- consume trailing spaces.
            firstChar <- anyChar
            -- getInput is more convenient than doing "many anyChar" because it doesn't
            -- need to parse anything for the remaining input.
            remaining <- getInput
            setInput ""
            pure $ T.cons firstChar remaining

-- | An argument that can or cannot exist.
instance (ParsableArgument a) => ParsableArgument (Maybe a) where
    parserForArg =
        try (Just <$> parserForArg)
            <|> (do
            -- artificially put a space so it won't complain about missing
            -- spaces between arguments.
                    remaining <- getInput
                    setInput $ " " <> remaining
                    pure Nothing
                )

-- | An argument that always has to be followed by another.
instance (ParsableArgument a, ParsableArgument b) => ParsableArgument (a, b) where
    parserForArg = (,) <$> (parserForArg <* endOrSpaces) <*> parserForArg





-----------------------------------------------------------------------------
    -- Discord specific types
-----------------------------------------------------------------------------


instance ParsableArgument Snowflake where
    parserForArg = read <$> (many1 digit <?> "a snowflake ID")

-- | Parses "online" "dnd" "idle" and "invisible" as 'UpdateStatusType's
instance ParsableArgument UpdateStatusType where
    parserForArg =
        -- consume either of the following:
        -- (if fail then backtrack using try)
                   choice $ map
        try
        [ string "online" >> pure UpdateStatusOnline
        , string "dnd" >> pure UpdateStatusDoNotDisturb
        , string "idle" >> pure UpdateStatusAwayFromKeyboard
        , string "invisible" >> pure UpdateStatusInvisibleOffline
        ]

-- | Parses "playing", "streaming", "listening to" and "competing in" as
-- 'ActivityType's.
instance ParsableArgument ActivityType where
    parserForArg =
        -- consume either of the following:
        -- (if fail then backtrack using try)
                   choice $ map
        try
        [ string "playing" >> pure ActivityTypeGame
        , string "streaming" >> pure ActivityTypeStreaming
        , string "listening to" >> pure ActivityTypeListening
        , string "competing in" >> pure ActivityTypeCompeting
        ]

