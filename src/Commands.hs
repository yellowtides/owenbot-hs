{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands ( command ) where

import           Control.Exception      ( throw
                                        )
import qualified Data.Text as T
import           Discord.Types
import           Discord
import           Text.Parsec.Text
import           Text.Parsec hiding     ( GenParser )
import           Text.Parsec.Combinator

command
    :: (CommandHandlerType h)
    => T.Text
    -> (Message -> h)
    -> Message
    -> DiscordHandler ()
command name handler msg = callCommand name (handler msg) msg

class CommandHandlerType h where
    callCommand :: T.Text -> h -> Message -> DiscordHandler ()

instance CommandHandlerType (DiscordHandler ()) where
    callCommand name handler msg = handler

instance (ParsableArgument a, CommandHandlerType b) => CommandHandlerType (a -> b) where
    callCommand name handler msg =
        case runParser parserForArg () "" (messageText msg) of
            Left e -> throw "incorrect arguments"
            Right result -> callCommand name (handler result) msg

class ParsableArgument a where
    parserForArg :: GenParser st a

instance ParsableArgument T.Text where
    parserForArg = (T.pack <$>) $ many $ noneOf " "
  -- a parser that takes text and returns text
