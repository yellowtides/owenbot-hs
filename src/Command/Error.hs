{-|
Module      : Command.Error
License     : BSD (see the LICENSE file)
Description : Error types for commands.

Provides the errors for Commands.
Its usage is mainly internal, to be used from "Command.Command".

-}
module Command.Error (CommandError(..)) where

import Control.Exception.Safe (Exception, SomeException)
import qualified Data.Text as T
import Text.Parsec.Error (ParseError)

import Discord (RestCallErrorCode)

-- | This represents any error that can arise from an invocation of a command.
-- Some are thrown by the system (such as ArgumentParseError), however you can 
-- also manually throw them with 'throwM' within any handler.
data CommandError
    = ArgumentParseError T.Text
    -- ^ Indicates the command arguments failed to parse, either because of lack
    -- of arguments, incorrect types, or too many arguments.
    | RequirementError T.Text
    -- ^ Indicates that a requirement for this command failed to pass. The
    -- reason is specified in the text field.
    | ProcessingError T.Text
    -- ^ This is a value unused by the system, and is free to be used by the
    -- handler as they wish.
    | DiscordError RestCallErrorCode
    -- ^ Indicates there was a fatal Discord call somewhere in the handler.
    | HaskellError SomeException
    -- ^ Indicates there was some sort of runtime error. This contains all
    -- sorts of errors, however they are guaranteed to be safe synchronous
    -- exceptions.
    deriving Show

instance Exception CommandError
