{-|
Module      : Command.Type
License     : BSD (see the LICENSE file)
Description : Fundamental datatype for Command.

Provides the fundamental type for Command.
Its usage is mainly internal. This is separated into a different module so that
it can be used from various submodules within "Command".
-}
module Command.Type
    ( Command(..)
    ) where

import qualified Data.Text as T
import           Discord.Types

import           Command.Error

-- | A @Command@ is a datatype containing the metadata for a user-registered
-- command. 
--
-- @Command h m@ is a command that runs in the monad @m@, which when called
-- will trigger the polyvariadic handler function @h@. The handler @h@ *must*
-- be in the @m@ monad (this is enforced in 'command' using constraints)
data Command h m = Command
    { commandName         :: T.Text
    -- ^ The name of the command.
    , commandHandler      :: h
    -- ^ The polyvariadic handler function for the command. All of its argument
    -- types must be of 'ParsableArgument'. Its return type after applying all
    -- the arguments must be @m ()@. These are enforced by the type-checker if
    -- you use 'command' instead of manually constructing this datatype.
    , commandArgApplier   :: Message -> T.Text -> h -> m ()
    -- ^ The function used to apply the arguments into the @commandHandler@. It
    -- needs to take a 'Message' that triggered the command, the name of the
    -- command, the handler, and the return monad.
    , commandErrorHandler :: Message -> CommandError -> m ()
    -- ^ The function called when a 'CommandError' is raised during the handling
    -- of a command.
    , commandHelp         :: T.Text
    -- ^ The help for this command.
    , commandRequires     :: [Message -> Either T.Text ()]
    -- ^ A list of requirements that need to pass ('Right') for the command to
    -- be processed.
    }
