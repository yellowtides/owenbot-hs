{-# LANGUAGE OverloadedStrings, FlexibleInstances, ExistentialQuantification, ScopedTypeVariables, MultiParamTypeClasses #-} -- allow arbitrary nested types in instance declarations
{-|
Description: Everything commands :)

Inspired heavily by the calamity-commands library (MIT).
Amateur attempt at abstraction.

Extensions used:

    * OverloadedStrings: Overloading of T.Text
    * FlexibleInstances: To allow arbitrary nested types in instance
    declarations
    * ExistentialQuantification: For explitit usage of forall.
    * ScopedTypeVariables: For using the same type variables in `where'
    statements as function declarations
    * MultiParamTypeClasses: For declaring CommandHandlerType that has 2 params
-}
module Command.Command
    ( -- * The fundamentals
    Command
    , command
    , runCommand
    -- ** Templates
    , defaultErrorHandler
    -- ** The MonadDiscord type
    , module Discord.Monad
    ) where

import           Control.Applicative        ( Alternative )
import           Control.Exception.Safe     ( catch
                                            , throwM
                                            , MonadCatch
                                            , MonadThrow
                                            , Exception
                                            , SomeException
                                            )
import           Control.Monad.IO.Class     ( MonadIO )
import           Control.Monad              ( void
                                            , guard
                                            )
import qualified Data.Text as T
import           Data.List                  ( intercalate )
import           Text.Parsec.Error          ( errorMessages
                                            , showErrorMessages
                                            )
import           Text.Parsec.Combinator
import qualified Text.Parsec.Text as T
import           Text.Parsec                ( runParser
                                            , eof
                                            , ParseError
                                            , space
                                            , anyChar
                                            , char
                                            , (<|>)
                                            )
import           UnliftIO                   ( liftIO )

import           Discord.Monad
import           Discord.Types
import           Discord

import           Command.Error              ( CommandError(..) )
import           Command.Parser             ( ParsableArgument(..)
                                            , manyTill1
                                            )
import           Owoifier                   ( owoify )


-- | A @Command@ is a datatype containing the metadata for a user-registered
-- command. 
--
-- @Command h m@ is a command that runs in the monad @m@, which when called
-- will trigger the polyvariadic handler function @h@. The handler @h@ *must*
-- be in the @m@ monad (this is enforced in 'command' using constraints)
--
-- The contents of this abstract datatype are not exported from this module for
-- encapsulation. Use 'command' to instantiate one.
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

-- | @command name handler@ creates a 'Command' named @name@, which upon
-- receiving a message will call @handler@. @name@ cannot contain any spaces,
-- as it breaks the parser. The @handler@ function can take an arbitrary amount
-- of arguments of arbitrary types (it is polyvariadic), as long as they are
-- instances of 'ParsableArgument'.
--
-- The Command that this function creates is polymorphic in the Monad it is run
-- in. This means you can call it from 'DiscordHandler' or 'IO', or any mock
-- monads in tests (as long as they are instances of 'MonadDiscord')
-- 
-- @
-- pong :: (MonadDiscord m) => Command (Message -> m ()) m
-- pong = command "ping" $ \\msg -> respond msg "pong!"
-- @
-- 
-- @
-- lePong :: (MonadDiscord m) => Message -> m ()
-- lePong = runCommand $
--     command "ping" $ \\msg -> respond msg "pong!"
-- @
--
-- @
-- weather :: (MonadDiscord m) => Command (Message -> T.Text -> m ()) m
-- weather = command "weather" $ \\msg location -> do
--     result <- liftIO $ getWeather location
--     respond msg $ "Weather at " <> location <> " is " <> result <> "!"
-- @
--
-- @
-- complex :: (MonadDiscord m) => Command (Message -> Int -> ConfigKey -> m ()) m
-- complex = command "complexExample" $ \\msg i key -> do
--     ...
-- @
--
-- The type signature of the return type can become quite long depending on how
-- many arguments @handler@ takes (as seen above). If it can be inferred (and it
-- usually can), it is safe to omit it. However, it may be kept for legibility. 
command
    :: (CommandHandlerType h m , MonadDiscord m)
    => T.Text
    -- ^ The name of the command.
    -> h
    -- ^ The handler for the command, that takes an arbitrary amount of
    -- 'ParsableArgument's and returns a @m ()@
    -> Command h m
command name handler = Command
    { commandName         = name
    , commandHandler      = handler
    , commandArgApplier   = applyArgs name
    , commandErrorHandler = defaultErrorHandler
    , commandHelp         = "Help not available."
    , commandRequires     = []
    }

-- | @runCommand command msg@ attempts to run the specified 'Command' with the
-- given message. It checks the command name, applies/parses the arguments, and
-- catches any errors.
--
-- @
-- runCommand pong :: (Message -> m ())
-- @
runCommand
    :: forall m h.
    (MonadDiscord m, Alternative m)
    => Command h m
    -- ^ The command to run against.
    -> Message
    -- ^ The message to run the command with.
    -> m ()
runCommand command msg =
    -- First check that the command name is right.
    case runParser (parseCommandName command) () "" (messageText msg) of
        Left e -> pure ()
        Right args -> do
            -- Apply the arguments one by one on the appropriate handler
            ((commandArgApplier command) msg args (commandHandler command))
                -- Asynchrnous errors are not caught as the `catch` comes from
                -- Control.Exception.Safe.
                `catch` basicErrorCatcher
                `catch` allErrorCatcher
  where
    -- | Catch CommandErrors and handle them with the handler
    basicErrorCatcher :: CommandError -> m ()
    basicErrorCatcher = (commandErrorHandler command) msg

    -- | Catch any and all errors, including ones thrown in basicErrorCatcher.
    allErrorCatcher :: SomeException -> m ()
    allErrorCatcher = ((commandErrorHandler command) msg) . HaskellError


-- | @defaultErrorHandler m e@ is the default error handler unless you override
-- it manually.
--
-- [On argument error] it responds with the errors.
-- [On requirement error] DMs the user with the errors.
-- [On a processing error] it responds with the error.
-- [On a Discord error] it likely won't be able to respond, so put to console.
-- [On Haskell error] this is just a runtime error, so respond with the error.
defaultErrorHandler
    :: (MonadDiscord m)
    => Message
    -- ^ The original message that led to this error.
    -> CommandError
    -- ^ The error thrown by the handler.
    -> m ()
defaultErrorHandler m e =
    case e of
        ArgumentParseError x ->
            respond m $ owoify $ T.pack $ showWithoutPos x
        RequirementError x -> do
            chan <- createDM (userId $ messageAuthor m)
            void $ createMessage (channelId chan) x
        ProcessingError x ->
            respond m $ owoify x
        DiscordError x ->
            liftIO $ putStrLn $ "Discord responded with a " <> (show x)
        HaskellError x ->
            respond m (owoify $ T.pack $ show x)
  where
    -- The default 'Show' instance for ParseError contains the error position,
    -- which only adds clutter in a Discord message.
    showWithoutPos :: ParseError -> String
    showWithoutPos err = showErrorMessages "or" "unknown parse error"
        "expecting" "unexpected" "end of input" (errorMessages err)


-- @CommandHandlerType@ is a dataclass for all types of arguments that a
-- command handler may have. Its only function is @applyArgs@.
class (MonadThrow m) => CommandHandlerType h m where
    applyArgs
        :: T.Text
        -- ^ Name of the command, to be used in error dialogues if any
        -- (unused as of now).
        -> Message
        -- ^ The relevant message.
        -> T.Text
        -- ^ The arguments of the command, i.e. everything after the command
        -- name followed by one or more spaces. Is "" if empty.
        -> h
        -- ^ The handler. It must be in the same monad as @m@.
        -> m ()
        -- ^ The monad to run the handler in, and to throw parse errors in.

-- | For the case when all arguments have been applied. Base case.
instance (MonadThrow m) => CommandHandlerType (m ()) m where
    applyArgs name msg input handler = 
        case runParser eof () "" input of
            Left e -> throwM $ ArgumentParseError e
            Right _ -> handler

-- | For the case where there is only one argument to apply.
-- Although this looks redundant in place of the (a -> b) instance, its presence
-- prevents GHC errors related to incoherent choices.
instance {-# OVERLAPPING #-} (MonadThrow m, ParsableArgument a) => CommandHandlerType (a -> m ()) m where
    applyArgs name msg input handler =
        case runParser (parserForArg msg) () "" input of
            Left e -> throwM $ ArgumentParseError e
            Right (x, remaining) -> applyArgs name msg remaining (handler x)

-- | For the case where there are multiple arguments to apply. 
instance (MonadThrow m, ParsableArgument a, CommandHandlerType b m) => CommandHandlerType (a -> b) m where
    applyArgs name msg input handler =
        case runParser (parserForArg msg) () "" input of
            Left e -> throwM $ ArgumentParseError e
            Right (x, remaining) -> applyArgs name msg remaining (handler x) 

-- | @parseCommandName@ creates a parser that tries to consume the prefix,
-- Command name, appropriate amount of spaces, and returns the arguments.
-- If there are no arguments, it will return the empty text, "".
parseCommandName :: Command h m -> T.Parser T.Text
parseCommandName cmd = do
    -- consume prefix
    char ':'
    -- consume at least 1 character until a space is encountered
    -- don't consume the space
    cmdName <- manyTill1 anyChar (void (lookAhead space) <|> eof)
    -- check it's the proper command
    guard (T.pack cmdName == commandName cmd)
    -- parse either an end of input, or spaces followed by arguments
    (eof >> pure "") <|> do
        -- consumes one or more isSpace characters
        many1 space
        -- consume everything until end of input
        args <- manyTill anyChar eof
        -- return the args
        pure (T.pack args)
