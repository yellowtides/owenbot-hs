{-# LANGUAGE OverloadedStrings, FlexibleInstances, ExistentialQuantification, ScopedTypeVariables, MultiParamTypeClasses #-} -- allow arbitrary nested types in instance declarations
{-
Inspired heavily by the calamity-commands library (MIT).
Amateur attempt at abstraction.

Extensions used:
- OverloadedStrings: Overloading of T.Text
- FlexibleInstances: To allow arbitrary nested types in instance declarations
- ExistentialQuantification: For explitit usage of forall.
- ScopedTypeVariables: For using the same type variables in `where' statements as function declarations
- MultiParamTypeClasses: For declaring EinmyriaHandlerType that has 2 params
-}
module Einmyria.Commands
    ( command
    , runCommand
    , Einmyria
    , MonadDiscord(..)
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
import           Control.Monad              ( void )
import qualified Data.Text as T
import           Data.List                  ( intercalate )
import           Text.Parsec.Error          ( errorMessages
                                            , showErrorMessages
                                            )
import           Text.Parsec                ( runParser
                                            , eof
                                            , ParseError
                                            )
import           UnliftIO                   ( liftIO )

import           Discord.Monad
import           Discord.Types
import           Discord

import           Einmyria.Error             ( EinmyriaError(..) )
import           Einmyria.Parser            ( ParsableArgument(..)
                                            , parseEinmyriaName
                                            )
import           Einmyria.Type
import           Owoifier                   ( owoify )


-- | Create a command from the name and handler.
command
    :: (EinmyriaHandlerType h m , MonadDiscord m, MonadThrow m)
    => T.Text
    -> h
    -> Einmyria h m
command name handler = Einmyria
    { einmyriaName         = name
    , einmyriaHandler      = handler
    , einmyriaArgApplier   = applyArgs name
    , einmyriaErrorHandler = defaultErrorHandler
    , einmyriaHelp         = "Help not available."
    , einmyriaRequires     = []
    }

-- | @runCommand einmyria msg@ attempts to run the specified Einmyria with the
-- message. It checks the command name, applies/parses the arguments, and
-- catches any errors.
runCommand
    :: forall m h.
    (MonadDiscord m, Alternative m, MonadCatch m, MonadThrow m, MonadIO m)
    => Einmyria h m
    -- ^ The command to run against.
    -> Message
    -- ^ The message to run the command with.
    -> m ()
runCommand einmyria msg =
    -- First check that the command name is right.
    case runParser (parseEinmyriaName einmyria) () "" (messageText msg) of
        Left e -> pure ()
        Right args -> do
            -- Apply the arguments one by one on the appropriate handler
            ((einmyriaArgApplier einmyria) msg args (einmyriaHandler einmyria))
                -- Asynchrnous errors are not caught as the `catch` comes from
                -- Control.Exception.Safe.
                `catch` basicErrorCatcher
                `catch` allErrorCatcher
  where
    -- | Catch EinmyriaErrors and handle them with the handler
    basicErrorCatcher :: EinmyriaError -> m ()
    basicErrorCatcher = (einmyriaErrorHandler einmyria) msg

    -- | Catch any and all errors, including ones thrown in basicErrorCatcher.
    allErrorCatcher :: SomeException -> m ()
    allErrorCatcher = ((einmyriaErrorHandler einmyria) msg) . HaskellError


-- | @defaultErrorHandler m e@ is the default error handler unless you override
-- it manually.
--
-- On argument error: it responds with the errors.
-- On requirement error: DMs the user with the errors.
-- On a processing error: it responds with the error.
-- On a Discord error: it likely won't be able to respond, so put to console.
-- On Haskell error: this is just a runtime error, so respond with the error.
defaultErrorHandler
    :: (MonadThrow m, MonadDiscord m, MonadIO m)
    => Message
    -- ^ The original message that led to this error.
    -> EinmyriaError
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


-- @EinmyriaHandlerType@ is a dataclass for all types of arguments that a
-- command handler may have. Its only function is @applyArgs@.
class (MonadThrow m) => EinmyriaHandlerType h m where
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
instance (MonadThrow m) => EinmyriaHandlerType (m ()) m where
    applyArgs name msg input handler = 
        case runParser eof () "" input of
            Left e -> throwM $ ArgumentParseError e
            Right _ -> handler

-- | For the case where there is only one argument to apply.
-- Although this looks redundant in place of the (a -> b) instance, its presence
-- prevents GHC errors related to incoherent choices.
instance {-# OVERLAPPING #-} (MonadThrow m, ParsableArgument a) => EinmyriaHandlerType (a -> m ()) m where
    applyArgs name msg input handler =
        case runParser (parserForArg msg) () "" input of
            Left e -> throwM $ ArgumentParseError e
            Right (x, remaining) -> applyArgs name msg remaining (handler x)

-- | For the case where there are multiple arguments to apply. 
instance (MonadThrow m, ParsableArgument a, EinmyriaHandlerType b m) => EinmyriaHandlerType (a -> b) m where
    applyArgs name msg input handler =
        case runParser (parserForArg msg) () "" input of
            Left e -> throwM $ ArgumentParseError e
            Right (x, remaining) -> applyArgs name msg remaining (handler x) 


