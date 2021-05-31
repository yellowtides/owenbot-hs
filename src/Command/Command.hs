{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances, FunctionalDependencies, UndecidableInstances #-}
{-|
Module      : Command.Command
License     : BSD (see the LICENSE file)
Description : Everything commands :)

Amateur attempt at command abstraction and polyvariadic magic.

Inspired heavily but calamity-commands, which is provided by Ben Simms 2020
under the MIT license. 

Ideally, this module wouldn't need to be touched after its initial creation
(and hence quite the jump in complex GHC extensions compared to other modules),
however it is documented quite extensively anyway.

Notable extensions used:

    * ScopedTypeVariables: For using the same type variables in `where'
    statements as function declarations.
    * FlexibleInstances: To allow complex type variables in instance declarations,
    like @CommandHandlerType m (a -> m ())@.
    [See more]
    (https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html#extension-FlexibleInstances)
    * FunctionalDependencies: To write that @m@ can be determined from @h@ in
    CommandHandlerType. It makes logical sense to tell GHC this because @h@ 
    must be in the @m@ monad (otherwise, @h@ may be in another monad).
    [See more]
    (https://en.wikibooks.org/wiki/Haskell/Advanced_type_classes)
    * MultiParamTypeClasses: For declaring CommandHandlerType that has 2 params.
    This comes with FunctionalDependencies automatically.
    * UndecidableInstances: Risky, but I think I logically checked over it. Used
    in the @m (a -> b)@ instance declaration of 'CommandHandlerType', because
    @(a -> b)@ doesn't explicitly determine the required functional dependency
    (@h -> m@). The extension is risky because the type-checker can fail to
    terminate if the instance declarations recursively reference each other. In
    this module however, all instances strictly converges to the @m (m ())@
    instance so I say it is safe. [See more]
    (https://www.reddit.com/r/haskell/comments/5zjwym/when_is_undecidableinstances_okay_to_use/)


Implementation references:

    * [Varargs - Haskell Wiki]
    (https://wiki.haskell.org/Varargs)
    * [Haskell-polyvariadic]
    (https://github.com/AJFarmar/haskell-polyvariadic)
    * [How to create a polyvariadic haskell function?]
    (https://stackoverflow.com/questions/3467279/how-to-create-a-polyvariadic-haskell-function)
    * [How to write a Haskell function that takes a variadic function as an argument]
    (https://stackoverflow.com/questions/8353845/how-to-write-a-haskell-function-that-takes-a-variadic-function-as-an-argument] 

Design references:

    * [calamity-commands](https://github.com/simmsb/calamity/tree/master/calamity-commands)
-}
module Command.Command
    ( -- * The fundamentals
    -- | These offer the core functionality for Commands. The most imporatnt
    -- functions are 'command' and 'runCommand'.
    -- 
    --     * 'command' creates a 'Command'
    --     * 'runCommand' converts the 'Command' to a normal receiver.
    --
    -- There are several functions that can be composed onto 'command', such as
    -- 'help' (overwrite the help message), 'onError' (overwrite the error 
    -- handler), or 'requires' (add a requirement for the command).
    --
    -- If your command demands a special syntax that is impossible with the
    -- existing 'command' function, use 'customCommand', where you can provide
    -- your own parser. It is less powerful however, and is intended to be used
    -- only for special special special occasions like "::quotes", dad jokes,
    -- or owoification.
    Command
    , command
    , runCommand
    , help
    , onError
    , defaultErrorHandler
    , requires
    , customCommand
    -- * Parsing arguments
    -- | The 'ParsableArgument' is the core dataclass for command arguments that
    -- can be parsed. Some special types are added to create functionality
    -- that is unrepresentable in existing Haskell datatypes.
    --
    -- As an OwenDev, if you want to make your own datatype parsable, all you
    -- have to do is to add an instance declaration for it (and a parser) inside
    -- @Command/Parser.hs@. Parsec offers very very very useful functions that
    -- you can simply compose together to create parsers.
    , ParsableArgument
    , RemainingText(..)
    -- * The MonadDiscord type
    -- | MonadDiscord is the underlying Monad class for all interactions to the
    -- Discord REST API. The abstraction makes for great polymorphic receivers
    -- that are easier to test and run from various contexts.
    -- 
    -- This is a common way to design abstraction so you can mock them. Same as
    -- Java interfaces, C++ virtual functions. If you want to read more:
    --
    --     * [Link 1]
    -- (https://www.reddit.com/r/haskell/comments/5bnr6b/mocking_in_haskell/)
    --     * [Link 2]
    -- (https://lexi-lambda.github.io/blog/2017/06/29/unit-testing-effectful-haskell-with-monad-mock/)
    , module Discord.Internal.Monad
    -- * MonadIO
    -- | Exported solely for convenience purposes, since many modules that use
    -- Commands require the MonadIO constraint, but it can get confusing which
    -- one to import. 
    , MonadIO(..)
    ) where

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
import           Data.Maybe                 ( catMaybes )
import qualified Data.Text as T
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
                                            , string
                                            , (<|>)
                                            )
import           UnliftIO                   ( liftIO )

import           Discord.Internal.Monad
import           Discord.Types
import           Discord

import           Command.Error              ( CommandError(..) )
import           Command.Parser             ( ParsableArgument(..)
                                            , RemainingText(..)
                                            , manyTill1
                                            )
import           Owoifier                   ( owoify )


-- | A @Command@ is a datatype containing the metadata for a user-registered
-- command. 
--
-- @Command m h@ is a command that runs in the monad @m@, which when called
-- will trigger the polyvariadic handler function @h@. The handler @h@ *must*
-- be in the @m@ monad, else your type-checker will complain.
--
-- The contents of this abstract datatype are not exported from this module for
-- encapsulation. Use 'command' to instantiate one.
data Command m h = Command
    { commandName         :: T.Text
    -- ^ The name of the command.
    , commandPrefix       :: String
    -- ^ The prefix for the command. Not Text because it is used in a parser,
    -- plus it's usually so short that there is no benefit to using Text here.
    , commandHandler      :: h
    -- ^ The polyvariadic handler function for the command. All of its argument
    -- types must be of 'ParsableArgument'. Its return type after applying all
    -- the arguments must be @m ()@. These are enforced by the type-checker.
    , commandArgApplier   :: Message -> T.Text -> h -> m ()
    -- ^ The function used to apply the arguments into the @commandHandler@. It
    -- needs to take a 'Message' that triggered the command, the input text in
    -- the message, the handler, and the return monad.
    , commandCustomParser :: Bool
    -- ^ Whether to use a custom parser. If true, then parseCommandName will not
    -- be used, and the custom arg applier will be called. If false, the command
    -- name will be verified before applyArgs is called.
    , commandErrorHandler :: Message -> CommandError -> m ()
    -- ^ The function called when a 'CommandError' is raised during the handling
    -- of a command.
    , commandHelp         :: T.Text
    -- ^ The help for this command.
    , commandRequires     :: [Message -> m (Maybe T.Text)]
    -- ^ A list of requirements that need to pass (Nothing) for the command to
    -- be processed. If any fails, the reason will be passed to
    -- commandErrorHandler.
    }


-- | @command name handler@ creates a 'Command' named @name@, which upon
-- receiving a message will call @handler@. The @name@ cannot contain any spaces,
-- as it breaks the parser. The @handler@ function can take an arbitrary amount
-- of arguments of arbitrary types (it is polyvariadic), as long as they are
-- instances of 'ParsableArgument'.
--
-- The Command that this function creates is polymorphic in the Monad it is run
-- in. This means you can call it from 'DiscordHandler' or 'IO', or any mock
-- monads in tests (as long as they are instances of 'MonadDiscord')
-- 
-- @pong@ below responds to a ping by a pong.
--
-- @
-- pong :: (MonadDiscord m) => Command m (Message -> m ())
-- pong = command "ping" $ \\msg -> respond msg "pong!"
-- @
-- 
-- @pong2@ shows that @help@ and @runCommand@ can be composed to create a normal
-- receiver.
--
-- @
-- pong2 :: (MonadDiscord m) => Message -> m ()
-- pong2
--     = runCommand
--     . help "Custom help message"
--     . command "ping" $ \\msg -> respond msg "pong!"
-- @
--
-- @weather@ below shows the type signature can be omitted, if it can be
-- inferred. Here, @location@ is likely inferred to be 'T.Text'.
--
-- @
-- weather = command "weather" $ \\msg location -> do
--     result <- liftIO $ getWeather location
--     respond msg $ "Weather at " <> location <> " is " <> result <> "!"
-- @
--
-- @complex@ shows that explicit type signature may help in understanding what
-- the various arguments are. It also shows that you can parse any type! (as
-- long as you create an instance declaration of 'ParsableArgument' for it)
-- You also don't need to have the Message at all if you won't use it.
--
-- @
-- instance ParsableArgument Int where
--     parserForArg msg = do
--         -- some parsec
--         parsed \<- read \<$> many digit
--         (eof <|> void (many1 space))
--         remaining <- getInput
--         pure (parsed, remaining)
--
-- complex :: (MonadDiscord m) => Command m (Int -> ConfigKey -> m ())
-- complex = command "complexExample" $ \\i key -> do
--     ...
-- @
--
-- The type signature of the return type can become quite long depending on how
-- many arguments @handler@ takes (as seen above). Thanks to the
-- @FunctionalDependencies@ and @UndecidableInstances@ extensions that are used
-- in the Commands library (you don't have to worry about them), they can be
-- inferred most of the time. It may be kept for legibility. As an OwenDev,
-- there are no extensions you need to enable in the receiver modules.
--
-- (yeah, they're wicked extensions that *could* cause harm, but in this case
-- (I hope) I've used them appropriately and as intended.)
command
    :: (CommandHandlerType m h , MonadDiscord m)
    => T.Text
    -- ^ The name of the command. This is ignored if 'customParser' is used.
    -> h
    -- ^ The handler for the command, that takes an arbitrary amount of
    -- 'ParsableArgument's and returns a @m ()@
    -> Command m h
command name handler = Command
    { commandName         = name
    , commandPrefix       = ":"
    , commandHandler      = handler
    , commandArgApplier   = applyArgs
    , commandCustomParser = False
    , commandErrorHandler = defaultErrorHandler
    , commandHelp         = "Help not available."
    , commandRequires     = []
    }

-- | @onError@ overwrites the default error handler of a command with a custom
-- implementation.
--
-- @
-- example
--     = onError (\\msg e -> respond msg (T.pack $ show e))
--     . command "example" $ do
--         ...
-- @
onError
    :: (Message -> CommandError -> m ())
    -- ^ Error handler that takes the original message that caused the error,
    -- and the error itself. It runs in the same monad as the command handlers.
    -> Command m h
    -> Command m h
onError errorHandler cmd = cmd
    { commandErrorHandler = errorHandler
    }

-- | @prefix@ overwrites the default command prefix ":" of a command with a
-- custom one. This is ignored if 'customParser' is used.
--
-- @
-- example
--     = prefix "->"
--     . command "idk" $ do
--         ...
-- @
prefix
    :: String
    -> Command m h
    -> Command m h
prefix newPrefix cmd = cmd
    { commandPrefix = newPrefix
    }

-- | @requires@ adds a requirement to the command. The requirement is a function
-- that takes a 'Message' and either returns @'Nothing'@ (no problem) or
-- @'Just' "explanation"@. The function is in the @m@ monad so it can access
-- any additional information from Discord as necessary.
--
-- Commands default to having no requirements.
requires :: (Message -> m (Maybe T.Text)) -> Command m h -> Command m h
requires requirement cmd = cmd
    { commandRequires = requirement : commandRequires cmd
    }

-- | @help@ sets the help message for the command. The default is "Help not
-- available."
help :: T.Text -> Command m h -> Command m h
help newHelp cmd = cmd
    { commandHelp = newHelp
    }

-- | @customCommand@ defines a command that has no name, and has a custom
-- parser. It exists to make things like dad jokes and owoification easier, as
-- they aren't "commands" but can benefit from having a error handling and
-- parsers. It can also help things like "::quotes" because they have special
-- syntax that demands a special parser.
--
-- 'prefix', if used together, is ignored. Other compoasble functions like
-- 'help', 'requires', and 'onError' are still valid.
--
-- The handler __must__ take a 'Message' and a 'String' as argument (nothing
-- more, nothing less), where the latter is the result of the parser.
--
-- @
-- example
--     = requires moderatorPrivs
--     . customCommand (string '::' >> many1 anyChar) $ \msg quoteName -> do
--         ...
--         -- this is triggered on "::<one or more chars>" where quoteName
--         -- contains the section enclosed in <>
-- @
customCommand
    :: (MonadDiscord m)
    => T.Parser String
    -- ^ The custom parser for the command. It has to return a 'String'.
    -> (Message -> String -> m ())
    -- ^ The handler for the command.
    -> Command m (Message -> String -> m ())
customCommand parserFunc handler = Command
    { commandName         = "<custom parser>"
    , commandPrefix       = "?"
    , commandHandler      = handler
    , commandArgApplier   = applyCustomParser parserFunc
    , commandCustomParser = True
    , commandErrorHandler = defaultErrorHandler
    , commandHelp         = "Help not available."
    , commandRequires     = []
    }

-- | @runCommand command msg@ attempts to run the specified 'Command' with the
-- given message. It checks for any requirement failures, then calls the
-- @cmmandArgApplier@ function in the 'Command' ADT. Any errors inside the
-- handler is caught and appropriately handled.
--
-- If a custom parser is defined, the requirements are checked and parser is called.
-- If a custom parser is not defined, the command name is first matched and
-- confirmed, before doing the requirement check and parser calls.
--
-- @
-- runCommand pong :: Message -> m ()
-- @
runCommand
    :: forall m h.
    (MonadDiscord m)
    => Command m h
    -- ^ The command to run against.
    -> Message
    -- ^ The message to run the command with.
    -> m ()
runCommand command msg = case commandCustomParser command of
    True -> doChecksAndRunCommand ""
    False -> do
        -- First check that the command name is correct, and extract arguments.
        case runParser (parseCommandName command) () "" (messageText msg) of
            Left e -> pure ()
            Right args -> doChecksAndRunCommand args

  where
    doChecksAndRunCommand :: T.Text -> m ()
    doChecksAndRunCommand args = do
        -- Check for requirements. checks will be a list of Maybes
        checks <- sequence $ map ($ msg) (commandRequires command)
        -- only get the Justs
        let failedChecks = catMaybes checks
        if null failedChecks
            then do
                let handler = commandHandler command
                -- Apply the arguments one by one on the appropriate handler
                ((commandArgApplier command) msg args handler)
                    -- Asynchrnous errors are not caught as the `catch`
                    -- comes from Control.Exception.Safe. This is good.
                    `catch` basicErrorCatcher
                    `catch` allErrorCatcher
            else
                -- give the first requirement error to the error handler
                basicErrorCatcher (RequirementError $ head failedChecks)

    -- | Catch CommandErrors and handle them with the handler
    basicErrorCatcher :: CommandError -> m ()
    basicErrorCatcher = (commandErrorHandler command) msg

    -- | Catch any and all errors, including ones thrown in basicErrorCatcher.
    allErrorCatcher :: SomeException -> m ()
    allErrorCatcher = ((commandErrorHandler command) msg) . HaskellError






-- | @defaultErrorHandler m e@ is the default error handler unless you override
-- it manually. This is exported and documented for reference only.
--
-- [On argument error] It calls 'respond' with the errors, owoified.
-- [On requirement error] It sends a DM to the invoking user with the errors.
-- [On a processing error] It calls 'respond' with the error.
-- [On a Discord error] If a Discord request failed, it's likely that the bot
-- would not be able to respond, so the error is put to stdout.
-- [On a Runtime/Haskell error] It calls 'respond' with the error, owoified.
defaultErrorHandler
    :: (MonadDiscord m)
    => Message
    -> CommandError
    -> m ()
defaultErrorHandler m e =
    case e of
        ArgumentParseError x ->
            respond m $ owoify $ T.pack $ showWithoutPos x
        RequirementError x -> do
            chan <- createDM (userId $ messageAuthor m)
            void $ createMessage (channelId chan) x
        ProcessingError x ->
            respond m x
        DiscordError x ->
            respond m $ T.pack $ "Discord responded with a " <> (show x)
        HaskellError x ->
            respond m (owoify $ T.pack $ show x)
  where
    -- The default 'Show' instance for ParseError contains the error position,
    -- which only adds clutter in a Discord message. This defines a much
    -- simpler string representation.
    showWithoutPos :: ParseError -> String
    showWithoutPos err = showErrorMessages "or" "unknown parse error"
        "expecting" "unexpected" "end of input" (errorMessages err)








-- @CommandHandlerType@ is a dataclass for all types of arguments that a
-- command handler may have. Its only function is @applyArgs@.
class (MonadThrow m) => CommandHandlerType m h | h -> m where
    applyArgs
        :: Message
        -- ^ The relevant message.
        -> T.Text
        -- ^ The arguments of the command, i.e. everything after the command
        -- name followed by one or more spaces. Is "" if empty.
        -> h
        -- ^ The handler. It must be in the same monad as @m@.
        -> m ()
        -- ^ The monad to run the handler in, and to throw parse errors in.

-- | For the case when all arguments have been applied. Base case.
instance (MonadThrow m) => CommandHandlerType m (m ()) where
    applyArgs msg input handler = 
        case runParser eof () "" input of
            Left e -> throwM $ ArgumentParseError e
            Right _ -> handler

-- | For the case where there are multiple arguments to apply. 
instance (MonadThrow m, ParsableArgument a, CommandHandlerType m b) => CommandHandlerType m (a -> b) where
    applyArgs msg input handler =
        case runParser (parserForArg msg) () "" input of
            Left e -> throwM $ ArgumentParseError e
            Right (x, remaining) -> applyArgs msg remaining (handler x) 

-- | For the case where there is only one argument to apply.
-- It overlaps the previous instance (it is more specific). With the OVERLAPPING
-- pragma, GHC prefers this one when both match.
--
-- This instance is necessary because otherwise @Message -> m ()@ will match
-- both @m ()@ (@(->) r@ is a monad) and @a -> b@ and since neither are more
-- specific, GHC cannot prefer one over the other, even with any pragmas.
instance {-# OVERLAPPING #-} (MonadThrow m, ParsableArgument a) => CommandHandlerType m (a -> m ()) where
    applyArgs msg input handler =
        case runParser (parserForArg msg) () "" input of
            Left e -> throwM $ ArgumentParseError e
            Right (x, remaining) -> applyArgs msg remaining (handler x)


-- | @applyCustomParser@ is in a similar fashion to @applyArgs@. In fact, if
-- you apply the first Parser argument, it is completely identical. In fact x2,
-- some arguments are not even used, just to make it completely isomorphic.
--
-- This is used when there is a custom parser that's defined. It only has one
-- possible instance (@Message -> String -> m ()@), so no special class is
-- defined like with CommandHandlerType (that is polyvariadic).
applyCustomParser
    :: (Monad m)
    => T.Parser String 
    -> Message
    -> T.Text
    -> (Message -> String -> m ())
    -> m ()
applyCustomParser parser msg name handler =
    case runParser parser () "" (messageText msg) of
        Left e -> pure ()
        Right result -> handler msg result



-- | @parseCommandName@ returns a parser that tries to consume the prefix,
-- Command name, appropriate amount of spaces, and returns the arguments.
-- If there are no arguments, it will return the empty text, "".
parseCommandName :: Command m h -> T.Parser T.Text
parseCommandName cmd = do
    -- consume prefix
    string (commandPrefix cmd)
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
