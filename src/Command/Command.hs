{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE OverloadedStrings #-}
-- for CommandHandlerType
{-# LANGUAGE FlexibleInstances, FunctionalDependencies, UndecidableInstances #-}
-- for Command
{-# LANGUAGE NamedFieldPuns #-}
-- for runCommand
{-# LANGUAGE ScopedTypeVariables #-}
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

__As an OwenDev, you do not need to enable any GHC extensions, as the__
__extensions are used internally within this module only__.

==== __Notable extensions used (if you want to know)__

    * ScopedTypeVariables: For using the same type variables in @where@
    statements as function declarations.
    * FlexibleInstances: To allow complex type variables in instance declarations,
    like @CommandHandlerType m (a -> m ())@.
    [Read more]
    (https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html#extension-FlexibleInstances)
    * FunctionalDependencies: To write that @m@ can be determined from @h@ in
    @CommandHandlerType@. It makes logical sense to tell GHC this because @h@ 
    must be in the @m@ monad (otherwise, @h@ may be in another monad).
    [Read more]
    (https://en.wikibooks.org/wiki/Haskell/Advanced_type_classes)
    * MultiParamTypeClasses: For declaring CommandHandlerType that has 2 params.
    This comes with FunctionalDependencies automatically, and is not explicitly
    used.
    * UndecidableInstances: Risky, but I think I logically checked over it. Used
    in the @m (a -> b)@ instance declaration of 'CommandHandlerType', because
    @(a -> b)@ doesn't explicitly determine the required functional dependency
    (@h -> m@). The extension is risky because the type-checker can fail to
    terminate if the instance declarations recursively reference each other. In
    this module however, all instances strictly converges to the @m (m ())@
    instance so I say it is safe. [Read more]
    (https://www.reddit.com/r/haskell/comments/5zjwym/when_is_undecidableinstances_okay_to_use/)
    * NamedFieldPuns: Shorten pattern matching ADT field names.
    
==== __Implementation references__

    * [Varargs - Haskell Wiki]
    (https://wiki.haskell.org/Varargs)
    * [Haskell-polyvariadic]
    (https://github.com/AJFarmar/haskell-polyvariadic)
    * [How to create a polyvariadic haskell function?]
    (https://stackoverflow.com/questions/3467279/how-to-create-a-polyvariadic-haskell-function)
    * [How to write a Haskell function that takes a variadic function as an argument]
    (https://stackoverflow.com/questions/8353845/how-to-write-a-haskell-function-that-takes-a-variadic-function-as-an-argument)

==== __Design references__

    * [calamity-commands](https://github.com/simmsb/calamity/tree/master/calamity-commands)
-}
module Command.Command
    (
    -- * Troubleshooting
    -- | See the [Common Errors](#commonerrors) section.

    -- * The fundamentals
    -- | These offer the core functionality for Commands. The most imporatnt
    -- functions are 'command' and 'runCommand'.
    -- 
    --     * 'command' creates a 'Command'
    --     * 'runCommand' converts the 'Command' to a normal receiver.
    --
    -- If your command demands a special syntax that is impossible with the
    -- existing 'command' function, use 'parsecCommand' (Parsec) or 'regexCommand'
    -- (Regex).
    Command
    , command
    , runCommand
    , runCommands
    , runHelp
    -- ** Custom command parsing
    -- | Parsec and Regex options are available.
    , parsecCommand
    , regexCommand
    -- ** Compsable Functions
    -- | These can be composed onto 'command' to overwrite default functionality.
    , help
    , alias
    , onError
    , defaultErrorHandler
    , requires
    -- ** Errors
    , CommandError(..)
    -- ** Parsing arguments
    -- | The 'ParsableArgument' is the core dataclass for command arguments that
    -- can be parsed.
    --
    -- As an OwenDev, if you want to make your own datatype parsable, all you
    -- have to do is to add an instance declaration for it (and a parser) inside
    -- @Command/Parser.hs@. Parsec offers very very very useful functions that
    -- you can simply compose together to create parsers.
    , ParsableArgument
    , RemainingText(..)
    -- * Exported classes
    -- | Here are the monad dataclasses exported from this module.

    -- ** The MonadDiscord class
    -- | MonadDiscord is the underlying Monad class for all interactions to the
    -- Discord REST API. 
    -- 
    -- This is a common way to design abstraction so you can mock them. Same as
    -- Java interfaces, C++ virtual functions. If you want to read more:
    --
    --     * [Here is a link]
    -- (https://www.reddit.com/r/haskell/comments/5bnr6b/mocking_in_haskell/)
    --     * [Another link]
    -- (https://lexi-lambda.github.io/blog/2017/06/29/unit-testing-effectful-haskell-with-monad-mock/)
    , module Discord.Internal.Monad
    -- ** MonadIO
    -- | Exported solely for convenience purposes, since many modules that use
    -- Commands require the MonadIO constraint, but it can get confusing where
    -- to import it from (UnliftIO or Control.Monad.IO.Class). The one exported
    -- from this module is from "Control.Monad.IO.Class" which is in @base@.
    , MonadIO(..)
    -- * Common Errors #commonerrors#
    -- | Here are some common errors that can occur when defining commands.
    -- They may appear cryptic, but they are most of the time dealable.
    --
    -- @
    -- Could not deduce (ParsableArgument p0) arising from the use of \'command'.
    -- The type variable 'p0' is ambiguous.
    -- @
    --     * The type for one of the arguments to your handler function cannot
    --     be inferred. Make sure you use the argument, otherwise, just remove it.
    --
    -- @
    -- Could not deduce (ParsableArgument SomeType) arising from the use of
    -- \'command'.
    -- @
    --
    --     * The type could be inferred as SomeType, but it's not an instance
    --     of ParsableArgument. Contribute your own parser in @Command/Parser.hs@.
    --
    -- @
    -- Could not deduce (MonadIO m) arising from the use of \'liftIO'.
    -- @
    --
    --     * Your handler requires IO actions, but you haven't given the
    --     appropriate constraint. Add @(MonadIO m)@.
    --     * Rationale: This happens because some handlers are pure and don't need IO -
    --     it's better to explicitly signify which actions you're going to use
    --     in the constraints than to add a catch-all constraint into the
    --     definition of @MonadDiscord@.
    -- 
    -- If an error is super duper cryptic, it may be a bug in the Commands 
    -- module itself, in which case we may need a rewrite.
    ) where

import           Control.Applicative        ( Alternative(..)
                                            , liftA2
                                            )
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
                                            , unless
                                            )
import           Data.Maybe                 ( catMaybes )
import qualified Data.Text as T
import           Text.Parsec.Combinator
import           Text.Parsec.Error          ( errorMessages
                                            , showErrorMessages
                                            )
import qualified Text.Parsec.Text as T
import           Text.Parsec                ( parse
                                            , eof
                                            , ParseError
                                            , space
                                            , anyChar
                                            , string
                                            , getInput
                                            )
import           Text.Regex.TDFA            ( (=~) )
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
-- @Command m@ is a command that runs in the monad @m@, which when triggered
-- will run a polyvariadic handler function. The handler *must* run in the
-- @m@ monad, which is enforced by the type-checker (to see the details, look in
-- @CommandHandlerType@ in the source code).
--
-- The contents of this abstract datatype are not exported from this module for
-- encapsulation. Use 'command', 'parsecCommand', or 'regexCommand' to
-- instantiate one.
data Command m = Command
    { commandName         :: T.Text
    -- ^ The name of the command.
    , commandPrefix       :: T.Text
    -- ^ The prefix for the command. 
    , commandAliases      :: [T.Text]
    -- ^ Any alias names for the command.
    , commandInitialMatch :: Message -> Command m -> Maybe [T.Text]
    -- ^ A function that performs an initial match check before checking for
    -- requirements or applying arguments. Grabs the necessary parts to be passed
    -- into commandApplier. If not matching, this will be Nothing.
    -- TODO: Find a better type than [T.Text], that can express all of:
    -- 1. Regex capture results (list of captured T.Text)
    -- 2. Custom parsec result type (String)
    -- 3. Arguments after a normal command name (T.Text: fed into the parser)
    , commandApplier      :: Message -> [T.Text] -> m ()
    -- ^ The function used to apply the arguments into a handler. It needs to
    -- take a 'Message' that triggered the command, the input text, and produce
    -- an action in the monad @m@.
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
-- receiving a message will run @handler@. The @name@ cannot contain any spaces,
-- as it breaks the parser. The @handler@ function can take an arbitrary amount
-- of arguments of arbitrary types (it is polyvariadic), as long as they are
-- instances of 'ParsableArgument'.
--
-- The Command that this function creates is polymorphic in the Monad it is run
-- in. This means you can call it from 'DiscordHandler' or any other
-- Monad that satisfies the constraints of 'MonadDiscord'.
-- 
-- ==== __See some examples__
--
-- @pong@ below responds to a ping by a pong.
--
-- @
-- pong :: (MonadDiscord m) => Command m
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
-- @weather@ below shows having arbitrary arguments in action. @location@ is
-- likely inferred to be 'T.Text'.
--
-- @
-- weather = command "weather" $ \\msg location -> do
--     result <- liftIO $ getWeather location
--     respond msg $ "Weather at " <> location <> " is " <> result <> "!"
-- @
--
-- @complex@ shows that you can parse any type! (as long as you create an
-- instance declaration of 'ParsableArgument' for it). Ideally this should be
-- placed in @Command/Parser.hs@ among all the other parsers.
--
-- @
-- instance ParsableArgument Int where
--     parserForArg msg = do
--         -- some parsec
--         parsed \<- read \<$> many digit
--         endOrSpaces
--         pure parsed
--
-- complex :: (MonadDiscord m) => Command m
-- complex = command "setLimit" $ \\i -> do
--     ...
-- @
--
command
    :: (CommandHandlerType m h , MonadDiscord m)
    => T.Text
    -- ^ The name of the command.
    -> h
    -- ^ The handler for the command, that takes an arbitrary amount of
    -- 'ParsableArgument's and returns a @m ()@
    -> Command m
command name commandHandler = Command
    { commandName         = name
    , commandPrefix       = ":"
    , commandAliases      = []
    , commandInitialMatch = \msg cmd ->
        case parse (parseCommandName cmd) "" (messageText msg) of
            Left e -> Nothing
            Right args -> Just [args]
    , commandApplier      = \x y -> applyArgs commandHandler x (head y)
    , commandErrorHandler = defaultErrorHandler
    , commandHelp         = "Help not available."
    , commandRequires     = []
    }

-- | @onError@ overwrites the default error handler of a command with a custom
-- implementation. Usually the default 'defaultErrorHandler' suffices.
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
    -> Command m
    -> Command m
onError errorHandler cmd = cmd
    { commandErrorHandler = errorHandler
    }

-- | @prefix@ overwrites the default command prefix ":" of a command with a
-- custom one. This is ignored if a regex parser is used, however it is still
-- applicable for 'parsecCommand'.
--
-- @
-- example
--     = prefix "->"
--     . command "idk" $ do
--         ...
-- @
prefix
    :: T.Text
    -> Command m
    -> Command m
prefix newPrefix cmd = cmd
    { commandPrefix = newPrefix
    }

-- | @requires@ adds a requirement to the command. The requirement is a function
-- that takes a 'Message' and either returns @'Nothing'@ (no problem) or
-- @'Just' "explanation"@. The function is in the @m@ monad so it can access
-- any additional information from Discord as necessary.
--
-- Commands default to having no requirements.
requires :: (Message -> m (Maybe T.Text)) -> Command m -> Command m
requires requirement cmd = cmd
    { commandRequires = requirement : commandRequires cmd
    }

-- | @help@ sets the help message for the command. The default is "Help not
-- available."
help :: T.Text -> Command m -> Command m
help newHelp cmd = cmd
    { commandHelp = newHelp
    }

-- | @alias@ adds an alias for the command's name. This is ignored if a custom
-- parser like 'regexCommand' or 'parsecCommand' is used.
--
-- Functionally, this is equivalent to defining a new command with the same
-- handler, however the aliases may not appear on help pages.
alias :: T.Text -> Command m -> Command m
alias newAlias cmd = cmd
    { commandAliases = newAlias : commandAliases cmd
    }

-- | @parsecCommand@ defines a command that has no name, and has a custom
-- parser that begins from the start of a message. It can help things like
-- "::quotes" because they have special syntax that demands a special parser.
--
-- 'alias', if used together, is ignored. Other compoasble functions like 'help',
-- 'prefix', 'requires', and 'onError' are still valid.
--
-- The handler __must__ take a 'Message' and a 'String' as argument (nothing
-- more, nothing less), where the latter is the result of the parser.
--
-- @
-- example
--     = requires moderatorPrivs
--     . prefix "~~"
--     . parsecCommand (string "abc" >> many1 anyChar) $ \\msg quoteName -> do
--         ...
--         -- this is triggered on "~~abc\<one or more chars>" where quoteName
--         -- contains the section enclosed in \<>
-- @
parsecCommand
    :: (MonadDiscord m)
    => T.Parser String
    -- ^ The custom parser for the command. It has to return a 'String'.
    -> (Message -> String -> m ())
    -- ^ The handler for the command.
    -> Command m
parsecCommand parserFunc commandHandler = Command
    { commandName         = "<custom parser>"
    , commandPrefix       = ""
    , commandAliases      = []
    , commandInitialMatch = \msg cmd ->
      let
        parser = do
            -- consume prefix
            string (T.unpack $ commandPrefix cmd)
            -- apply parser
            parserFunc
      in
        case parse parser "" (messageText msg) of
            Left e -> Nothing
            Right result -> Just [T.pack $ result]
            -- has to be packed and unpacked, which is not really good.
            -- TODO: find some datatype that can express String, T.Text, and [T.Text]
    , commandApplier      = \x y -> commandHandler x (T.unpack $ head y)
    , commandErrorHandler = defaultErrorHandler
    , commandHelp         = "Help not available."
    , commandRequires     = []
    }

-- | @regexCommand@ defines a command that has no name, and has a custom
-- regular expression matcher, that __searches across any part of the message__.
--
-- 'prefix' and 'alias', if used together, are ignored. Other compoasble
-- functions like 'help', 'requires', and 'onError' are still valid.
--
-- The handler __must__ take a 'Message' and a '[T.Text]' as argument, where
-- the latter are the list of captured values from the Regex (same as the past
-- @newCommand@).
--
-- @
-- thatcher = regexCommand "thatcher [Ii]s ([Dd]ead|[Aa]live)" $ \\msg caps -> do
--     let verb = head caps
-- ...
-- @
regexCommand
    :: (MonadDiscord m)
    => T.Text
    -> (Message -> [T.Text] -> m ())
    -> Command m
regexCommand regex commandHandler = Command
    { commandName         = "<regex>"
    , commandPrefix       = ""
    , commandAliases      = []
    , commandInitialMatch = \msg cmd ->
      let
        match = messageText msg =~ regex :: [[T.Text]]
      in
        if null match then Nothing else Just $ (concat . map tail) match
    , commandApplier      = commandHandler
    , commandErrorHandler = defaultErrorHandler
    , commandHelp         = "Help not available."
    , commandRequires     = []
    }

-- | @runCommand command msg@ runs the specified 'Command' with the given
-- message. It first does the initial checks:
--
-- For commands registered with 'command', the check will check for the prefix
-- and the name.
--
-- For commands registered with 'regexCommand', the check will check against the
-- regex.
--
-- For commands registered with 'parsecCommand', the check will check for the 
-- prefix and the custom parser.
--
-- Any failures during this stage is silently ignored, as it may still be a valid
-- command elsewhere. After this, the requirements are checked (chance, priv, etc).
-- Finally, the @commandApplier@ is called. Any errors inside the handler is
-- caught and appropriately handled.
--
-- @
-- runCommand pong :: Message -> m ()
-- @
runCommand
    :: forall m.
    (MonadDiscord m)
    => Command m
    -- ^ The command to run against.
    -> Message
    -- ^ The message to run the command with.
    -> m ()
runCommand cmd@Command{ commandInitialMatch, commandApplier, commandErrorHandler, commandRequires } msg =
    case (commandInitialMatch msg cmd) of
        Nothing -> pure ()
        Just results -> do
            -- Check for requirements. checks will be a list of Maybes
            checks <- sequence $ map ($ msg) commandRequires
            -- only get the Justs
            let failedChecks = catMaybes checks
            if null failedChecks
                then
                    -- Apply the arguments on the handler
                    commandApplier msg results
                        -- Asynchrnous errors are not caught as the `catch`
                        -- comes from Control.Exception.Safe. This is good.
                        `catch` basicErrorCatcher
                        `catch` allErrorCatcher
                else
                    -- give the first requirement error to the error handler
                    basicErrorCatcher (RequirementError $ head failedChecks)
  where
    -- | Catch CommandErrors and handle them with the handler
    basicErrorCatcher :: CommandError -> m ()
    basicErrorCatcher = commandErrorHandler msg

    -- | Catch any and all errors, including ones thrown in basicErrorCatcher.
    allErrorCatcher :: SomeException -> m ()
    allErrorCatcher = (commandErrorHandler msg) . HaskellError

-- | @runCommands@ calls runCommand for all the Commands, and folds them with
-- the Monadic bind ('>>').
runCommands
    :: (MonadDiscord m, Alternative m)
    => [Command m]
    -> Message
    -> m ()
runCommands cmds msg = void $ sequence $ map ($ msg) (map runCommand cmds)

-- | @runHelp@ creates a super duper simple help command that just lists
-- each command's names together with their help text.
runHelp
    :: (MonadDiscord m)
    => T.Text
    -> [Command m]
    -> Message
    -> m ()
runHelp name cmds
    = runCommand
    . command name $ \msg -> do
        chan <- createDM (userId $ messageAuthor msg)
        void $ createMessage (channelId chan) $
            "```" <> (T.intercalate "\n" $ map createCommandHelp cmds) <> "```"
  where
    createCommandHelp :: Command m -> T.Text
    createCommandHelp Command{ commandPrefix, commandName, commandHelp } =
        "# " <> commandPrefix <> commandName <> "\n" <> commandHelp



-- | @defaultErrorHandler m e@ is the default error handler unless you override
-- it manually. This is exported and documented for reference only.
--
-- [On argument error] It calls 'respond' with the errors. This isn't owoified
-- for legibility.
-- [On requirement error] It sends a DM to the invoking user with the errors.
-- [On a processing error] It calls 'respond' with the error.
-- [On a Discord request failure] It calls 'respond' with the error.
-- [On a Runtime/Haskell error] It calls 'respond' with the error, owoified.
defaultErrorHandler
    :: (MonadDiscord m)
    => Message
    -> CommandError
    -> m ()
defaultErrorHandler m e =
    case e of
        ArgumentParseError x ->
            respond m x
        RequirementError x -> do
            chan <- createDM (userId $ messageAuthor m)
            void $ createMessage (channelId chan) x
        ProcessingError x ->
            respond m x
        DiscordError x ->
            respond m $ T.pack $ "Discord request failed with a " <> (show x)
        HaskellError x ->
            respond m $ owoify $ T.pack $ "Runtime error (contact OwenDev, this is a bug): " <> show x









-- @CommandHandlerType@ is a dataclass for all types of arguments that a
-- command handler may have. Its only function is @applyArgs@.
class (MonadThrow m) => CommandHandlerType m h | h -> m where
    applyArgs
        :: h
        -- ^ The handler. It must be in the same monad as @m@.
        -> Message
        -- ^ The relevant message.
        -> T.Text
        -- ^ The arguments of the command, i.e. everything after the command
        -- name followed by one or more spaces. Is "" if empty.
        -> m ()
        -- ^ The monad to run the handler in, and to throw parse errors in.

-- | For the case when all arguments have been applied. Base case.
instance (MonadThrow m) => CommandHandlerType m (m ()) where
    applyArgs handler msg input =
        case parse eof "" input of
            Left e -> throwM $ ArgumentParseError $
                "Too many arguments! " <> showErrAsText e
            Right _ -> handler

-- | For the case where there are multiple arguments to apply. 
instance (MonadThrow m, ParsableArgument a, CommandHandlerType m b) => CommandHandlerType m (a -> b) where
    applyArgs handler msg input =
        case parse (liftA2 (,) (parserForArg msg) getInput) "arguments" input of
            Left e -> throwM $ ArgumentParseError $
                "Error while parsing argument. " <> showErrAsText e
            Right (x, remaining) -> applyArgs (handler x) msg remaining

-- | For the case where there is only one argument to apply.
-- It overlaps the previous instance (it is more specific). With the OVERLAPPING
-- pragma, GHC prefers this one when both match.
--
-- This instance is necessary because otherwise @Message -> m ()@ will match
-- both @m ()@ (@(->) r@ is a monad) and @a -> b@ and since neither are more
-- specific, GHC cannot prefer one over the other, even with any pragmas.
instance {-# OVERLAPPING #-} (MonadThrow m, ParsableArgument a) => CommandHandlerType m (a -> m ()) where
    applyArgs handler msg input =
        case parse (liftA2 (,) (parserForArg msg) getInput) "arguments" input of
            Left e -> throwM $ ArgumentParseError $
                "Error while parsing argument. " <> showErrAsText e
            Right (x, remaining) -> applyArgs (handler x) msg remaining

-- The default 'Show' instance for ParseError contains the error position,
-- which only adds clutter in a Discord message. This defines a much
-- simpler string representation.
showErrAsText :: ParseError -> T.Text
showErrAsText err = T.tail $ T.pack $ showErrorMessages "or" "unknown parse error"
    "Expecting" "Unexpected" "end of message" (errorMessages err)

-- | @parseCommandName@ returns a parser that tries to consume the prefix,
-- Command name, appropriate amount of spaces, and returns the arguments.
-- If there are no arguments, it will return the empty text, "".
parseCommandName :: Command m -> T.Parser T.Text
parseCommandName cmd = do
    -- consume prefix
    string (T.unpack $ commandPrefix cmd)
    -- consume at least 1 character until a space is encountered
    -- don't consume the space
    cmdName <- manyTill1 anyChar (void (lookAhead space) <|> eof)
    -- check it's the proper command
    guard $ T.pack cmdName `elem` (commandName cmd : commandAliases cmd)
    -- parse either an end of input, or spaces followed by arguments
    (eof >> pure "") <|> do
        -- consumes one or more isSpace characters
        many1 space
        -- consume everything until end of input
        args <- manyTill anyChar eof
        -- return the args
        pure (T.pack args)
