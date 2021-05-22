{-# LANGUAGE FlexibleInstances #-} -- allow arbitrary nested types in instance declarations
{-# LANGUAGE OverloadedStrings #-} -- for T.Text overloading
{-# LANGUAGE ExistentialQuantification #-} -- for explitit forall.
{-# LANGUAGE ScopedTypeVariables #-} -- for using type parameters in `where` statements
{-# LANGUAGE MultiParamTypeClasses #-} -- for EinmyriaHandlerType
{-
Inspired heavily by the calamity-commands library (MIT).
Amateur attempt at abstraction.
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
import           Control.Monad              ( void
                                            , when
                                            )
import qualified Data.Text as T
import qualified Data.Proxy as P
import           Text.Parsec.Combinator
import qualified Text.Parsec.Text as T
import           Text.Parsec
import           UnliftIO                   ( liftIO )

import           Discord.Monad
import           Discord.Types
import           Discord

import           Einmyria.Error             ( EinmyriaError(..) )


data Einmyria h m = Einmyria
    { einmyriaName         :: T.Text
    , einmyriaHandler      :: h
    , einmyriaArgApplier   :: Message -> T.Text -> h -> m ()
    , einmyriaErrorHandler :: Message -> EinmyriaError -> m ()
    , einmyriaHelp         :: T.Text
    , einmyriaRequires     :: [Message -> Either T.Text ()]
    }

-- | Create a command.
command
    :: (EinmyriaHandlerType h m , MonadDiscord m, MonadThrow m)
    => T.Text
    -> h
    -> Einmyria h m
command name handler = Einmyria
    { einmyriaName         = name
    , einmyriaHandler      = handler
    , einmyriaArgApplier   = applyArgs name
    , einmyriaErrorHandler = \msg reason -> do
        respond msg (T.pack $ show reason)
    , einmyriaHelp         = "Help not available."
    , einmyriaRequires     = []
    }

-- | Build a command.
runCommand
    :: forall m h.
    (MonadDiscord m, Alternative m, MonadCatch m, MonadThrow m, MonadIO m)
    => Einmyria h m
    -> Message
    -> m ()
runCommand einmyria msg = do
    -- First check that the command name is right.
    case runParser (parseEinmyriaName einmyria) () "" (messageText msg) of
        Left e -> pure ()
        Right args -> do
            -- Apply the arguments one by one on the appropriate handler
            ((einmyriaArgApplier einmyria) msg args (einmyriaHandler einmyria))
                `catch` (errorCatcher)
                -- ^ catch einmyria-errors only
                `catch` allErrorCatcher
                -- ^ catch any and all errors, including ones thrown in the first
                -- error handler. Asynchrnous errors are not caught.
  where
    errorCatcher :: EinmyriaError -> m ()
    errorCatcher = (einmyriaErrorHandler einmyria) msg

    allErrorCatcher :: SomeException -> m ()
    allErrorCatcher = ((einmyriaErrorHandler einmyria) msg) . HaskellError

-- Parser that consumes the prefix, command name, and returns the arguments.
parseEinmyriaName :: Einmyria -> T.Parser T.Text
parseEinmyriaName ein = do
    -- consume prefix
    char ':'
    -- consume all characters until a space is encountered, don't consume space 
    cmdName <- manyTill anyChar (try spaces)
    -- consumes one or more isSpace characters
    many1 spaces
    -- consume everything until end of input
    args <- manyTill anyChar eof
    -- return the args
    pure (T.pack args)

-- Argument parser, in any monad that can throw exceptions.
class (MonadThrow m) => EinmyriaHandlerType h m where
    applyArgs :: T.Text -> Message -> T.Text -> h -> m ()

instance (MonadThrow m) => EinmyriaHandlerType (m ()) m where
    applyArgs name msg remaining handler = 
        if T.length remaining > 0
            then throwM $ ArgumentParseError $ "Too many arguments " <> remaining <> (T.pack $ show $ T.length remaining)
            else handler

instance {-# OVERLAPPING #-} (MonadThrow m, ParsableArgument a) => EinmyriaHandlerType (a -> m ()) m where
    applyArgs name msg input handler =
        case runParser (parserForArg msg) () "" input of
            Left e -> throwM $ ArgumentParseError $ T.pack $ show e
            Right (x, remaining) -> applyArgs name msg remaining (handler x)

instance (MonadThrow m, ParsableArgument a, EinmyriaHandlerType b m) => EinmyriaHandlerType (a -> b) m where
    applyArgs name msg input handler =
        case runParser (parserForArg msg) () "" input of
            Left e -> throwM $ ArgumentParseError $ T.pack $ show e
            Right (x, remaining) -> applyArgs name msg remaining (handler x) 

class ParsableArgument a where
    parserForArg :: Message -> T.Parser (a, T.Text)

instance ParsableArgument Message where
    parserForArg msg = do
        remaining <- getInput
        pure (msg, remaining)

instance ParsableArgument T.Text where
    parserForArg msg = do
        parsed <- many $ noneOf " "
        remaining <- getInput
        pure (T.pack parsed, remaining)

-- instance ParsableArgument [T.Text] where
    -- parserForArg msg = (T.pack <$>) $ many $ noneOf ""

