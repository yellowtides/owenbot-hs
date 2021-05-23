module Einmyria.Error
    ( EinmyriaError(..)
    ) where

import qualified Data.Text as T
import           Control.Exception.Safe     ( SomeException
                                            , Exception
                                            )
import           Text.Parsec.Error          ( ParseError )

import           Discord                    ( RestCallErrorCode )

data EinmyriaError
    = ArgumentParseError ParseError
    | RequirementError T.Text
    | ProcessingError T.Text
    | DiscordError RestCallErrorCode
    | HaskellError SomeException
    deriving Show

instance Exception EinmyriaError
