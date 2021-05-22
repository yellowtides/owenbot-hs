module Einmyria.Error
    ( EinmyriaError(..)
    ) where

import qualified Data.Text as T
import           Control.Exception.Safe     ( SomeException
                                            , Exception
                                            )
import           Discord                    ( RestCallErrorCode )

data EinmyriaError
    = ArgumentParseError T.Text
    | RequirementError T.Text
    | ProcessingError T.Text
    | DiscordError RestCallErrorCode
    | HaskellError SomeException
    deriving Show

instance Exception EinmyriaError
