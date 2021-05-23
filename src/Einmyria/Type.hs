module Einmyria.Type
    ( Einmyria(..)
    ) where

import qualified Data.Text as T
import           Discord.Types

import           Einmyria.Error

data Einmyria h m = Einmyria
    { einmyriaName         :: T.Text
    , einmyriaHandler      :: h
    , einmyriaArgApplier   :: Message -> T.Text -> h -> m ()
    , einmyriaErrorHandler :: Message -> EinmyriaError -> m ()
    , einmyriaHelp         :: T.Text
    , einmyriaRequires     :: [Message -> Either T.Text ()]
    }
