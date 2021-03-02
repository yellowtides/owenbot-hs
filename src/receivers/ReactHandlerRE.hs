{-# LANGUAGE OverloadedStrings #-}

module ReactHandlerRE (reactLimitRE) where

import qualified Data.Text as T
import TemplateRE (trailingWS)

threeDig :: T.Text
threeDig = "[0-9]{1,3}"

reactLimitRE :: T.Text
reactLimitRE = "^:reactLimit " <> threeDig <> trailingWS -- :reactLimit