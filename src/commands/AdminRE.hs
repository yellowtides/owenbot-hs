{-# LANGUAGE OverloadedStrings #-}

module AdminRE (instanceRE, restartRE) where

import qualified Data.Text as T
import TemplateRE (trailingWS)

instanceRE, restartRE :: T.Text
instanceRE = "^:instance" <> trailingWS -- :instance
restartRE  = "^:restart" <> trailingWS