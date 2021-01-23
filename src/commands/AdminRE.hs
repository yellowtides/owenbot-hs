{-# LANGUAGE OverloadedStrings #-}

module AdminRE (gitRE, instanceRE, restartRE) where

import qualified Data.Text as T
import TemplateRE (trailingWS)

gitRE, instanceRE, restartRE :: T.Text
gitRE      = "^:git" <> trailingWS
instanceRE = "^:instance" <> trailingWS
restartRE  = "^:restart" <> trailingWS