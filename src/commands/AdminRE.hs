{-# LANGUAGE OverloadedStrings #-}

module AdminRE ( gitRE, instanceRE, restartRE, statusRE, correctStatusRE) where

import qualified Data.Text as T
import TemplateRE (trailingWS)

gitRE, instanceRE, restartRE :: T.Text
gitRE      = "^:repo" <> trailingWS
instanceRE = "^:instance" <> trailingWS
restartRE  = "^:restart" <> trailingWS

statusRE :: T.Text
statusRE = "^:status(.*)$"

-- This is matched when giving the user correct syntax
correctStatusRE :: String
correctStatusRE = "^:status (online|idle|dnd|invisible) (playing|streaming|listening|watching) (.*)$"
