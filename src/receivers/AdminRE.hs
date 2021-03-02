{-# LANGUAGE OverloadedStrings #-}

module AdminRE ( gitRE, instanceRE, restartRE, statusRE, addDevsRE, correctStatusRE) where

import qualified Data.Text as T
import TemplateRE (trailingWS)

gitRE, instanceRE, restartRE, statusRE, addDevsRE :: T.Text
gitRE      = "^:repo" <> trailingWS
instanceRE = "^:instance" <> trailingWS
restartRE  = "^:restart" <> trailingWS
statusRE = "^:status(.*)$"
addDevsRE = "^:addDev [0-9]{1,32}$"
-- This is matched when giving the user correct syntax
correctStatusRE :: String
correctStatusRE = "^:status (online|idle|dnd|invisible) (playing|streaming|listening|competing) (.*)$"
