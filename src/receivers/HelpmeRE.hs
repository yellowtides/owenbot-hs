{-# LANGUAGE OverloadedStrings #-}

module HelpmeRE where

import qualified Data.Text as T
import TemplateRE (trailingWS)

helpRE :: T.Text
helpRE = "^:helpme" <> trailingWS -- :help