{-# LANGUAGE OverloadedStrings #-}

module AdminRE (instanceRE) where

import qualified Data.Text as T
import TemplateRE (trailingWS)

instanceRE :: T.Text
instanceRE = "^:instance" <> trailingWS -- :instance