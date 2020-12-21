{-# LANGUAGE OverloadedStrings #-}

module Inf1ARE where

import qualified Data.Text as T
import TemplateRE (trailingWS)

hoogleInfRE, booleanRE, syllogismsRE, i1atextbookRE :: T.Text
i1atextbookRE = "^:i1atextbook"     <> trailingWS         -- :i1atextbook
syllogismsRE  = "^:syl(logisms)?"   <> trailingWS         -- :syllogisms
booleanRE     = "^:bool(ean)?"      <> trailingWS         -- :boolean
hoogleInfRE   = "^:doc [a-z']+"     <> trailingWS         -- :doc