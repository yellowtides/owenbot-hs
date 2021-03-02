{-# LANGUAGE OverloadedStrings #-}

module Inf1ARE where

import qualified Data.Text as T
import TemplateRE (trailingWS, textbookRE)

hoogleInfRE, booleanRE, syllogismsRE, i1atextbookRE :: T.Text
i1atextbookRE = textbookRE <> "i(nf)?1a" <> trailingWS         -- :textbook inf1a
syllogismsRE  = "^:syl(logisms)?"        <> trailingWS         -- :syllogisms
booleanRE     = "^:bool(ean)?"           <> trailingWS         -- :boolean
hoogleInfRE   = "^:doc [a-z']+"          <> trailingWS         -- :doc