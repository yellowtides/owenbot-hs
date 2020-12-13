{-# LANGUAGE OverloadedStrings #-}

module OwenRegex where

import qualified Data.Text as T

thmRE, defRE, lemmaRE, textbookRE, syllogismsRE, booleanRE, hoogleInfRE, helpRE :: T.Text
thmRE        = ":(thm|theorem) *([0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{1,2})"         -- :thm
defRE        = "^:(def|definition) *([0-9]{1,2}\\.[0-9]{1,2})"                  -- :def
lemmaRE      = ":(lem|lemma) *([0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{1,2})"           -- :lemma
textbookRE   = ":textbook *"                                                    -- :textbook
syllogismsRE = ":(syllogisms|syl) *"                                            -- :syllogisms
booleanRE    = ":(boolean|bool) *"                                              -- :boolean
hoogleInfRE  = "^:doc [a-z']+"                                                  -- :doc
helpRE       = "^:helpme *"

owoifiableRE :: T.Text
owoifiableRE  = "[lrLR]|[nNmM][oO]"

commandREs :: [T.Text] -- list of all command regexes 
commandREs = [thmRE, defRE, lemmaRE, textbookRE, syllogismsRE, booleanRE, hoogleInfRE, helpRE]