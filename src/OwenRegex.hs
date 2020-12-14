{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module OwenRegex where

import qualified Data.Text as T
import Text.RawString.QQ

thmRE, defRE, lemmaRE, textbookRE, syllogismsRE, booleanRE, hoogleInfRE, helpRE :: T.Text
thmRE        = [r|^:(thm|theorem)\s*([0-9]{1,2}\.[0-9]{1,2}\.[0-9]{1,2})\s*$|]   -- :thm
defRE        = "^:(def|definition) *([0-9]{1,2}\\.[0-9]{1,2})"                  -- :def
lemmaRE      = ":(lem|lemma) *([0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{1,2})"           -- :lemma
textbookRE   = ":textbook *"                                                    -- :textbook
syllogismsRE = ":(syllogisms|syl) *"                                            -- :syllogisms
booleanRE    = ":(boolean|bool) *"                                              -- :boolean
hoogleInfRE  = "^:doc [a-z']+"                                                  -- :doc
helpRE       = [r|^:helpme\s*$|]                                                -- :help

owoifiableRE :: T.Text
owoifiableRE = [r|[lrLR]|[nNmM][oO]|]

commandREs :: [T.Text] -- list of all command regexes 
commandREs = [thmRE, defRE, lemmaRE, textbookRE, syllogismsRE, booleanRE, hoogleInfRE, helpRE]