{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module OwenRegex where

import qualified Data.Text as T
import Text.RawString.QQ

twoDigit, threeDigit :: T.Text
twoDigit   = "([0-9]{1,2}\\.[0-9]{1,2})"
threeDigit = "([0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{1,2})"

thmRE, defRE, lemmaRE, textbookRE, syllogismsRE, booleanRE, hoogleInfRE, helpRE :: T.Text
thmRE        = ":(thm|theorem) *" `T.append` threeDigit                                 -- :thm
defRE        = "^:def(inition)? *" `T.append` twoDigit                                  -- :def
lemmaRE      = ":lem(ma)? *" `T.append` threeDigit                                      -- :lemma
textbookRE   = ":textbook *"                                                    -- :textbook
syllogismsRE = ":syl(logisms)? *"                                               -- :syllogisms
booleanRE    = ":bool(ean)? *"                                                  -- :boolean
hoogleInfRE  = "^:doc [a-z']+"                                                  -- :doc
helpRE       = "^:he(l|w)pme *"                                                 -- :help

owoifiableRE :: T.Text
owoifiableRE  = "[lrLR]|[nNmM][oO]"

commandREs :: [T.Text] -- list of all command regexes
commandREs = [thmRE, defRE, lemmaRE, textbookRE, syllogismsRE, booleanRE, hoogleInfRE, helpRE]
