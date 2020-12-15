{-# LANGUAGE OverloadedStrings #-}

module OwenRegex where

import qualified Data.Text as T

twoDigit, threeDigit :: T.Text
twoDigit   = " ([0-9]{1,2}\\.[0-9]{1,2})"
threeDigit = " ([0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{1,2})"

thmRE, defRE, lemmaRE, textbookRE, syllogismsRE, booleanRE, hoogleInfRE, helpRE :: T.Text
thmRE        = "^:th(eore)?m"    <> threeDigit <> trailingWS         -- :thm
defRE        = "^:def(inition)?" <> twoDigit   <> trailingWS         -- :def
lemmaRE      = "^:lem(ma)?"      <> threeDigit <> trailingWS         -- :lemma
textbookRE   = "^:textbook"                    <> trailingWS         -- :textbook
syllogismsRE = "^:syl(logisms)?"               <> trailingWS         -- :syllogisms
booleanRE    = "^:bool(ean)?"                  <> trailingWS         -- :boolean
hoogleInfRE  = "^:doc [a-z']+"                 <> trailingWS         -- :doc
helpRE       = "^:he(l|w)pme"                  <> trailingWS         -- :help

trailingWS :: T.Text
trailingWS   = "[:space:]*$"

owoifiableRE :: T.Text
owoifiableRE  = "[lrLR]|[nNmM][oO]"

commandREs :: [T.Text] -- list of all command regexes
commandREs = [thmRE, defRE, lemmaRE, textbookRE, syllogismsRE, booleanRE, hoogleInfRE, helpRE]
