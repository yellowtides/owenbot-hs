{-# LANGUAGE OverloadedStrings #-}

module OwenRegex where

import qualified Data.Text as T

oneDot, twoDot :: T.Text
oneDot = " ([0-9]{1,3}\\.[0-9]{1,3})"
twoDot = " ([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3})"

thmRE, defRE, lemmaRE, textbookRE, syllogismsRE, booleanRE, hoogleInfRE, helpRE :: T.Text
thmRE        = "^:th(eore)?m"    <> twoDot <> trailingWS         -- :thm
defRE        = "^:def(inition)?" <> oneDot <> trailingWS         -- :def
lemmaRE      = "^:lem(ma)?"      <> twoDot <> trailingWS         -- :lemma
textbookRE   = "^:textbook"                <> trailingWS         -- :textbook
syllogismsRE = "^:syl(logisms)?"           <> trailingWS         -- :syllogisms
booleanRE    = "^:bool(ean)?"              <> trailingWS         -- :boolean
hoogleInfRE  = "^:doc [a-z']+"             <> trailingWS         -- :doc
helpRE       = "^:he(l|w)pme"              <> trailingWS         -- :help

trailingWS :: T.Text
trailingWS   = " *$"

owoifiableRE :: T.Text
owoifiableRE  = "[lLrR]|[nNmM][oO]"

nietzscheRE :: T.Text
nietzscheRE = "[gG]od *[iI]s *[dD]ead"

commandREs :: [T.Text] -- list of all command regexes
commandREs = [thmRE, defRE, lemmaRE, textbookRE, syllogismsRE, booleanRE, hoogleInfRE, helpRE]
