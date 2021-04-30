{-# LANGUAGE OverloadedStrings #-}

module TemplateRE
    ( oneDot
    , twoDot
    , trailingWS
    , thmRE
    , defRE
    , lemRE
    , textbookRE
    ) where

import qualified Data.Text as T

threeDig :: T.Text
threeDig = "[0-9]{1,3}"

oneDot, twoDot :: T.Text
oneDot = threeDig <> "\\." <> threeDig
twoDot = oneDot   <> "\\." <> threeDig

trailingWS :: T.Text
trailingWS = " *$"

textbookRE :: T.Text
textbookRE = "t(ext)?b(ook)? *"

thmRE, defRE, lemRE :: T.Text
thmRE = "th(eore)?m *"
defRE = "def(inition)? *"
lemRE = "lem(ma)? *"
