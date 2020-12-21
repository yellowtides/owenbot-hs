{-# LANGUAGE OverloadedStrings #-}

module TemplateRE where

import qualified Data.Text as T

threeDig :: T.Text
threeDig = "[0-9]{1,3}"

oneDot, twoDot :: T.Text
oneDot = threeDig <> "\\." <> threeDig
twoDot = oneDot   <> "\\." <> threeDig

trailingWS :: T.Text
trailingWS = " *$"