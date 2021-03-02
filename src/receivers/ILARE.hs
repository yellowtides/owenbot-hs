{-# LANGUAGE OverloadedStrings #-}

module ILARE where

import qualified Data.Text as T
import TemplateRE (oneDot, twoDot, trailingWS, thmRE, defRE, lemRE, textbookRE)

ilaRE :: T.Text
ilaRE = "ila *"

ilathmRE, iladefRE, ilalemmaRE, ilatextbookRE :: T.Text
ilathmRE      = thmRE      <> ilaRE <> twoDot <> trailingWS
iladefRE      = defRE      <> ilaRE <> oneDot <> trailingWS
ilalemmaRE    = lemRE      <> ilaRE <> twoDot <> trailingWS
ilatextbookRE = textbookRE <> ilaRE           <> trailingWS