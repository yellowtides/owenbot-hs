{-# LANGUAGE OverloadedStrings #-}

module CalcRE where

import qualified Data.Text as T
import TemplateRE (defRE, lemRE, oneDot, textbookRE, thmRE, trailingWS, twoDot)

calcRE :: T.Text
calcRE = "calc *"

calctextbookRE :: T.Text
calctextbookRE = textbookRE <> calcRE <> trailingWS