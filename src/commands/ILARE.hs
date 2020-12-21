{-# LANGUAGE OverloadedStrings #-}

module ILARE where

import qualified Data.Text as T
import TemplateRE (oneDot, twoDot, trailingWS)

ilathmRE, iladefRE, ilalemmaRE, ilatextbookRE :: T.Text
ilathmRE      = "^:ila *th(eore)?m *"   <> twoDot <> trailingWS         -- :ila thm
iladefRE      = "^:ila *def(inition) *" <> oneDot <> trailingWS         -- :ila def
ilalemmaRE    = "^:ila *lem(ma)? *"     <> twoDot <> trailingWS         -- :ila lemma
ilatextbookRE = "^:ilatextbook"                   <> trailingWS         -- :ilatextbook