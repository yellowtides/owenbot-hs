{-# LANGUAGE OverloadedStrings #-}

module TemplateRE
    ( trailingWS
    , accoladedArgRE
    , quotedArgRE
    , spaceRE
    ) where

import qualified Data.Text as T

trailingWS :: T.Text
trailingWS = " *$"

quotedArgRE :: T.Text
quotedArgRE = "\"(.*)\""

accoladedArgRE :: T.Text
accoladedArgRE  = "({.*})"

spaceRE :: T.Text
spaceRE = " +"
