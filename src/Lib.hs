-- {-# LANGUAGE OverloadedStrings #-}

module Lib
    ( isCommand
    ) where

import Data.List (isPrefixOf)
import Data.Char (toLower)

import Hoogle

cmds :: [String]
cmds = ["thm", "theorem", "def", "definition", "lem", "lemma",
        "hoogle", "doc"]

lower = map toLower

isCommand :: [String] -> String -> Bool
isCommand cmds (c:str) = c == ':' && any (`isPrefixOf` lower (str++" ")) cmds

isThm :: String -> Bool
isThm    = isCommand ["thm", "theorem"]

isDef :: String -> Bool
isDef    = isCommand ["def", "definition"]

isLem :: String -> Bool
isLem    = isCommand ["lem", "lemma"]

isHoogle :: String -> Bool
isHoogle = isCommand ["hoogle"]

isDoc:: String -> Bool
isDoc    = isCommand ["doc"]
