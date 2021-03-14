{-# LANGUAGE OverloadedStrings #-}

module Owoifier ( owoify
                , weakOwoify 
                ) where

import qualified Data.Text as T

owoify :: T.Text -> T.Text
owoify = (<> " owo") . inserty . insertY . weakOwoify

weakOwoify :: T.Text -> T.Text
weakOwoify = T.map owoifyChar

-- returns ['l'/'L'/'r'/'R'] for ['w'/'W'], otherwise `id`
owoifyChar :: Char -> Char
owoifyChar c
    | c `elem` ("LR" :: [Char]) = 'W'
    | c `elem` ("lr" :: [Char]) = 'w'
    | otherwise     = c

-- inserts a 'y' between ['n'/'m'] and ['o'/'O']
inserty :: T.Text -> T.Text
inserty s = foldl (\s x -> T.replace x (T.intersperse 'y' x) s) s 
                    $ T.cons <$> ("nm" :: [Char]) 
                             <*> (T.singleton <$> "oO")

insertY :: T.Text -> T.Text
insertY s = foldl (\s x -> T.replace x (T.intersperse 'Y' x) s) s 
                    $ T.cons <$> ("NM" :: [Char]) 
                             <*> (T.singleton <$> "oO")
