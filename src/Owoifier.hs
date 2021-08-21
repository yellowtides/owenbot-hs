{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Owoifier (owoify, weakOwoify) where

import qualified Data.Text as T

-- | String constant, text enclosed in this will not be owoified
owolessDelim :: T.Text
owolessDelim = "```"

-- | Maps f over every other element in a list:
-- e.g. `[f a, a, f a, a]`
alternate :: (a -> a) -> [a] -> [a]
alternate f = zipWith ($) (cycle [f, id])

-- | This takes a text and returns an owofied text
-- e.g. "North ```West```" -> "Nyowth ```West``` owo"
owoify :: T.Text -> T.Text
owoify text = T.intercalate owolessDelim (alternate owoifySegment segments) <> " owo"
    where segments = T.splitOn owolessDelim text

-- | Implements the weakOwoify over the segments of text
owoifySegment :: T.Text -> T.Text
owoifySegment = doInserts . weakOwoify

-- | Maps the function that actually owofies the text over each character of the string
weakOwoify :: T.Text -> T.Text
weakOwoify = T.map owoifyChar

-- | Substitutes given characters for owoified variants in a string, otherwise `id`
owoifyChar :: Char -> Char
owoifyChar c
    | c `elem` ("LR" :: [Char]) = 'W'
    | c `elem` ("lr" :: [Char]) = 'w'
    | otherwise                 = c

-- | Takes the cartesian product of two lists of chars and packs as a Text
-- e.g. "nmNM <-> "o" returns ["no", "mo", "No", "Mo"]
(<->) :: [Char] -> [Char] -> [T.Text]
(<->) as bs = [ T.pack [a, b] | a <- as, b <- bs ]

-- | Checks if the y added should be lowercase
segmentsSmallY :: [T.Text]
segmentsSmallY = "NMnm" <-> "o" ++ "nm" <-> "O"

-- | Checks if the y added should be uppercase
segmentsBigY :: [T.Text]
segmentsBigY = "NM" <-> "O"

-- | Inserts the wanted symbol in between segments
mkRules :: [(Char, [T.Text])] -> [(T.Text, Char)]
mkRules defs = do
    (sym, segments) <- defs
    (, sym) <$> segments

-- | Depending on what type of segment it is, it inserts a Y or y
insertionRules :: [(T.Text, Char)]
insertionRules = mkRules [('y', segmentsSmallY), ('Y', segmentsBigY)]
-- | If it the word comes as a tuple with a Y then it is interpersed in the word and then replaces the og
applyRule :: (T.Text, Char) -> T.Text -> T.Text
applyRule rule =
    let
        (word, ins) = rule
        newWord     = T.intersperse ins word
    in T.replace word newWord

-- | Inserts the y-words in the text
doInserts :: T.Text -> T.Text
doInserts str = foldl (flip applyRule) str insertionRules
