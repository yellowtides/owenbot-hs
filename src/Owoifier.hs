{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Owoifier ( owoify
                , weakOwoify
                ) where

import qualified Data.Text as T

-- | String constant, text enclosed in this will not be owoified
owolessDelim :: T.Text
owolessDelim = "```"

-- | Maps f over every other element in a list:
-- e.g. `[f a, a, f a, a]`
alternate :: (a -> a) -> [a] -> [a]
alternate f = zipWith ($) (cycle [f, id])

owoify :: T.Text -> T.Text
owoify text = T.intercalate owolessDelim (alternate owoifySegment segments) <> " owo"
    where segments = T.splitOn owolessDelim text

owoifySegment :: T.Text -> T.Text
owoifySegment = doInserts . weakOwoify

weakOwoify :: T.Text -> T.Text
weakOwoify = T.map owoifyChar

-- returns ['l'/'L'/'r'/'R'] for ['w'/'W'], otherwise `id`
owoifyChar :: Char -> Char
owoifyChar c
    | c `elem` ("LR" :: [Char]) = 'W'
    | c `elem` ("lr" :: [Char]) = 'w'
    | otherwise                 = c

-- | Takes the cartesian product of two lists of chars and packs as a Text
(<->) :: [Char] -> [Char] -> [T.Text]
(<->) as bs = [T.pack [a,b] | a <- as, b <- bs]

segmentsSmallY :: [T.Text]
segmentsSmallY = "NMnm" <-> "o" ++ "nm" <-> "O"

segmentsBigY :: [T.Text]
segmentsBigY = "NM" <-> "O"

mkRules :: [(Char, [T.Text])] -> [(T.Text, Char)]
mkRules defs = do
    (sym, segments) <- defs
    (, sym) <$> segments

insertionRules :: [(T.Text, Char)]
insertionRules = mkRules
    [ ('y', segmentsSmallY)
    , ('Y', segmentsBigY)
    ]

applyRule :: (T.Text, Char) -> T.Text -> T.Text
applyRule rule
    = let (word, ins) = rule
          newWord     = T.intersperse ins word in
    T.replace word newWord

doInserts :: T.Text -> T.Text
doInserts str = foldl (flip applyRule) str insertionRules
