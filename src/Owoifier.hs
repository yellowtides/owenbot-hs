{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Owoifier ( owoify
                , weakOwoify
                ) where

import qualified Data.Text as T

owoify :: T.Text -> T.Text
owoify = (<> " owo") . doInserts . weakOwoify

weakOwoify :: T.Text -> T.Text
weakOwoify = T.map owoifyChar

-- returns ['l'/'L'/'r'/'R'] for ['w'/'W'], otherwise `id`
owoifyChar :: Char -> Char
owoifyChar c
    | c `elem` ("LR" :: [Char]) = 'W'
    | c `elem` ("lr" :: [Char]) = 'w'
    | otherwise     = c

cartesianConcat :: [Char] -> [Char] -> [T.Text]
cartesianConcat half1 half2 = T.cons <$> half1
                                     <*> (T.singleton <$> half2)

-- ['NMnm' <-> 'o']
-- ['nm'   <-> 'O']
insertablesSmallY :: [T.Text]
insertablesSmallY = cartesianConcat "NMnm" "o" ++ cartesianConcat "nm" "O"

-- ['NM' <-> 'O']
insertablesBigY :: [T.Text]
insertablesBigY = cartesianConcat "NM" "O"

insertionRules :: [(T.Text, Char)]
insertionRules = concat
    [ (, 'y') <$> insertablesSmallY
    , (, 'Y') <$> insertablesBigY
    ]

applyRule :: (T.Text, Char) -> T.Text -> T.Text
applyRule rule text = do
    let (word, ins) = rule
    let newWord     = T.intersperse ins word
    T.replace word newWord text

doInserts :: T.Text -> T.Text
doInserts str = foldl (flip applyRule) str insertionRules