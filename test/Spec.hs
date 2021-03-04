module Main where

import Test.QuickCheck                  ( quickCheck
                                        , Property
                                        , (==>)
                                        )
import Test.QuickCheck.Monadic          ( assert
                                        , monadicIO
                                        )
import TestCSV                          ( prop_readWriteCSV
                                        , prop_readWriteSingleColCSV
                                        , cleanupTempCSV
                                        )

main :: IO ()
main = do
    quickCheck prop_readWriteCSV
    quickCheck prop_readWriteSingleColCSV
    cleanupTempCSV
