module TestCSV ( prop_readWriteCSV
               , prop_readWriteSingleColCSV
               , cleanupTempCSV
               ) where

import qualified Data.Text as T
import           Test.QuickCheck            ( quickCheck
                                            , Property
                                            , (==>)
                                            )
import           Test.QuickCheck.Monadic    ( assert
                                            , monadicIO
                                            , run
                                            )
import           System.Directory           ( removeFile )
import           CSV                        ( readCSV
                                            , readSingleColCSV
                                            , writeCSV
                                            , writeSingleColCSV
                                            )

prop_readWriteCSV :: [[Char]] -> Int -> Property
prop_readWriteCSV line n = not (null line) ==> monadicIO $ do
    let writeData = replicate n (fmap T.pack line)
    run $ writeCSV "temp.csv" writeData
    readData <- run $ readCSV "temp.csv"
    assert $ writeData == readData 

prop_readWriteSingleColCSV :: [[Char]] -> Property
prop_readWriteSingleColCSV line = monadicIO $ do
    let writeData = fmap T.pack line
    run $ writeSingleColCSV "temp.csv" writeData
    readData <- run $ readSingleColCSV "temp.csv"
    assert $ writeData == readData

cleanupTempCSV :: IO ()
cleanupTempCSV = removeFile ".owen/temp.csv"