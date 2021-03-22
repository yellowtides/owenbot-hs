module CSVSpec ( spec ) where

import qualified Data.Text as T
import           Test.Hspec
import           Test.QuickCheck            ( Property
                                            , property
                                            , (==>)
                                            )
import           Test.QuickCheck.Monadic    ( assert
                                            , monadicIO
                                            , run
                                            )
import           System.Directory           ( removeFile )
import           CSV                        ( configDir
                                            , readCSV
                                            , readSingleColCSV
                                            , writeCSV
                                            , writeSingleColCSV
                                            )

spec :: Spec
spec = do
    describe "CSV Operations" $ afterAll_ cleanupTempCSV $ do
        it "checks if read/write operations work" $
            property prop_readWriteCSV
        it "checks if single-column read/write operations work" $
            property prop_readWriteSingleColCSV

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
cleanupTempCSV = configDir >>= \x -> removeFile $ x <> "temp.csv"
