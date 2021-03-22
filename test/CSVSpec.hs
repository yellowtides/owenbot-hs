module CSVSpec ( spec ) where

import qualified Data.Text as T
import           Test.Hspec
import           Test.Hspec.QuickCheck      ( modifyMaxSuccess )
import           Test.QuickCheck            ( Property
                                            , property
                                            , (==>)
                                            )
import           Test.QuickCheck.Instances.Text
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
        modifyMaxSuccess (const 50) $ -- not be so hard on system >.<
            it "checks if read/write operations work" $
                property prop_readWriteCSV
        it "checks if single-column read/write operations work" $
            property prop_readWriteSingleColCSV

prop_readWriteCSV :: [T.Text] -> Int -> Property
prop_readWriteCSV line n = not (null line) ==> monadicIO $ do
    let writeData = replicate n line
    run $ writeCSV "temp.csv" writeData
    readData <- run $ readCSV "temp.csv"
    assert $ writeData == readData 

prop_readWriteSingleColCSV :: [T.Text] -> Property
prop_readWriteSingleColCSV line = monadicIO $ do
    run $ writeSingleColCSV "temp.csv" line
    readData <- run $ readSingleColCSV "temp.csv"
    assert $ line == readData

cleanupTempCSV :: IO ()
cleanupTempCSV = configDir >>= \x -> removeFile $ x <> "temp.csv"
