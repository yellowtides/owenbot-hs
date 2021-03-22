{-# LANGUAGE OverloadedStrings #-}

module OwoifierSpec ( spec ) where

import qualified Data.Text as T
import           Test.Hspec
import           Test.QuickCheck            ( property )
import           Test.QuickCheck.Instances.Text
import           Owoifier                   ( owoify
                                            , weakOwoify
                                            )

spec :: Spec
spec = do
    describe "Owoify operations" $ do
        it "checks owoify ends with owo" $
            property prop_owoifyEndsWithOwo
        it "checks owoify against a complex string" $
            owoify "Laughter Rolling no more NO MORE nO mORE No More." `shouldBe`
                "Waughtew Wowwing nyo myowe NYO MYOWE nyO myOWE Nyo Myowe. owo"
    describe "Owoify (weak ver.) operations" $ do
        it "checks weakOwoify doesn't change the length" $
            property prop_weakNoChangeLength
        it "checks weakOwoify against a complex string" $
            weakOwoify "Laughter Rolling no more NO MORE nO mORE No More." `shouldBe`
                "Waughtew Wowwing no mowe NO MOWE nO mOWE No Mowe."

prop_owoifyEndsWithOwo :: T.Text -> Bool
prop_owoifyEndsWithOwo str = T.takeEnd 4 (owoify str) == " owo" 

prop_weakNoChangeLength :: T.Text -> Bool
prop_weakNoChangeLength str = T.length (weakOwoify str) == T.length str
