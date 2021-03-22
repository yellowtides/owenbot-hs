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
            owoify "Laughter Rolling no more MORE NOW nOT a mOMENT." `shouldBe`
                "Waughtew Wowwing nyo myowe MYOWE NYOW nyOT a myOMENT. owo"
        it "checks weakOwoify doesn't change the length" $
            property prop_weakNoChangeLength
        it "checks weakOwoify against a complex string" $
            weakOwoify "Laughter Rolling no more MORE NOW nOT a mOMENT." `shouldBe`
                "Waughtew Wowwing no mowe MOWE NOW nOT a mOMENT."

prop_owoifyEndsWithOwo :: T.Text -> Bool
prop_owoifyEndsWithOwo str = T.takeEnd 4 (owoify str) == " owo" 

prop_weakNoChangeLength :: T.Text -> Bool
prop_weakNoChangeLength str = T.length (weakOwoify str) == T.length str