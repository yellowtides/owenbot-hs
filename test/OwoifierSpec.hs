{-# LANGUAGE OverloadedStrings #-}

module OwoifierSpec ( spec ) where

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
import           Owoifier                   ( owoify
                                            , weakOwoify
                                            )

spec :: Spec
spec = do
    describe "Owoify operations" $ do
        it "asserts some common owoifies" $
            owoify "Hello, I am rolling." `shouldBe` "Hewwo, I am wowwing. owo"