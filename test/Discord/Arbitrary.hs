module Discord.Arbitrary where

import Data.Char
import Discord.Types
import Test.QuickCheck

instance Arbitrary Snowflake where
    arbitrary = do
        ints <- vectorOf 64 (choose (0, 9))
        let chars = fmap intToDigit ints
        pure $ read chars

