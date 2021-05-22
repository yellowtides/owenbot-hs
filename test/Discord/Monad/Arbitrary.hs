{-# LANGUAGE FlexibleInstances #-} -- allow instance declaration of MonadDiscord
module Discord.Monad.Arbitrary
    () where

import           Test.QuickCheck.Monadic    ( PropertyM )
import           Discord.Monad

-- | Implements every single possible rest call as a wrapper function.
-- Convenient notation achieved using a point-free restCallAndHandle
instance MonadDiscord (PropertyM IO) where
    getChannel c = arbitrary

