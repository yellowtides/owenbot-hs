{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
module MonadDiscordMock where

import qualified Discord.Requests as R
import Command
import Test.HMock

deriving instance Show R.MessageTiming
deriving instance Show R.ReactionTiming

makeMockableWithOptions (MockableOptions { mockSuffix = "Mock", mockVerbose = False }) ''MonadDiscord
