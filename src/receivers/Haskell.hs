{-# LANGUAGE OverloadedStrings #-}

module Haskell (
    receivers
) where

import           Data.Maybe              ( fromMaybe )
import           Discord                 ( DiscordHandler
                                         )
import           Discord.Types ( Message ( messageChannel ) )
import           Pointfree               ( pointfree' )
import qualified Data.Text as T

import Owoifier                ( owoify )
import Utils                   ( sendMessageChan
                               , newCommand )

receivers =
    [ pointfree
    ]

codeblock :: String -> String
codeblock = ("`" ++) . (++ "`")

pointfree :: Message -> DiscordHandler ()
pointfree m = newCommand m "pf (.*)" $ \captures -> do
    case captures of
         [hs] -> sendMessageChan (messageChannel m) $ pf hs
         _    -> sendMessageChan (messageChannel m) $ owoify
                    "Usage: `:pf <haskell code>`"
    where pf = T.pack . codeblock . fromMaybe "" . pointfree' . T.unpack
