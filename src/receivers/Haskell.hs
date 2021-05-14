{-# LANGUAGE OverloadedStrings #-}

module Haskell (
    receivers
) where

import           Data.Maybe              ( fromMaybe )
import           Discord                 ( DiscordHandler
                                         )
import           Discord.Types           ( Message )
import           Pointfree               ( pointfree' )
import qualified Data.Text as T

import           Utils                   ( respond
                                         , newCommand )

receivers =
    [ pointfree
    ]

-- | Surrounds a String with backticks for nice formatting on Discord
codeblock :: String -> String
codeblock = ("``" ++) . (++ "``")

-- | Converts given haskell code to point-free form
-- >>> :pf f x = 1 + x
-- f = (1 +)
--
-- >>> :pf codeblock str = "`" ++ str ++ "`"
-- codeblock = ("``" ++) . (++ "``")
pointfree :: Message -> DiscordHandler ()
pointfree m = newCommand m "pf (.*)"
        $ \captures -> respond m $ pf $ head captures
    where pf = T.pack . codeblock . fromMaybe "" . pointfree' . T.unpack
