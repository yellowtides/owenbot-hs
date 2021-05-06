{-# LANGUAGE OverloadedStrings #-}

module Calc ( receivers ) where

import           Discord.Types      ( Message ( messageChannel ) )
import           Discord            ( DiscordHandler )
import qualified Data.Text as T
import           Utils              ( sendMessageChan
                                    , newCommand
                                    )
import           TemplateRE         ( textbookRE )

receivers :: [Message -> DiscordHandler ()]
receivers = [ sendTextbook ]

calctextbookRE :: T.Text
calctextbookRE = textbookRE <> "calc"

url :: T.Text
url = "http://gen.lib.rus.ec/book/index.php?md5=13ecb7a2ed943dcb4a302080d2d8e6ea"

sendTextbook :: Message -> DiscordHandler ()
sendTextbook m = newCommand m calctextbookRE $ \_ ->
    sendMessageChan (messageChannel m)
        $ "The textbook can be found here:\n" <> url
