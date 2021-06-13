{-# LANGUAGE OverloadedStrings #-}

module Inf1A ( receivers ) where

import           Discord.Types      ( Message ( messageChannel ) )
import           Discord            ( DiscordHandler )
import qualified Data.Text as T
import           Utils              ( sendMessageChan
                                    , sendAssetChan
                                    , newCommand
                                    , assetDir
                                    )
import           TemplateRE         ( textbookRE )

receivers :: [Message -> DiscordHandler ()]
receivers =
    [ sendBool
    , sendSyl
    , sendTextbook
    ]

inf1atextbookRE :: T.Text
inf1atextbookRE = textbookRE <> "i(nf)?1a"

sendAsset :: Message -> T.Text -> T.Text -> FilePath -> DiscordHandler ()
sendAsset m regex filename path = newCommand m regex $ \_ ->
    sendAssetChan (messageChannel m) filename path

sendSyl, sendBool, sendTextbook :: Message -> DiscordHandler ()
sendSyl      m = sendAsset m "syl(logisms)?" "id-smash-aristotle.png" "cl/syllogisms.png"
sendBool     m = sendAsset m "bool(ean)?"    "literally-satan.png"    "cl/Bool.png"
sendTextbook m = sendAsset m inf1atextbookRE "the-holy-bible-2.png"   "textbooks/i1a-textbook.pdf"
