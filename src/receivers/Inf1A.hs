{-# LANGUAGE OverloadedStrings #-}

module Inf1A ( receivers ) where

import           Discord.Types      ( ChannelId
                                    , Message ( messageChannel )
                                    )
import           Discord            ( DiscordHandler )
import qualified Data.Text as T
import           Utils              ( sendMessageChan
                                    , sendFileChan
                                    , newCommand
                                    , assetDir
                                    )
import           TemplateRE         ( textbookRE )

receivers :: [Message -> DiscordHandler ()]
receivers =
    [ sendBool
    , sendSyl
    , sendHDoc
    , sendTextbook
    ]

inf1atextbookRE :: T.Text
inf1atextbookRE = textbookRE <> "i(nf)?1a"

sendSyl :: Message -> DiscordHandler ()
sendSyl m = newCommand m "syl(logisms)?" $ \_ ->
    sendFileChan (messageChannel m) "id-smash-aristotle.png"
                                    $ assetDir <> "/syllogisms.png"

sendBool :: Message -> DiscordHandler ()
sendBool m = newCommand m "bool(ean)?" $ \_ ->
    sendFileChan (messageChannel m) "literally-satan.png"
                                    $ assetDir <> "cl/Bool.png"

sendHDoc :: Message -> DiscordHandler ()
sendHDoc m = newCommand m "doc ([a-z']+)" $ \captures ->
    sendMessageChan (messageChannel m) "not yet implemented :^)"

sendTextbook :: Message -> DiscordHandler ()
sendTextbook m = newCommand m inf1atextbookRE $ \_ ->
    sendFileChan (messageChannel m) "the-holy-bible-2.png"
                                    $ assetDir <> "textbooks/i1a-textbook.pdf"
