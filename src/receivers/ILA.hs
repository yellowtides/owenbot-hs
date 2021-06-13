{-# LANGUAGE OverloadedStrings #-}

module ILA ( receivers ) where

import           Discord.Types      ( Message ( messageChannel ) )
import           Discord            ( DiscordHandler )
import qualified Data.Text as T
import           Data.Bifunctor     ( first )
import           Data.Char          ( toLower )

import           Utils              ( sendAssetChan
                                    , newCommand
                                    , assetDir
                                    )
import           TemplateRE         ( oneDot
                                    , twoDot
                                    , trailingWS
                                    , thmRE
                                    , defRE
                                    , lemRE
                                    , textbookRE
                                    )

receivers :: [Message -> DiscordHandler ()]
receivers =
    [ sendThm
    , sendDef
    , sendLem
    , sendTextbook
    ]

ilathmRE, iladefRE, ilalemmaRE, ilatextbookRE :: T.Text
ilathmRE      = thmRE      <> "ila (" <> twoDot <> ")"
iladefRE      = defRE      <> "ila (" <> oneDot <> ")"
ilalemmaRE    = lemRE      <> "ila (" <> twoDot <> ")"
ilatextbookRE = textbookRE <> "ila"

sendAsset :: Message -> T.Text -> String -> DiscordHandler ()
sendAsset m regex name = newCommand m (regex <> trailingWS)
        $ \(_:assetNr:_) -> do
    let assetNr' = parseAssetNr assetNr
    let name'    = T.pack $ name <> " " <> assetNr'
    let path     = "ila/" <> map toLower name <> "s/" <> assetNr'
    sendAssetChan (messageChannel m) name' path

sendThm, sendDef, sendLem :: Message -> DiscordHandler ()
sendThm m = sendAsset m ilathmRE   "Theorem"
sendDef m = sendAsset m iladefRE   "Definition"
sendLem m = sendAsset m ilalemmaRE "Lemma"

sendTextbook :: Message -> DiscordHandler ()
sendTextbook m = newCommand m ilatextbookRE $ \_ ->
    sendAssetChan (messageChannel m) "ila-textbook.pdf"
                        "textbooks/ila-textbook.pdf"

parseAssetNr :: T.Text -> String
parseAssetNr = T.unpack . (<> ".png") . uncurry (<>) . first (padZeroes 2)
        . T.breakOn "." .  T.intercalate "." . map rmZeroes . T.splitOn "."

rmZeroes :: T.Text -> T.Text
rmZeroes digits = case T.dropWhile (== '0') digits of
    "" -> "0"
    d' -> d'

padZeroes :: Int -> T.Text -> T.Text
padZeroes newLen digits = T.replicate (newLen - T.length digits) "0" <> digits
