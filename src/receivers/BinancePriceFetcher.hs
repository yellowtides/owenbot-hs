{-# language OverloadedStrings, DeriveGeneric #-}

module BinancePriceFetcher (fetchADADetails, fetchTicker, commands) where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T (pack, unpack)
import Discord (DiscordHandler)
import Discord.Types (Message, messageChannel)
import GHC.Generics
import Network.HTTP.Conduit (simpleHttp)
import Text.Regex.TDFA ((=~))
import UnliftIO (liftIO)

import Command
import Owoifier (owoify)

commands :: [Command DiscordHandler]
commands = [handleTicker, handleAda24h]

data Ticker = Ticker
    { symbol             :: String
    , priceChange        :: String
    , priceChangePercent :: String
    , weightedAvgPrice   :: String
    , prevClosePrice     :: String
    , lastPrice          :: String
    , lastQty            :: String
    , bidPrice           :: String
    , bidQty             :: String
    , askPrice           :: String
    , askQty             :: String
    , openPrice          :: String
    , highPrice          :: String
    , lowPrice           :: String
    , volume             :: String
    , quoteVolume        :: String
    , openTime           :: Integer
    , closeTime          :: Integer
    , firstId            :: Integer
    , lastId             :: Integer
    , count              :: Integer
    }
    deriving (Show, Generic)

instance FromJSON Ticker
instance ToJSON Ticker

adaEmoji :: String
adaEmoji = "<:ada:805934431071371305>"
-- TODO: allow server to choose emoji through :config

validCurrencyRegex :: String
validCurrencyRegex = "^[A-Z0-9-_.]{1,20}$"

jsonURL :: String -> String -> String
jsonURL base quote =
    "https://api.binance.com/api/v3/ticker/24hr?symbol=" <> base <> quote

sign :: String -> String
sign "BUSD"  = "$"
sign "TUSD"  = "$"
sign "USDT"  = "$"
sign "AUD"   = "$"
sign "CAD"   = "$"
sign "EUR"   = "€"
sign "GBP"   = "£"
sign "JPY"   = "¥"

sign "ADA"   = "₳"
sign "BCH"   = "Ƀ"
sign "BSV"   = "Ɓ"
sign "BTC"   = "₿"
sign "DAI"   = "◈"
sign "DOGE"  = "Ð"
sign "EOS"   = "ε"
sign "ETC"   = "ξ"
sign "ETH"   = "Ξ"
sign "LTC"   = "Ł"
sign "MKR"   = "Μ"
sign "REP"   = "Ɍ"
sign "STEEM" = "ȿ"
sign "XMR"   = "ɱ"
sign "XRP"   = "✕"
sign "XTZ"   = "ꜩ"
sign "ZEC"   = "ⓩ"

sign x       = x

getJSON :: String -> String -> IO B.ByteString
getJSON a b = simpleHttp $ jsonURL a b

fetchADADetails :: IO (Either String String)
fetchADADetails = do
    ticker <- fetchTicker "ADA" "BUSD"
    pure $ case ticker of
        Left  err -> Left err
        Right str -> Right $ adaEmoji ++ " (philcoin) is " ++ str

fetchTicker :: String -> String -> IO (Either String String)
fetchTicker base quote = do
    detailsM <- (eitherDecode <$> getJSON base quote) :: IO (Either String Ticker)
    pure $ case detailsM of
        Left  err     -> Left err
        Right details -> do
            let percentChangeD = read (priceChangePercent details) :: Double
                curPriceD      = read (lastPrice details) :: Double
                lowPriceD      = read (lowPrice details) :: Double
                highPriceD     = read (highPrice details) :: Double
            Right $ tickerAnnounce
                base
                quote
                percentChangeD
                curPriceD
                lowPriceD
                highPriceD

tickerAnnounce :: String -> String -> Double -> Double -> Double -> Double -> String
tickerAnnounce base quote percentChange curPrice lowPrice highPrice = concat
    [ "**"
    , if percentChange < 0 then "down 💢" else "up 🚀🚀"
    , "** "
    , "**"
    , show (abs percentChange)
    , "%** in the past 24 hours, "
    , "currently sitting at **"
    , sign base
    , "1** = **"
    , sign quote
    , show curPrice
    , "** per unit.\n"
    , "Lowest price in the past 24h: **"
    , sign quote
    , show lowPrice
    , "**.\n"
    , "Highest price in the past 24h: **"
    , sign quote
    , show highPrice
    , "**."
    ]


handleTicker :: Command DiscordHandler
handleTicker =
    help "Usage: `:binance <ticker> <ticker>`"
    . command "binance" $ \m base quote -> do
        case base =~ validCurrencyRegex && quote =~ validCurrencyRegex of
             False -> respond m $ owoify "One of your currencies was invalid!"
             True  -> do
                announcementM <- liftIO $ fetchTicker base quote
                case announcementM of
                    Left _ -> respond m $ owoify "Couldn't get the data! Sorry!"
                    Right ann -> respond m $ owoify . T.pack
                            $  base <> "/" <> quote <> " is " <> ann

handleAda24h :: Command DiscordHandler
handleAda24h =
    alias "ada24h"
    . help "Print current philcoin status."
    . command "ada" $ \m -> do
        adaAnnouncementM <- liftIO fetchADADetails
        case adaAnnouncementM of
            Left err -> do
                liftIO
                    $  putStrLn
                    $  "[DEBUG] Cannot fetch ADA details from Binance: "
                    ++ err
                respond m $ owoify "Couldn't get the data! Sorry!"
            Right announcement -> respond m $ owoify $ T.pack announcement
