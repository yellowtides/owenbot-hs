{-# language OverloadedStrings, DeriveGeneric #-}

module ADAPriceFetcher ( fetchADADetails
                       , fetchTicker
                       ) where

import           GHC.Generics
import           Data.Aeson
import           Network.HTTP.Conduit   ( simpleHttp )
import qualified Data.ByteString.Lazy as B

data Ticker = Ticker {
    symbol              :: String,
    priceChange         :: String,
    priceChangePercent  :: String,
    weightedAvgPrice    :: String,
    prevClosePrice      :: String,
    lastPrice           :: String,
    lastQty             :: String,
    bidPrice            :: String,
    bidQty              :: String,
    askPrice            :: String,
    askQty              :: String,
    openPrice           :: String,
    highPrice           :: String,
    lowPrice            :: String,
    volume              :: String,
    quoteVolume         :: String,
    openTime            :: Integer,
    closeTime           :: Integer,
    firstId             :: Integer,
    lastId              :: Integer,
    count               :: Integer
} deriving (Show, Generic)

instance FromJSON Ticker
instance ToJSON Ticker

jsonURL :: String -> String -> String
jsonURL base quote = "https://api.binance.com/api/v3/ticker/24hr?symbol=" <> base <> quote

getJSON :: String -> String -> IO B.ByteString
getJSON a b = simpleHttp $ jsonURL a b

fetchADADetails :: IO (Either String String)
fetchADADetails = do
    ticker <- fetchTicker "ADA" "BUSD"
    pure $ case ticker of
        Left  err -> Left err
        Right str -> Right $ "<:ada:805934431071371305> (philcoin) is " ++ str

fetchTicker :: String -> String -> IO (Either String String)
fetchTicker base quote = do
    detailsM <- (eitherDecode <$> getJSON base quote) :: IO (Either String Ticker)
    pure $ case detailsM of
        Left err       -> Left err
        Right details -> do
            let percentChangeD = read (priceChangePercent details) :: Double
            let curPriceD      = read (lastPrice          details) :: Double
            let lowPriceD      = read (lowPrice           details) :: Double
            let highPriceD     = read (highPrice          details) :: Double
            Right $ tickerAnnounce base quote percentChangeD curPriceD lowPriceD highPriceD

tickerAnnounce :: String -> String -> Double -> Double -> Double -> Double -> String
tickerAnnounce base quote percentChange curPrice lowPrice highPrice = concat [
      "**", if percentChange < 0 then "down 💢" else "up 🚀🚀", "** "
    , "**", show (abs percentChange), "%** in the past 24 hours, "
    , "currently sitting at **", sign base, "1** = **"
    , sign quote, show curPrice, "** per unit.\n"

    , "Lowest price in the past 24h: **", sign quote, show lowPrice, "**.\n"

    , "Highest price in the past 24h: **", sign quote, show highPrice, "**."
    ]

sign :: String -> String
sign "BUSD" = "$"
sign "TUSD" = "$"
sign "USDT" = "$"
sign "AUD"  = "$"
sign "EUR"  = "€"
sign "GBP"  = "£"
sign "JPY"  = "¥"

sign "ADA"  = "₳"
sign "BTC"  = "₿"
sign "ETH"  = "Ξ"

sign x      = x
