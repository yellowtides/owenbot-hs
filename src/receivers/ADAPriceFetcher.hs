{-# language OverloadedStrings, DeriveGeneric #-}

module ADAPriceFetcher (fetchADADetails) where

import GHC.Generics
import Data.Aeson
import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy as B

data ADAUSD24h = ADAUSD24h {
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

instance FromJSON ADAUSD24h
instance ToJSON ADAUSD24h

jsonURL :: String
jsonURL = "https://api.binance.com/api/v3/ticker/24hr?symbol=ADABUSD"

getJSON :: IO B.ByteString
getJSON = simpleHttp jsonURL

fetchADADetails :: IO (Either String String) 
fetchADADetails = do
    adaDeetsM <- (eitherDecode <$> getJSON) :: IO (Either String ADAUSD24h)
    pure $ case adaDeetsM of
        Left err       -> Left err
        Right adaDeets -> do
            let percentChangeDouble = read (priceChangePercent adaDeets) :: Double
            let currPriceDouble     = read (lastPrice adaDeets) :: Double
            let lowPriceDouble      = read (lowPrice adaDeets) :: Double
            let highPriceDouble     = read (highPrice adaDeets) :: Double
            let ann0 = "<:ada:805934431071371305> (philcoin) is "
            let ann1 = ann0 <> "**" <> (if percentChangeDouble < 0 then "down 💢" else "up 🚀🚀") <> "** "
            let ann2 = ann1 <> "**" <> show (abs percentChangeDouble) <> "%** in the past 24 hours, "
            let ann3 = ann2 <> "currently sitting at **$" <> show currPriceDouble <> "** per unit (₳).\n"
            let ann4 = ann3 <> "Lowest price in the past 24h: **$"  <> show lowPriceDouble <> "**.\n"
            let ann5 = ann4 <> "Highest price in the past 24h: **$" <> show highPriceDouble <> "**."
            Right ann5