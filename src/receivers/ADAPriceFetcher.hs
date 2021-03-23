{-# language OverloadedStrings, DeriveGeneric #-}

module ADAPriceFetcher (fetchADADetails) where

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
    adaDeetsM <- (eitherDecode <$> getJSON "ADA" "BUSD") :: IO (Either String Ticker)
    pure $ case adaDeetsM of
        Left err       -> Left err
        Right adaDeets -> do
            let percentChangeDouble = read (priceChangePercent adaDeets) :: Double
            let curPriceDouble      = read (lastPrice adaDeets) :: Double
            let lowPriceDouble      = read (lowPrice adaDeets) :: Double
            let highPriceDouble     = read (highPrice adaDeets) :: Double
            let adaAnnouncement = concat [
                                    "<:ada:805934431071371305> (philcoin) is "
                                    , "**", if percentChangeDouble < 0 then "down 💢" else "up 🚀🚀", "** "
                                    , "**", show (abs percentChangeDouble), "%** in the past 24 hours, "
                                    , "currently sitting at **$", show curPriceDouble, "** per unit (₳).\n"

                                    , "Lowest price in the past 24h: **$", show lowPriceDouble, "**.\n"

                                    , "Highest price in the past 24h: **$", show highPriceDouble, "**."
                                  ]
            Right adaAnnouncement
