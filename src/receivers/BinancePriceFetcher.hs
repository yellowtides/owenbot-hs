{-# language OverloadedStrings, DeriveGeneric #-}

module BinancePriceFetcher ( fetchADADetails
                           , fetchTicker
                           ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           GHC.Generics
import           Network.HTTP.Conduit   ( simpleHttp )
import qualified Data.Text as T         ( pack
                                        , unpack )
import           Discord                ( DiscordHandler )
import           Discord.Types          ( Message
                                        , messageChannel )
import           UnliftIO               ( liftIO )

import           Utils                  ( newCommand
                                        , sendMessageChan )
import           Owoifier               ( owoify )

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

adaEmoji :: String
adaEmoji = "<:ada:805934431071371305>"
-- TODO: allow server to choose emoji through :config

jsonURL :: String -> String -> String
jsonURL base quote = "https://api.binance.com/api/v3/ticker/24hr?symbol=" <> base <> quote

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


handleTicker :: Message -> DiscordHandler ()
handleTicker m = newCommand m "binance ([A-Z]+) ([A-Z]+)" $ \symbol -> do
    let [base, quote] = T.unpack <$> symbol
    announcementM <- liftIO $ fetchTicker base quote
    case announcementM of
         Left err -> do
            liftIO (putStrLn $ "Cannot get ticker from binance: " ++ err)
            sendMessageChan (messageChannel m)
                $ owoify "Couldn't get the data! Sorry"
         Right announcement ->
            sendMessageChan (messageChannel m)
                $ owoify . T.pack $ base <> "/" <> quote <> " is "
                                 <> announcement

handleAda24h :: Message -> DiscordHandler ()
handleAda24h m = newCommand m "ada24h" $ \_ -> do
    adaAnnouncementM <- liftIO fetchADADetails
    case adaAnnouncementM of
        Left err -> do
            liftIO (putStrLn $ "Cannot fetch ADA details from Binance: " ++ err)
            sendMessageChan (messageChannel m)
                $ owoify "Couldn't get the data! Sorry"
        Right announcement ->
            sendMessageChan (messageChannel m)
                $ owoify $ T.pack announcement
