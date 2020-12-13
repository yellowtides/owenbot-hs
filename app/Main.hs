module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord

import EventHandler

token :: IO String
token = readFile "../.token.txt"

-- | UWU
owen :: String -> IO ()
owen t = do
    userFacingError <- runDiscord $ def { discordToken   = T.pack t
                                        , discordOnEvent = handleEvent }
    TIO.putStrLn userFacingError

main :: IO ()
main = do
    putStrLn "starting Owen"
    t <- token
    putStrLn ("Token:" ++ t)
    owen t
    
