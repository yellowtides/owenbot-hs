{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord

import EventHandler (handleEvent)

-- | UWU
owen :: T.Text -> IO ()
owen t = do
    
    userFacingError <- runDiscord $ def { discordToken   = t
                                        , discordOnEvent = handleEvent
                                        , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn "" }
    TIO.putStrLn userFacingError

main :: IO ()
main = do
    putStrLn "starting Owen"
    tok <- TIO.readFile ".token.txt"  
    putStrLn ("Token:" ++ T.unpack tok)
    owen tok   
    
    
