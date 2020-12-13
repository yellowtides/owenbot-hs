{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_, when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Regex.TDFA 
import Text.Regex.TDFA.Text () 

import Discord
import qualified Discord.Requests as R  
import Discord.Types

import UnliftIO (liftIO)
import UnliftIO.Concurrent

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: IO ()
pingpongExample = do 
        t <- token
        userFacingError <- runDiscord $ def
                                            { discordToken = T.pack t
                                            , discordOnEvent = eventHandler }
        TIO.putStrLn userFacingError

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
       MessageCreate m -> when (not (fromBot m) && isCall (messageText m)) $ do
             _ <- commands m  --send messages with  _ <- restCall (R.CreateMessage (messageChannel m) ("Pong!"))
             pure ()
       _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

commands :: Message -> DiscordHandler (Either RestCallErrorCode Message)
commands m = checkReg (T.unpack $ messageText m) 
        where 
            checkReg x
                | ((x =~ thmRE) :: Bool) = test
                | ((x =~ defRE) :: Bool) = test
                | ((x =~ lemmaRE) :: Bool) = test
                | ((x =~ textbookRE) :: Bool) = test
                | ((x =~ syllogismsRE) :: Bool) = test
                | ((x =~ booleanRE) :: Bool) = test
                | ((x =~ hoogleInfRE) :: Bool) = test
                | ((x =~ helpRE) :: Bool) = test
            
            test = restCall (R.CreateMessage (messageChannel m) ("Message received"))

isCall :: T.Text -> Bool
isCall m = or $ map ( ((T.unpack m) =~) :: String -> Bool) regex --map through all regex's and see if any of them match

regex :: [String] --List of all regexs 
regex = [thmRE, defRE, lemmaRE, textbookRE, syllogismsRE, booleanRE, hoogleInfRE, helpRE]

-- individual regexs for each command
thmRE, defRE, lemmaRE, textbookRE, syllogismsRE, booleanRE, hoogleInfRE, helpRE :: String
thmRE = ":(thm|theorem) *([0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{1,2})" -- :thm
defRE = "^:(def|definition) *([0-9]{1,2}\\.[0-9]{1,2})"                    -- :def
lemmaRE = ":(lem|lemma) *([0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{1,2})"           -- :lemma
textbookRE = ":textbook *"                                                 -- :textbook
syllogismsRE = ":(syllogisms|syl) *"                                     -- :syllogisms
booleanRE = ":(boolean|bool) *"                                          -- :boolean
hoogleInfRE = "^:doc [a-z']+"                                                 -- :doc
helpRE = "^:helpme *"             

token :: IO String
token = do
  r <- readFile "../.token.txt"
  return r

main :: IO ()
main = do
  putStrLn "starting Owen"
  t <- token
  putStrLn ("Token:" ++ t)
  pingpongExample
    
