module EventHandler (handleEvent) where 

import Discord.Types
    ( Message(messageAuthor, messageText),
      Event(MessageCreate, MessageReactionAdd),
      User(userIsBot) )
import Discord ( DiscordHandler )

import Control.Monad (when, guard, unless)

import CommandHandler (handleCommand, isCommand)
import MiscHandler (handleOwoify, isOwoifiable,
                    handleNietzsche, isNietzsche)

import ReactHandler
    ( notInHallOfFameChannel,
      isHallOfFameEmote,
      isEligibleForHallOfFame,
      handleHallOfFame )

import System.Random (randomR, mkStdGen)
import Data.Hashable (hash)

isFromBot :: Message -> Bool
isFromBot m = userIsBot (messageAuthor m)

handleEvent :: Event -> DiscordHandler ()
handleEvent event = case event of
       MessageCreate m -> let content = messageText m in
                          unless (isFromBot m)
                          $ do
                              when (isCommand content)
                                   (handleCommand m >> pure ())
                              guard . not $ isCommand content

                              when (isNietzsche content)
                                   (handleNietzsche m >> pure())
                              guard . not $ isNietzsche content

                              let randStr = hash . show $ event
                              let seed    = mkStdGen randStr
                              let roll    = fst $ randomR ((1, 500) :: (Int, Int)) seed
                              when (isOwoifiable content && roll == 1)
                                   (handleOwoify  m >> pure ())
       MessageReactionAdd r -> when (isHallOfFameEmote r && notInHallOfFameChannel r)
                               $ do
                                    eligibleM <- isEligibleForHallOfFame r
                                    case eligibleM of
                                         Right eligible -> if
                                                             eligible
                                                           then
                                                                handleHallOfFame r >> pure ()
                                                           else
                                                                pure ()
                                         Left err -> pure ()
       _ -> pure ()
