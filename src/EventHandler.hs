module EventHandler (handleEvent) where 

import Discord.Types
    ( Message(messageAuthor, messageText),
      Event(Ready, MessageCreate, MessageReactionAdd, MessageReactionRemove),
      User(userIsBot) )
import Discord ( DiscordHandler )
import Data.Maybe ( isJust, fromJust, isNothing )
import Control.Monad (when, guard, unless, mplus)
import System.Random ( randomR, getStdRandom )
import UnliftIO ( liftIO )
import qualified Data.Text as T ( length )

import CommandHandler (handleCommand, isCommand)
import MiscHandler (handleOwoify, isOwoifiable,
                    handleNietzsche, isNietzsche,
                    handleThatcher, isThatcher,
                    handleDadJoke, isDadJoke,
                    handleFortune, isFortune,
                    handleMovie, isMovie )

import ReactHandler
    ( notInHallOfFameChannel,
      isHallOfFameEmote,
      isEligibleForHallOfFame,
      handleHallOfFame )

import RoleSelfAssign 
     ( handleRoleAssign,
       handleRoleRemove,
       isOnAssignMessage )
import Status (setStatusFromFile)

isFromBot :: Message -> Bool
isFromBot m = userIsBot (messageAuthor m)

roll :: Int -> IO Int
roll n = getStdRandom $ randomR (1, n)

handleEvent :: Event -> DiscordHandler ()
handleEvent event = case event of
       Ready i u ch ungu sess_id -> setStatusFromFile >> pure ()
       MessageCreate m -> let content = messageText m in
                          unless (isFromBot m)
                          $ (do 
                              when (isCommand content)
                                   (handleCommand m >> pure ())
                              guard . not $ isCommand content

                              when (isNietzsche content)
                                   (handleNietzsche m >> pure ())
                              guard . not $ isNietzsche content
                              
                              when (isThatcher content)
                                   (handleThatcher m >> pure ())
                              guard . not $ isThatcher content

                              when (isMovie content)
                                   (handleMovie m >> pure ())
                              guard . not $ isMovie content

                              when (isFortune content)
                                   (handleFortune m >> pure ())
                              guard . not $ isFortune content

                              roll10 <- liftIO $ roll 20
                              let isDadJokeM = isDadJoke content
                              when (isJust isDadJokeM && roll10 == 1
                                    && (T.length (fromJust isDadJokeM) >= 3))
                                   (handleDadJoke m (fromJust isDadJokeM) >> pure ())

                              roll500 <- liftIO $ roll 500
                              when (isOwoifiable content && roll500 == 1)
                                   (handleOwoify  m >> pure ())
                              ) `mplus` pure ()
       MessageReactionAdd r -> do
                                   isSelfAssign <- liftIO $ isOnAssignMessage r
                                   when isSelfAssign
                                        (handleRoleAssign r >> pure ())
                                   guard . not $ isSelfAssign

                                   when (isHallOfFameEmote r && notInHallOfFameChannel r)
                                        (do
                                             eligibleM <- isEligibleForHallOfFame r
                                             case eligibleM of
                                                  Right eligible -> if
                                                                      eligible
                                                                 then
                                                                      handleHallOfFame r >> pure ()
                                                                 else
                                                                      pure ()
                                                  Left err -> pure ())
       MessageReactionRemove r -> handleRoleRemove r >> pure ()
       _ -> pure ()
