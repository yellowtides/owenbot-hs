module EventHandler (handleEvent) where 

import Discord.Types( Message(messageAuthor, messageText),
                      ReactionInfo(reactionEmoji, reactionUserId, reactionMessageId),
                      Event(MessageCreate, MessageReactionAdd, MessageReactionRemove),
                      User(userIsBot) )
import Discord ( DiscordHandler, RestCallErrorCode )
import Data.Maybe ( isJust, fromJust, isNothing )
import Control.Monad (when, guard, unless, void )
import System.Random ( randomR, getStdRandom )
import UnliftIO ( liftIO )
import qualified Data.Text as T ( length )

import qualified Admin
import CommandHandler (handleCommand, isCommand)
import MiscHandler (handleOwoify, isOwoifiable,
                    handleNietzsche, isNietzsche,
                    handleThatcher, isThatcher,
                    handleDadJoke, isDadJoke,
                    handleFortune, isFortune,
                    handleADA, isADA)

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

messageReceivers :: [Message -> DiscordHandler ()]
messageReceivers = concat [Admin.receivers]

reactionReceivers :: [ReactionInfo -> DiscordHandler ()]
reactionReceivers = concat []

isFromBot :: Message -> Bool
isFromBot m = userIsBot (messageAuthor m)

roll :: Int -> IO Int
roll n = getStdRandom $ randomR (1, n)

handleEvent :: Event -> DiscordHandler ()
handleEvent event = case event of
       MessageCreate m -> let content = messageText m
                          in
                              unless (isFromBot m) $ void (mapM ($ m) messageReceivers)

                         --  $ (do 
                         --      when (isCommand content)
                         --           (handleCommand m >> pure ())
                         --      guard . not $ isCommand content

                         --      when (isNietzsche content)
                         --           (handleNietzsche m >> pure ())
                         --      guard . not $ isNietzsche content
                              
                         --      when (isThatcher content)
                         --           (handleThatcher m >> pure ())
                         --      guard . not $ isThatcher content

                              -- when (isADA content)
                              --      (handleADA m >> pure ())
                              -- guard . not $ isADA content

                              -- when (isFortune content)
                              --      (handleFortune m >> pure ())
                              -- guard . not $ isFortune content

                         --      roll10 <- liftIO $ roll 20
                         --      let isDadJokeM = isDadJoke content
                         --      when (isJust isDadJokeM && roll10 == 1
                         --            && (T.length (fromJust isDadJokeM) >= 3))
                         --           (handleDadJoke m (fromJust isDadJokeM) >> pure ())

                              -- roll500 <- liftIO $ roll 500
                              -- when (isOwoifiable content && roll500 == 1)
                              --      (handleOwoify  m >> pure ())
                              -- ) `mplus` pure ()
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
