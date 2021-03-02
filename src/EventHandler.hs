module EventHandler (handleEvent) where 

import Discord.Types( Message(messageAuthor, messageText),
                      ReactionInfo(reactionEmoji, reactionUserId, reactionMessageId),
                      Event(MessageCreate, MessageReactionAdd, MessageReactionRemove),
                      User(userIsBot) )
import Discord ( DiscordHandler, RestCallErrorCode )
import Data.Maybe ( isJust, fromJust, isNothing )
import Control.Monad (when, guard, unless, void )
import UnliftIO ( liftIO )
import qualified Data.Text as T ( length )

import qualified Admin
import qualified MiscHandler
import CommandHandler (handleCommand, isCommand)

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
messageReceivers = concat [Admin.receivers, MiscHandler.receivers]

reactionReceivers :: [ReactionInfo -> DiscordHandler ()]
reactionReceivers = concat []

isFromBot :: Message -> Bool
isFromBot m = userIsBot (messageAuthor m)


handleEvent :: Event -> DiscordHandler ()
handleEvent event = case event of
       MessageCreate m -> let content = messageText m
                          in
                              unless (isFromBot m) $ void (mapM ($ m) messageReceivers)
                              
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
