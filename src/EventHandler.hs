module EventHandler (handleEvent) where 

import           Discord.Types     ( Message ( messageAuthor )
                                   , ReactionInfo
                                   , Event ( MessageCreate
                                           , MessageReactionAdd
                                           , MessageReactionRemove
                                           )
                                   , User ( userIsBot )
                                   )
import           Discord           ( DiscordHandler )
import           Control.Monad     ( when
                                   , unless
                                   , void )

import qualified Admin
import qualified Misc
import qualified Calc
import qualified Helpme
import qualified ILA
import qualified Inf1A
import qualified HallOfFame
import qualified RoleSelfAssign

import Status (setStatusFromFile)

messageReceivers :: [Message -> DiscordHandler ()]
messageReceivers = concat
     [ Admin.receivers
     , Misc.receivers
     , Calc.receivers
     , Helpme.receivers
     , ILA.receivers
     , Inf1A.receivers
     , HallOfFame.messageReceivers
     ]

reactionAddReceivers :: [ReactionInfo -> DiscordHandler ()]
reactionAddReceivers = concat 
     [ HallOfFame.reactionReceivers
     , RoleSelfAssign.reactionAddReceivers
     ]

reactionRemoveReceivers :: [ReactionInfo -> DiscordHandler()]
reactionRemoveReceivers = concat
     [ RoleSelfAssign.reactionRemReceivers ]

isFromBot :: Message -> Bool
isFromBot m = userIsBot (messageAuthor m)


handleEvent :: Event -> DiscordHandler ()
handleEvent event = case event of
     MessageCreate m -> unless (isFromBot m) $ void (mapM ($ m) messageReceivers)
     MessageReactionAdd r -> void (mapM ($ r) reactionAddReceivers)
     MessageReactionRemove r -> void (mapM ($ r) reactionRemoveReceivers)
     _ -> pure ()
