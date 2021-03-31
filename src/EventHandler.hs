module EventHandler ( handleEvent ) where

import           Discord.Types          ( Message ( messageAuthor )
                                        , ReactionInfo
                                        , Event ( MessageCreate
                                                , MessageReactionAdd
                                                , MessageReactionRemove
                                                )
                                        , User ( userIsBot )
                                        )
import           Discord                ( DiscordHandler )
import           Control.Monad          ( unless
                                        , void
                                        , forM_
                                        , mplus
                                        )
import           Control.Applicative    ( (<|>) )

import           Status            ( setStatusFromFile )
import qualified Admin
import qualified Misc
import qualified Calc
import qualified Helpme
import qualified ILA
import qualified Inf1A
import qualified HallOfFame
import qualified RoleSelfAssign
import qualified MCServer
import qualified QuoteSystem

import qualified AprilFools

messageReceivers :: [Message -> DiscordHandler ()]
messageReceivers = concat
     [ -- AprilFools.messageReceivers      -- the AprilFools message receivers MUST be first if active
     Admin.receivers
     , Misc.messageReceivers
     , Calc.receivers
     , Helpme.receivers
     , ILA.receivers
     , Inf1A.receivers
     , HallOfFame.messageReceivers
     , MCServer.receivers
     , QuoteSystem.receivers
     ]

reactionAddReceivers :: [ReactionInfo -> DiscordHandler ()]
reactionAddReceivers = concat
     [ -- AprilFools.reactionReceivers
     Misc.reactionReceivers
     , HallOfFame.reactionReceivers
     , RoleSelfAssign.reactionAddReceivers
     ]

reactionRemoveReceivers :: [ReactionInfo -> DiscordHandler()]
reactionRemoveReceivers = concat
     [ RoleSelfAssign.reactionRemReceivers
     ]

isFromBot :: Message -> Bool
isFromBot m = userIsBot (messageAuthor m)


handleEvent :: Event -> DiscordHandler ()
handleEvent event = case event of
     MessageCreate m ->
          unless (isFromBot m) $ forM_ messageReceivers ($ m) <|> pure ()
     MessageReactionAdd r ->
          forM_ reactionAddReceivers ($ r) <|> pure ()
     MessageReactionRemove r ->
          forM_ reactionRemoveReceivers ($ r) <|> pure ()
     _ -> pure ()
