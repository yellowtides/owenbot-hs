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
import           Discord.Types          ( messageText )
import           Data.Foldable          ( for_ )
import           Control.Monad          ( unless
                                        , forM_
                                        )
import qualified Data.Text as T         ( head )

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

commandReceivers :: [Message -> DiscordHandler ()]
commandReceivers = concat
     [  -- AprilFools.messageReceivers      -- the AprilFools message receivers MUST be first if active
       Admin.receivers
     , Misc.commandReceivers
     , Calc.receivers
     , Helpme.receivers
     , ILA.receivers
     , Inf1A.receivers
     , HallOfFame.messageReceivers
     , MCServer.receivers
     , QuoteSystem.receivers
     ]

messageReceivers :: [Message -> DiscordHandler ()]
messageReceivers = concat
    [ Misc.miscReceivers
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
          unless (isFromBot m) $ if T.head (messageText m) == ':'
            then for_ commandReceivers ($ m)
            else for_ messageReceivers ($ m)
     MessageReactionAdd r ->
          for_ reactionAddReceivers ($ r)
     MessageReactionRemove r ->
          for_ reactionRemoveReceivers ($ r)
     _ -> pure ()
