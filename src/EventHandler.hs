module EventHandler ( handleEvent ) where

import           Control.Applicative    ( (<|>) )
import           Control.Monad          ( unless )
import           Data.Foldable          ( for_ )
import qualified Data.Text as T         ( head )
import           Discord.Types
import           Discord

import qualified Admin
import qualified BinancePriceFetcher
import qualified Misc
import qualified Calc
import qualified Helpme
import qualified Haskell
import qualified ILA
import qualified Inf1A
import qualified HallOfFame
import qualified RoleSelfAssign
import qualified MCServer
import qualified QuoteSystem
import qualified ModifyEventsChannel
--import qualified AprilFools

commandReceivers :: [Message -> DiscordHandler ()]
commandReceivers = concat
     [  -- AprilFools.messageReceivers      -- the AprilFools message receivers MUST be first if active
       Admin.receivers
     , BinancePriceFetcher.receivers
     , Misc.commandReceivers
     , Misc.miscReceivers
     , Calc.receivers
     , Helpme.receivers
     , Haskell.receivers
     , ILA.receivers
     , Inf1A.receivers
     , HallOfFame.messageReceivers
     , MCServer.receivers
     , QuoteSystem.receivers
     , ModifyEventsChannel.receivers
     , RoleSelfAssign.receivers
     ]

reactionAddReceivers :: [ReactionInfo -> DiscordHandler ()]
reactionAddReceivers = concat
     [ -- AprilFools.reactionReceivers
       Misc.reactionReceivers
     , HallOfFame.reactionReceivers
     , RoleSelfAssign.reactionAddReceivers
     ]

{-# ANN reactionRemoveReceivers "HLint: ignore Evaluate" #-}
reactionRemoveReceivers :: [ReactionInfo -> DiscordHandler()]
reactionRemoveReceivers = concat
     [ RoleSelfAssign.reactionRemReceivers
     ]

isFromBot :: Message -> Bool
isFromBot m = userIsBot (messageAuthor m)

handleEvent :: Event -> DiscordHandler ()
handleEvent event = case event of
    MessageCreate m ->
        unless (isFromBot m) $ for_ commandReceivers ($ m)
    MessageReactionAdd r ->
        for_ reactionAddReceivers ($ r) <|> pure ()
    MessageReactionRemove r ->
        for_ reactionRemoveReceivers ($ r) <|> pure ()
    _ -> pure ()
