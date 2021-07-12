module EventHandler ( handleEvent ) where

import           Control.Applicative    ( (<|>) )
import           Control.Monad          ( unless )
import           Data.Foldable          ( for_ )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Discord.Types
import           Discord
import           UnliftIO               ( liftIO )

import qualified Academic
import qualified Admin
import qualified BinancePriceFetcher
import qualified Misc
import qualified Haskell
import qualified HallOfFame
import qualified RoleSelfAssign
import qualified MCServer
import qualified QuoteSystem
import qualified ModifyEventsChannel
--import qualified AprilFools

import           Command
import           Owoifier               ( owoify )
import           Utils                  ( sendMessageDM )

commands :: [Command DiscordHandler]
commands = concat
     [ Admin.commands
     , Academic.commands
     , BinancePriceFetcher.commands
     , Misc.commands
     , Haskell.commands
     , HallOfFame.commands
     , MCServer.commands
     , QuoteSystem.commands
     , ModifyEventsChannel.commands
     , RoleSelfAssign.commands
     ]

-- | This command executes the handler if there are no arguments. If there are
-- arguments, it replies with the help message set in each command.
generatedHelp :: Command DiscordHandler
generatedHelp = helpCommand (T.pack "helpme") commands $ \m ->
    liftIO (TIO.readFile "./src/assets/help.txt")
        >>= sendMessageDM (userId $ messageAuthor m) . owoify

{-# ANN messageReceivers "HLint: ignore Evaluate" #-}
messageReceivers :: [Message -> DiscordHandler ()]
messageReceivers = concat []
    -- [ AprilFools.messageReceivers ]

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
        unless (isFromBot m) $ do
            for_ messageReceivers ($ m) <|> pure ()
            runCommands (generatedHelp : commands) m
    MessageReactionAdd r ->
        for_ reactionAddReceivers ($ r) <|> pure ()
    MessageReactionRemove r ->
        for_ reactionRemoveReceivers ($ r) <|> pure ()
    _ -> pure ()
