module EventHandler (handleEvent) where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.Foldable (for_)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Discord
import Discord.Types
import UnliftIO (liftIO)

import Command
import qualified DB
import Owoifier (owoify)
import Utils (sendMessageDM)

--import qualified AprilFools
import qualified Academic
import qualified Admin
import qualified BinancePriceFetcher
import qualified HallOfFame
import qualified Haskell
import qualified MCServer
import qualified Misc
import qualified ModifyEventsChannel
import qualified QuoteSystem
import qualified RoleSelfAssign

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
generatedHelp = alias (T.pack "help") $ helpCommand (T.pack "helpme") commands $ \m ->
    liftIO (TIO.readFile "./src/assets/help.txt")
        >>= sendMessageDM (userId $ messageAuthor m)
        .   owoify

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
reactionRemoveReceivers :: [ReactionInfo -> DiscordHandler ()]
reactionRemoveReceivers = concat [RoleSelfAssign.reactionRemReceivers]

isFromBot :: Message -> Bool
isFromBot m = userIsBot (messageAuthor m)

handleEvent :: Event -> DiscordHandler ()
handleEvent event = case event of
    GuildCreate guild guildInfo ->
        liftIO $ DB.initGuildSpecificDatabase (guildId guild)
    MessageCreate m -> unless (isFromBot m) $ do
        for_ messageReceivers ($ m) <|> pure ()
        runCommands (generatedHelp : commands) m
    MessageReactionAdd    r -> for_ reactionAddReceivers ($ r) <|> pure ()
    MessageReactionRemove r -> for_ reactionRemoveReceivers ($ r) <|> pure ()
    _                       -> pure ()
