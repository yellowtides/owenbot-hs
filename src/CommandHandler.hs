module CommandHandler (handleCommand, isCommand) where

import qualified Discord.Requests as R
import Discord.Types
import Discord

import qualified Data.Text as T

import Text.Regex.TDFA ((=~))
import Control.Exception (catch, IOException)

import qualified ILA    as ILA (sendThmChan, sendDefChan, sendLemChan, sendTextbookChan)
import qualified Inf1A  as I1A (sendHDocChan, sendBoolChan, sendTextbookChan, sendSylChan)
import qualified Helpme as HLP (sendHelpDM)
import OwenRegex

-- map through all the regexes and see if any of them match
isCommand :: T.Text -> Bool
isCommand m = any (m =~) commandREs

handleCommand :: Message -> DiscordHandler (Either RestCallErrorCode Message)
handleCommand m
    | cmdText =~ thmRE        = ILA.sendThmChan channel cmdText
    | cmdText =~ defRE        = ILA.sendDefChan channel cmdText
    | cmdText =~ lemmaRE      = ILA.sendLemChan channel cmdText
    | cmdText =~ textbookRE   = simTyping $ ILA.sendTextbookChan channel
    | cmdText =~ syllogismsRE = I1A.sendSylChan channel
    | cmdText =~ booleanRE    = I1A.sendBoolChan channel
    | cmdText =~ hoogleInfRE  = I1A.sendHDocChan channel
    | cmdText =~ helpRE       = HLP.sendHelpDM user
    where
        cmdText   = messageText m
        channel   = messageChannel m
        user      = messageAuthor m
        simTyping = (>>) $ restCall (R.TriggerTypingIndicator channel)