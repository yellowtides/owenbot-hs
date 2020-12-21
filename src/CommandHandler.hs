module CommandHandler (handleCommand, isCommand) where

import qualified Discord.Requests as R
import Discord.Types
import Discord

import qualified Data.Text as T
import Text.Regex.TDFA ((=~))

import qualified ILA    as ILA (sendThmChan, sendDefChan, sendLemChan, sendTextbookChan)
import ILARE
import qualified Inf1A  as I1A (sendHDocChan, sendBoolChan, sendTextbookChan, sendSylChan)
import Inf1ARE
import qualified Helpme as HLP (sendHelpDM)
import HelpmeRE

isCommand :: T.Text -> Bool
isCommand m = any (m =~) commandREs

handleCommand :: Message -> DiscordHandler (Either RestCallErrorCode Message)
handleCommand m
    | cmdText =~ ilathmRE      = ILA.sendThmChan channel cmdText
    | cmdText =~ iladefRE      = ILA.sendDefChan channel cmdText
    | cmdText =~ ilalemmaRE    = ILA.sendLemChan channel cmdText
    | cmdText =~ ilatextbookRE = simTyping $ ILA.sendTextbookChan channel
    
    | cmdText =~ syllogismsRE  = I1A.sendSylChan channel
    | cmdText =~ booleanRE     = I1A.sendBoolChan channel
    | cmdText =~ hoogleInfRE   = I1A.sendHDocChan channel
    | cmdText =~ i1atextbookRE = simTyping $ I1A.sendTextbookChan channel

    | cmdText =~ helpRE        = HLP.sendHelpDM user
    where
        cmdText   = messageText m
        channel   = messageChannel m
        user      = messageAuthor m
        simTyping = (>>) $ restCall (R.TriggerTypingIndicator channel)

commandREs :: [T.Text]
commandREs = [  
                ilathmRE, iladefRE, ilalemmaRE, ilatextbookRE,      -- ILA
                i1atextbookRE, syllogismsRE, booleanRE,             -- CL
                hoogleInfRE,                                        -- FP
                helpRE                                              -- HELP  
             ]