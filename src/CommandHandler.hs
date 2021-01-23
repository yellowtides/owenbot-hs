module CommandHandler (handleCommand, isCommand) where

import qualified Discord.Requests as R
import Discord.Types
import Discord

import qualified Data.Text as T
import Utils (rmFuncText, sendMessageChan, (=~=), isMod, isRole)

import ILA                      ( sendThmChan, sendDefChan, sendLemChan, sendTextbookChan )
import ILARE                    ( ilathmRE, iladefRE, ilalemmaRE, ilatextbookRE )
import qualified Inf1A  as I1A  ( sendHDocChan, sendBoolChan, sendTextbookChan, sendSylChan )
import Inf1ARE                  ( i1atextbookRE, syllogismsRE, booleanRE, hoogleInfRE )
import Calc as CAP              ( sendTextbookChan)
import CalcRE as CRE            ( calctextbookRE )
import qualified Helpme as HLP  ( sendHelpDM)
import ReactHandler
import ReactHandlerRE
import Admin                    ( sendGitInfo, sendInstanceInfo, restartOwen )
import AdminRE                 
import HelpmeRE                 ( helpRE )

isCommand :: T.Text -> Bool
isCommand m = any (m =~=) commandREs

handleCommand :: Message -> DiscordHandler (Either RestCallErrorCode Message)
handleCommand m
    | cmdText =~= ilathmRE        = ILA.sendThmChan channel cmdText
    | cmdText =~= iladefRE        = testRE $ ILA.sendDefChan channel cmdText
    | cmdText =~= ilalemmaRE      = ILA.sendLemChan channel cmdText
    | cmdText =~= ilatextbookRE   = simTyping $ ILA.sendTextbookChan channel
    | cmdText =~= syllogismsRE    = I1A.sendSylChan channel
    | cmdText =~= booleanRE       = I1A.sendBoolChan channel
    | cmdText =~= hoogleInfRE     = testRE $ I1A.sendHDocChan channel
    | cmdText =~= i1atextbookRE   = simTyping $ I1A.sendTextbookChan channel
    | cmdText =~= calctextbookRE  = simTyping $ CAP.sendTextbookChan channel
    | cmdText =~= reactLimitRE    = setLimit m $ read $ T.unpack noCommandText
    | cmdText =~= helpRE          = HLP.sendHelpDM user
    | cmdText =~= gitRE           = simTyping $ Admin.sendGitInfo m
    | cmdText =~= instanceRE      = Admin.sendInstanceInfo m
    | cmdText =~= restartRE       = restartOwen m 
    where
        cmdText       = messageText m
        noCommandText = rmFuncText cmdText
        channel       = messageChannel m
        user          = messageAuthor m
        simTyping     = (>>) $ restCall (R.TriggerTypingIndicator channel)
        testRE        = (>>) $ sendMessageChan channel (T.pack "message received")

commandREs :: [T.Text]
commandREs = [  
                ilathmRE, iladefRE, ilalemmaRE, ilatextbookRE,      -- ILA
                i1atextbookRE, syllogismsRE, booleanRE,             -- CL
                hoogleInfRE,                                        -- FP
                calctextbookRE,                                     -- CALC
                reactLimitRE,                                       -- Reaction settings
                helpRE,                                             -- HELP  
                gitRE, instanceRE, restartRE                               -- Instance check
             ]