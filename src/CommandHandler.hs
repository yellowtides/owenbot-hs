module CommandHandler (handleCommand, isCommand) where

import qualified Discord.Requests as R
import Discord.Types
import Discord

import qualified Data.Text as T
import Utils (sendMessageChan, (=~=), isMod, toRoles)

import ILA (sendThmChan, sendDefChan, sendLemChan, sendTextbookChan)
import ILARE                   ( ilathmRE, iladefRE, ilalemmaRE, ilatextbookRE )
import qualified Inf1A  as I1A (sendHDocChan, sendBoolChan, sendTextbookChan, sendSylChan)
import Inf1ARE                 ( i1atextbookRE, syllogismsRE, booleanRE, hoogleInfRE )
import Calc as CAP (sendTextbookChan)
import CalcRE as CRE ( calctextbookRE )
import qualified Helpme as HLP (sendHelpDM)
import HelpmeRE                (helpRE )

isCommand :: T.Text -> Bool
isCommand m = any (m =~=) commandREs

handleCommand :: Message -> DiscordHandler (Either RestCallErrorCode Message)
handleCommand m

-- TODO: figure why none of these commands call
    | cmdText =~= ilathmRE      = ILA.sendThmChan channel cmdText
    | cmdText =~= iladefRE      = testRE $ ILA.sendDefChan channel cmdText
    | cmdText =~= ilalemmaRE    = ILA.sendLemChan channel cmdText
    | cmdText =~= ilatextbookRE = simTyping $ ILA.sendTextbookChan channel
    | cmdText =~= syllogismsRE  = I1A.sendSylChan channel
    | cmdText =~= booleanRE     = I1A.sendBoolChan channel
    | cmdText =~= hoogleInfRE   = testRE $ I1A.sendHDocChan channel
    | cmdText =~= i1atextbookRE = simTyping $ I1A.sendTextbookChan channel
    -- TODO: fix calctextbookRE to actually call the command
    | cmdText =~= calctextbookRE = simTyping $ CAP.sendTextbookChan channel

    | cmdText =~= helpRE        = HLP.sendHelpDM user
    where
        cmdText   = messageText m
        channel   = messageChannel m
        user      = messageAuthor m
        simTyping = (>>) $ restCall (R.TriggerTypingIndicator channel)
        testRE = (>>) $ sendMessageChan channel (T.pack "message received")

commandREs :: [T.Text]
commandREs = [  
                ilathmRE, iladefRE, ilalemmaRE, ilatextbookRE,      -- ILA
                i1atextbookRE, syllogismsRE, booleanRE,             -- CL
                hoogleInfRE,                                        -- FP
                calctextbookRE,                                     -- CALC
                helpRE                                              -- HELP  
             ]