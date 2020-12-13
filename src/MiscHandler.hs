module MiscHandler (isOwoifiable, handleOwoify) where

import qualified Discord.Requests as R
import Discord.Types
import Discord
import qualified Data.Text as T
import Text.Regex.TDFA

import Owoifier (owoify)
import OwenRegex (owoifiableRE)

isOwoifiable :: T.Text -> Bool
isOwoifiable = (=~ owoifiableRE)

handleOwoify :: Message -> DiscordHandler (Either RestCallErrorCode Message)
handleOwoify m = restCall $ R.CreateMessage (messageChannel m) (owoify $ messageText m)