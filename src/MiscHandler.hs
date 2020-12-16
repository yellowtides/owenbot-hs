module MiscHandler (isOwoifiable, handleOwoify,
                    isNietzsche, handleNietzsche) where

import qualified Discord.Requests as R
import Discord.Types
import Discord

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import UnliftIO (liftIO)
import Text.Regex.TDFA

import Senders (sendMessageChan, sendFileChan)
import Owoifier (owoify)
import OwenRegex (owoifiableRE, nietzscheRE)

isOwoifiable :: T.Text -> Bool
isOwoifiable = (=~ owoifiableRE)

handleOwoify :: Message -> DiscordHandler (Either RestCallErrorCode Message)
handleOwoify m = sendMessageChan (messageChannel m) (owoify $ messageText m)

isNietzsche :: T.Text -> Bool
isNietzsche = (=~ nietzscheRE)

handleNietzsche :: Message -> DiscordHandler (Either RestCallErrorCode Message)
handleNietzsche m = liftIO (TIO.readFile "./src/assets/nietzsche.txt") >>= sendMessageChan (messageChannel m) . owoify