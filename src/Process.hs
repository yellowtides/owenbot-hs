{-# LANGUAGE CPP #-}
{- | Here be dragons!
-}
module Process ( getMyProcessId ) where

import System.Process       (Pid)
#ifdef WINDOWS
import System.Win32.Process (getCurrentProcessId, ProcessId)
#else
import System.Posix.Process (getProcessID)
import System.Posix.Types   (ProcessID)
#endif

getMyProcessId :: IO Pid
getMyProcessId =
#ifdef WINDOWS
    getCurrentProcessId
#else
    getProcessID
#endif
