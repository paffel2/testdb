{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
module Logger where
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Parse
import Network.HTTP.Types
import GHC.Generics
import Data.Aeson
--import Testdatabase
import Control.Exception
import qualified Data.Text.Lazy.Encoding as EL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.Encoding as E
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BC
import Types
import Data.Maybe
import Text.Read
import Control.Applicative
import HelpFunction
import Responses
import Control.Monad


data Priority
    = Debug    -- ^ Debug messages
    | Info     -- ^ Notable information that requires no immediate action.
    | Warning  -- ^ Something is probably wrong, and we should investigate.
    | Error    -- ^ Something is wrong and immediate action is required.
    deriving (Eq, Ord, Show)

data Handle  = Handle

    {   priority :: Priority,
        log :: Priority -> T.Text -> IO () }



logging :: Priority -> Handle  -> T.Text -> IO ()
logging pr h text = when (pr >= p) (somePrint pr text)
    where p = Logger.priority h
          somePrint = Logger.log h

logDebug, logInfo, logWarning, logError :: Handle -> T.Text -> IO ()
logDebug = logging Debug
logInfo = logging Info
logWarning = logging Warning
logError = logging Logger.Error



printLog :: Priority -> T.Text -> IO ()
printLog priority txt = TIO.putStrLn $ T.concat [T.pack $ show priority, "| ", txt]


{-showOrd :: String
showOrd = show $ (Info > Logger.Error)-}


{-data Handle m = Handle

    {   priority :: Priority,
        log :: Priority -> T.Text -> m () }



logging :: Monad m => Priority -> Handle m -> T.Text -> m ()
logging pr h text = when (pr >= p) (somePrint pr text)
    where p = Logger.priority h
          somePrint = Logger.log h

logDebug, logInfo, logWarning, logError ::Monad m =>  Handle m-> T.Text -> m ()
logDebug = logging Debug
logInfo = logging Info
logWarning = logging Warning
logError = logging Logger.Error-}

