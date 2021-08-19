{-# LANGUAGE OverloadedStrings #-}

module Logger where

import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Priority
    = Debug -- ^ Debug messages
    | Info -- ^ Notable information that requires no immediate action.
    | Warning -- ^ Something is probably wrong, and we should investigate.
    | Error -- ^ Something is wrong and immediate action is required.
    deriving (Eq, Ord, Show)

data Handle =
    Handle
        { priority :: Priority
        , log :: Priority -> T.Text -> IO ()
        }

logging :: Priority -> Handle -> T.Text -> IO ()
logging pr h text = when (pr >= p) (somePrint pr text)
  where
    p = Logger.priority h
    somePrint = Logger.log h

logDebug, logInfo, logWarning, logError :: Handle -> T.Text -> IO ()
logDebug = logging Debug

logInfo = logging Info

logWarning = logging Warning

logError = logging Logger.Error

printLog :: Priority -> T.Text -> IO ()
printLog priority' txt =
    TIO.putStrLn $ T.concat [T.pack $ show priority', "| ", txt]
