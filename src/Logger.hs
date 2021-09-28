{-# LANGUAGE OverloadedStrings #-}

module Logger where

import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Priority
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq, Ord, Show)

data Handle m =
    Handle
        { priority :: Priority
        , log :: Priority -> T.Text -> m ()
        }

logging :: Monad m => Priority -> Handle m -> T.Text -> m ()
logging pr h text = when (pr >= p) (somePrint pr text)
  where
    p = Logger.priority h
    somePrint = Logger.log h

logDebug, logInfo, logWarning, logError :: Monad m => Handle m -> T.Text -> m ()
logDebug = logging Debug

logInfo = logging Info

logWarning = logging Warning

logError = logging Logger.Error

printLog :: Priority -> T.Text -> IO ()
printLog priority' txt =
    TIO.putStrLn $ T.concat [T.pack $ show priority', "| ", txt]
