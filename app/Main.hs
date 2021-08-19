{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai.Handler.Warp
    ( setMaximumBodyFlush, setPort, runSettings, defaultSettings )
import Logger ( logInfo, Handle(Handle), printLog )
import Config
    ( getDbConfig,
      getLgConfig,
      getSrConfig,
      getTkConfig,
      newConfigHandle,
      ConfigModules(lifeTime, log_priority, server_maximum_body_flush, server_port) )
import Router ( routes )
import HelpFunction ( dbAddress )

main :: IO ()
main = do
    hConfig <- newConfigHandle
    confToken <- getTkConfig hConfig
    confLogger <- getLgConfig  hConfig
    confServer <- getSrConfig hConfig
    confDb <- getDbConfig hConfig
    let db_address = dbAddress confDb
    let token_lifetime = lifeTime confToken
    let hLogger = Handle (log_priority confLogger) printLog
    logInfo hLogger "Serving"
    runSettings (setMaximumBodyFlush (server_maximum_body_flush confServer) $ setPort (server_port confServer) defaultSettings) $ routes hLogger db_address token_lifetime
