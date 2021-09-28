{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
    ( ConfigModules(lifeTime, log_priority, server_maximum_body_flush,
              server_port)
    , getDbConfig
    , getLgConfig
    , getPlConfig
    , getSrConfig
    , getTkConfig
    , newConfigHandle
    )
import HelpFunction (dbAddress, dbServerAddress)
import Logger (Handle(Handle), logInfo, printLog)
import Network.Wai.Handler.Warp
    ( defaultSettings
    , runSettings
    , setMaximumBodyFlush
    , setPort
    )
import OperationsHandle (operationsHandler)
import Router (routes)

main :: IO ()
main = do
    hConfig <- newConfigHandle
    confToken <- getTkConfig hConfig
    confLogger <- getLgConfig hConfig
    confServer <- getSrConfig hConfig
    confDb <- getDbConfig hConfig
    confPool <- getPlConfig hConfig
    let db_address = dbAddress confDb
    let token_lifetime = lifeTime confToken
    let hLogger = Handle (log_priority confLogger) printLog
    let db_server_address = dbServerAddress confDb
    logInfo hLogger "Server started"
    runSettings
        (setMaximumBodyFlush (server_maximum_body_flush confServer) $
         setPort (server_port confServer) defaultSettings) $
        routes
            hLogger
            db_address
            db_server_address
            token_lifetime
            confPool
            operationsHandler
