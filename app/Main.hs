{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
    
    
import ControllersHandle (handler)
import HelpFunction (dbAddress, dbServerAddress)
import Logger (Handle(Handle), logInfo, printLog)
import Network.Wai.Handler.Warp
    ( defaultSettings
    , runSettings
    , setMaximumBodyFlush
    , setPort
    )
import Router ( routes )

main :: IO ()
main = do
    hConfig <- newConfigHandle
    confToken <- getTokenConfig hConfig
    confLogger <- getLoggerConfig hConfig
    confServer <- getServerConfig hConfig
    confDb <- getDataBaseConfig hConfig
    confPool <- getPoolConfig hConfig
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
            handler
