{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
    ( ConfigModules(lifeTime, log_priority, server_maximum_body_flush,
              server_port)
    , getDbConfig
    , getLgConfig
    , getSrConfig
    , getTkConfig
    , newConfigHandle
    )
import HelpFunction (dbAddress)
import Logger (Handle(Handle), logInfo, printLog)
import Network.Wai.Handler.Warp
    ( defaultSettings
    , runSettings
    , setMaximumBodyFlush
    , setPort
    )
import Router (routes)
import ControllersHandle (handler)

main :: IO ()
main = do
    hConfig <- newConfigHandle
    confToken <- getTkConfig hConfig
    confLogger <- getLgConfig hConfig
    confServer <- getSrConfig hConfig
    confDb <- getDbConfig hConfig
    let db_address = dbAddress confDb
    let token_lifetime = lifeTime confToken
    let hLogger = Handle (log_priority confLogger) printLog
    logInfo hLogger "Serving"
    runSettings
        (setMaximumBodyFlush (server_maximum_body_flush confServer) $
         setPort (server_port confServer) defaultSettings) $
        routes hLogger db_address token_lifetime handler

