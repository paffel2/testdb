{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
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
    hConfig <- getConfig
    let db_address = dbAddress . db_conf $ hConfig
    let token_lifetime = lifeTime hConfig
    let hLogger = Handle (log_priority hConfig) printLog
    let db_server_address = dbServerAddress . db_conf $ hConfig
    let poolParams = pool_params hConfig
    logInfo hLogger "Server started"
    runSettings
        (setMaximumBodyFlush (server_maximum_body_flush . server_conf $ hConfig) $
         setPort (server_port . server_conf $ hConfig) defaultSettings) $
        routes
            hLogger
            db_address
            db_server_address
            token_lifetime
            poolParams
            operationsHandler
