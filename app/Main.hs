{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
    ( ConfigModules(db_conf, lifeTime, log_priority, pool_params,
              server_conf)
    , ServerConf(server_maximum_body_flush, server_port)
    , getConfig
    )
import Data.Pool (createPool)
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import HelpFunction (dbAddress, dbServerAddress)
import Logger (Handle(Handle), logError, logInfo, printLog)
import Network.Wai.Handler.Warp
    ( defaultSettings
    , runSettings
    , setMaximumBodyFlush
    , setPort
    )
import OperationsHandle (OperationsHandle(check_db), operationsHandler)
import Router (routes)
import Types (PoolParams(idle_time, max_resources, num_stripes))

main :: IO ()
main = do
    hConfig <- getConfig
    let db_address = dbAddress . db_conf $ hConfig
    let token_lifetime = lifeTime hConfig
    let hLogger = Handle (log_priority hConfig) printLog
    let db_server_address = dbServerAddress . db_conf $ hConfig
    let poolParams = pool_params hConfig
    logInfo hLogger "Server started"
    pool <-
        createPool
            (connectPostgreSQL db_address)
            close
            (num_stripes poolParams)
            (idle_time poolParams)
            (max_resources poolParams)
    db_check <- check_db operationsHandler hLogger pool
    if db_check
        then runSettings
                 (setMaximumBodyFlush
                      (server_maximum_body_flush . server_conf $ hConfig) $
                  setPort (server_port . server_conf $ hConfig) defaultSettings) $
             routes
                 hLogger
                 db_address
                 db_server_address
                 token_lifetime
                 pool
                 operationsHandler
        else logError hLogger "Database not exist"
