{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Config                           (ConfigModules (db_conf, lifeTime, log_priority, pool_params, server_conf),
                                                   PoolParams (idle_time, max_resources, num_stripes),
                                                   ServerConf (server_maximum_body_flush, server_port),
                                                   getConfig)
import           Data.Pool                        (createPool)
import           Database.PostgreSQL.Simple       (close, connectPostgreSQL)
import           Databaseoperations.CheckDatabase (checkDb)
import           HelpFunction                     (dbAddress)
import           Logger                           (Handle (Handle), logError,
                                                   logInfo, printLog)
import           Network.Wai.Handler.Warp         (defaultSettings, runSettings,
                                                   setMaximumBodyFlush, setPort)
import           OperationsHandle                 (operationsHandler)
import           Router                           (routes)

main :: IO ()
main = do
    hConfig <- getConfig
    let db_address = dbAddress . db_conf $ hConfig
    let token_lifetime = lifeTime hConfig
    let hLogger = Handle (log_priority hConfig) printLog
    let poolParams = pool_params hConfig
    logInfo hLogger "Server started"
    pool <-
        createPool
            (connectPostgreSQL db_address)
            close
            (num_stripes poolParams)
            (idle_time poolParams)
            (max_resources poolParams)
    let hOperations = operationsHandler hLogger pool
    logInfo hLogger "Checking database"
    ch_db <- checkDb hLogger pool
    if ch_db
        then runSettings
                 (setMaximumBodyFlush
                      (server_maximum_body_flush . server_conf $ hConfig) $
                  setPort (server_port . server_conf $ hConfig) defaultSettings) $
             routes hLogger token_lifetime hOperations
        else logError hLogger "Database not exist"
