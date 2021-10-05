{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import ControllersHandle (handler)
import Data.Pool
import Database.PostgreSQL.Simple
import Databaseoperations.CheckDatabase
import HelpFunction (dbAddress)
import Logger (Handle(Handle), logError, logInfo, printLog)
import Network.Wai.Handler.Warp
    ( defaultSettings
    , runSettings
    , setMaximumBodyFlush
    , setPort
    )
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
    logInfo hLogger "Server started"
    pool <-
        createPool
            (connectPostgreSQL db_address)
            close
            (num_stripes confPool)
            (idle_time confPool)
            (max_resources confPool)
    logInfo hLogger "Checking database"
    ch_db <- checkDb hLogger pool
    if ch_db
        then runSettings
                 (setMaximumBodyFlush (server_maximum_body_flush confServer) $
                  setPort (server_port confServer) defaultSettings) $
             routes hLogger token_lifetime pool handler
        else logError hLogger "Database not exist"
