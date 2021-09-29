{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.CheckDatabase where

import Control.Exception (catch)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection, Only, SqlError(sqlState))
import HelpFunction (readByteStringToInt)
import Logger (Handle, logError, logInfo)
import PostgreSqlWithPool (query_WithPool)

checkDb :: Handle IO -> Pool Connection -> IO Bool
checkDb hLogger pool =
    catch
        (do n <- query_WithPool pool "select 1" :: IO [Only Int]
            if n /= []
                then do
                    logInfo hLogger "Database exist"
                    return True
                else do
                    logError hLogger "Database not exist"
                    return False) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return False
