{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.CheckDatabase where

import           Control.Exception          (catch)
import qualified Data.ByteString.Char8      as BC
import           Data.Maybe                 (fromMaybe)
import           Data.Pool                  (Pool)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as E
import           Database.PostgreSQL.Simple (Connection, Only (fromOnly),
                                             SqlError (sqlErrorMsg, sqlState))
import           Databaseoperations.Users   (firstToken)
import           HelpFunction               (getMaybeLine, readByteStringToInt,
                                             toQuery)
import           Logger                     (LoggerHandle, logDebug, logError,
                                             logInfo)
import           PostgreSqlWithPool         (executeWithPool, execute_WithPool,
                                             query_WithPool)
import           Types.Other                (ErrorMessage, Token (..))
import           Types.Users                (AdminData (..), Login (Login),
                                             Password (Password))

checkFill :: LoggerHandle IO -> Pool Connection -> IO (Either ErrorMessage ())
checkFill hLogger pool =
    catch
        (do n <-
                query_WithPool
                    pool
                    "select count(*) from information_schema.tables  WHERE table_schema = 'public'" :: IO [Only Int]
            if null n
                then return $ Left "Database not exist or unavailable"
                else do
                    let num_of_tables = fromOnly $ Prelude.head n
                    if num_of_tables /= 13
                        then do
                            return $ Left "Database not filled"
                        else return $ Right ()) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left "Something Wrong"

checkDb :: LoggerHandle IO -> Pool Connection -> IO Bool
checkDb hLogger pool =
    catch
        (do n <- query_WithPool pool "select 1" :: IO [Only Int]
            if n /= []
                then do
                    logInfo hLogger "Database exist"
                    ch_filled <- checkFill hLogger pool
                    case ch_filled of
                        Right _ -> do
                            logInfo hLogger "Database filled"
                            return True
                        Left "Database not filled" -> createDbClear hLogger pool
                        Left _ -> return False
                else return False) $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        logError hLogger err
        return False

createDbClear :: LoggerHandle IO -> Pool Connection -> IO Bool
createDbClear hLogger pool = do
    logInfo hLogger "Input new admin login"
    new_admin_login <- getMaybeLine
    logInfo hLogger "Input new admin password"
    new_admin_password <- getMaybeLine
    logInfo hLogger "Input new admin first name"
    new_admin_first_name <- getMaybeLine
    logInfo hLogger "Input new admin last name"
    new_admin_last_name <- getMaybeLine
    let admin_information =
            AdminData
                { admin_login = Login <$> new_admin_login
                , admin_password = Password <$> new_admin_password
                , admin_first_name = new_admin_first_name
                , admin_last_name = new_admin_last_name
                , admin_mark' = True
                }
    result <- fillDb hLogger pool >>= fillConnections hLogger pool
    case result of
        Left _ -> return False
        Right _ -> do
            add_admin <- addAdminToDB hLogger pool admin_information
            case add_admin of
                Left _ -> return False
                Right tk -> do
                    logInfo hLogger $ T.concat ["admin token ", from_token tk]
                    return True

addAdminToDB ::
       LoggerHandle IO
    -> Pool Connection
    -> AdminData
    -> IO (Either ErrorMessage Token)
addAdminToDB hLogger _ (AdminData Nothing _ _ _ _) = do
    logError hLogger "No login"
    return $ Left "No Login"
addAdminToDB hLogger _ (AdminData _ Nothing _ _ _) = do
    logError hLogger "No password"
    return $ Left "No password"
addAdminToDB hLogger pool admin_data = do
    n <-
        executeWithPool
            pool
            "insert into users (login,user_password,first_name,last_name,admin_mark,creation_date) values (?,crypt(?,gen_salt('md5')),?,?,?,now())"
            admin_data
    if n > 0
        then firstToken
                 hLogger
                 pool
                 (admin_login admin_data)
                 (admin_password admin_data)
        else do
            return $ Left "Registration failed"

fillDb :: LoggerHandle IO -> Pool Connection -> IO (Either ErrorMessage ())
fillDb hLogger pool = do
    logDebug hLogger "Read script"
    script <- BC.readFile "sql/fill_database.sql"
    let q = toQuery script
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Start filling"
    _ <- execute_WithPool pool q
    logDebug hLogger "Db filled"
    return $ Right ()

fillConnections ::
       LoggerHandle IO
    -> Pool Connection
    -> Either ErrorMessage ()
    -> IO (Either ErrorMessage ())
fillConnections hLogger pool (Right _) = do
    logDebug hLogger "Read script"
    script <- BC.readFile "sql/fill_connections.sql"
    let q = toQuery script
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Start filling connections"
    _ <- execute_WithPool pool q
    logDebug hLogger "Connections created"
    return $ Right ()
fillConnections _ _ (Left mess) = return $ Left mess
