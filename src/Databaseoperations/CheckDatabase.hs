{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.CheckDatabase where

import Control.Exception (catch)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Database.PostgreSQL.Simple
    ( Connection
    , Only(fromOnly)
    , SqlError(sqlErrorMsg, sqlState)
    )
import Databaseoperations.InitDb (fillConnections, fillDb)
import Databaseoperations.Users (firstToken)
import HelpFunction (getMaybeLine, readByteStringToInt)
import Logger (Handle, logError, logInfo)
import PostgreSqlWithPool (executeWithPool, query_WithPool)
import Types (AdminData(..))

checkFill :: Handle -> Pool Connection -> IO (Either String String)
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
                        else return $ Right "Database filled") $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left "Something Wrong"

checkDb :: Handle -> Pool Connection -> IO Bool
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

createDbClear :: Handle -> Pool Connection -> IO Bool
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
                { admin_login = new_admin_login
                , admin_password = new_admin_password
                , admin_first_name = new_admin_first_name
                , admin_last_name = new_admin_last_name
                , admin_mark = True
                }
    result <- fillDb hLogger pool >>= fillConnections hLogger pool
    case result of
        Left _ -> return False
        Right _ -> do
            add_admin <- addAdminToDB hLogger pool admin_information
            case add_admin of
                Left _ -> return False
                Right bs -> do
                    logInfo hLogger $
                        T.concat
                            ["admin token ", E.decodeUtf8 $ LBS.toStrict bs]
                    return True

addAdminToDB ::
       Handle
    -> Pool Connection
    -> AdminData
    -> IO (Either LBS.ByteString LBS.ByteString)
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
                 (fromMaybe "" $ admin_login admin_data)
                 (fromMaybe "" $ admin_password admin_data)
        else do
            return $ Left "Registration failed"
