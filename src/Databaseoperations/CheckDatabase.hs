{-# LANGUAGE NamedFieldPuns    #-}
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
import           Types.Other                (SomeError (DatabaseError, OtherError),
                                             Token (getToken))
import           Types.Users                (AdminData (..), Login (Login),
                                             Password (Password))

checkFill :: LoggerHandle IO -> Pool Connection -> IO (Either SomeError ())
checkFill hLogger pool =
    catch
        (do n <-
                query_WithPool
                    pool
                    "select count(*) from information_schema.tables  WHERE table_schema = 'public'" :: IO [Only Int]
            if null n
                then return . Left . OtherError $
                     "Database not exist or unavailable"
                else do
                    let numOfTables = fromOnly $ Prelude.head n
                    if numOfTables /= 13
                        then do
                            return $ Left $ OtherError "Database not filled"
                        else return $ Right ()) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ "Database error " <> T.pack (show errStateInt)
        return $ Left DatabaseError

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
                        Left (OtherError "Database not filled") ->
                            createDbClear hLogger pool
                        Left _ -> return False
                else return False) $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        logError hLogger err
        return False

createDbClear :: LoggerHandle IO -> Pool Connection -> IO Bool
createDbClear hLogger pool = do
    logInfo hLogger "Input new admin login"
    newAdminLogin <- getMaybeLine
    logInfo hLogger "Input new admin password"
    newAdminPassword <- getMaybeLine
    logInfo hLogger "Input new admin first name"
    newAdminFirstName <- getMaybeLine
    logInfo hLogger "Input new admin last name"
    newAdminLastName <- getMaybeLine
    let adminInformation =
            AdminData
                { adminLogin = Login <$> newAdminLogin
                , adminPassword = Password <$> newAdminPassword
                , adminFirstName = newAdminFirstName
                , adminLastName = newAdminLastName
                , adminMark = True
                }
    result <- fillDb hLogger pool >>= fillConnections hLogger pool
    case result of
        Left _ -> return False
        Right _ -> do
            addAdmin <- addAdminToDB hLogger pool adminInformation
            case addAdmin of
                Left _ -> return False
                Right tk -> do
                    logInfo hLogger $ T.concat ["admin token ", getToken tk]
                    return True

addAdminToDB ::
       LoggerHandle IO
    -> Pool Connection
    -> AdminData
    -> IO (Either SomeError Token)
addAdminToDB hLogger _ AdminData {adminLogin = Nothing} = do
    logError hLogger "No login"
    return $ Left $ OtherError "No Login"
addAdminToDB hLogger _ AdminData {adminPassword = Nothing} = do
    logError hLogger "No password"
    return $ Left $ OtherError "No password"
addAdminToDB hLogger pool adminData = do
    n <-
        executeWithPool
            pool
            "insert into users (login,user_password,first_name,last_name,admin_mark,creation_date) values (?,crypt(?,gen_salt('md5')),?,?,?,now())"
            adminData
    if n > 0
        then firstToken
                 hLogger
                 pool
                 (adminLogin adminData)
                 (adminPassword adminData)
        else do
            return $ Left $ OtherError "Registration failed"

fillDb :: LoggerHandle IO -> Pool Connection -> IO (Either SomeError ())
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
    -> Either SomeError ()
    -> IO (Either SomeError ())
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
