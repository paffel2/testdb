{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.Migrations where

import           Control.Exception                    (catch)
import           Data.Maybe                           (fromMaybe)
import           Data.Pool                            (Pool)
import qualified Data.Text                            as T
import           Database.PostgreSQL.Simple           (Connection, Only,
                                                       SqlError (sqlState))
import           Database.PostgreSQL.Simple.Migration (MigrationResult (MigrationError, MigrationSuccess))
import           Databaseoperations.Users             (firstToken)
import           HelpFunction                         (getMaybeLine,
                                                       readByteStringToInt)
import           Logger                               (LoggerHandle, logError,
                                                       logInfo)
import           PostgreSqlWithPool                   (executeWithPool,
                                                       existSchemaMigrationWithPool,
                                                       initMigration,
                                                       query_WithPool,
                                                       runMigrationWithPool)
import           Types.Other                          (SomeError (DatabaseError),
                                                       Token (getToken))
import           Types.Users                          (AdminData (AdminData, adminFirstName, adminLastName, adminLogin, adminMark, adminPassword),
                                                       Login (Login),
                                                       Password (Password))

createAdmin :: LoggerHandle IO -> Pool Connection -> IO (Either SomeError ())
createAdmin hLogger pool = do
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
    n <-
        executeWithPool
            pool
            "insert into users (login,user_password,first_name,last_name,admin_mark,creation_date) values (?,crypt(?,gen_salt('md5')),?,?,?,now())"
            adminInformation
    if n > 0
        then do
            token <-
                firstToken
                    hLogger
                    pool
                    (adminLogin adminInformation)
                    (adminPassword adminInformation)
            case token of
                Left se -> return $ Left se
                Right to -> do
                    logInfo hLogger $ T.concat ["admin token ", getToken to]
                    return $ Right ()
        else do
            logError hLogger "Registration failed"
            return $ Left DatabaseError

------------------------------------------------------
checkDbExist :: LoggerHandle IO -> Pool Connection -> IO (Either SomeError ())
checkDbExist hLogger pool =
    catch
        (do n <- query_WithPool pool "select 1" :: IO [Only Int]
            if null n
                then do
                    logError hLogger "Database not exist or unavailable"
                    return $ Left DatabaseError
                else do
                    return $ Right ()) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            "Database cheking error " <> T.pack (show errStateInt)
        return $ Left DatabaseError

migrations ::
       LoggerHandle IO
    -> Pool Connection
    -> Either SomeError ()
    -> IO (Either SomeError ())
migrations _ _ (Left message) = return (Left message)
migrations hLogger pool (Right _) =
    catch
        (do check_migrations <- existSchemaMigrationWithPool pool
            if check_migrations
                then anotherMigration hLogger pool
                else initMigrations hLogger pool) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            "Database filling error " <> T.pack (show errStateInt)
        return $ Left DatabaseError

initMigrations :: LoggerHandle IO -> Pool Connection -> IO (Either SomeError ())
initMigrations hLogger pool =
    catch
        (do initDb <- initMigration pool
            case initDb of
                MigrationError _ -> return $ Left DatabaseError
                MigrationSuccess -> do
                    let dir = "sql/fill_Db"
                    result <- runMigrationWithPool pool dir
                    case result of
                        MigrationError _ -> do
                            logError hLogger "Database not filled"
                            return $ Left DatabaseError
                        MigrationSuccess -> do
                            logInfo hLogger "Database filled"
                            return $ Right ()) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            "Database init filling error " <> T.pack (show errStateInt)
        return $ Left DatabaseError

anotherMigration ::
       LoggerHandle IO -> Pool Connection -> IO (Either SomeError ())
anotherMigration hLogger pool =
    catch
        (do let dir = "sql/fill_Db"
            result <- runMigrationWithPool pool dir
            case result of
                MigrationError _ -> do
                    logError hLogger "Database not filled"
                    return $ Left DatabaseError
                MigrationSuccess -> do
                    logInfo hLogger "Database filled"
                    return $ Right ()) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            "Database another filling error " <> T.pack (show errStateInt)
        return $ Left DatabaseError

checkAdminInDb ::
       LoggerHandle IO
    -> Pool Connection
    -> Either SomeError ()
    -> IO (Either SomeError ())
checkAdminInDb _ _ (Left message) = return (Left message)
checkAdminInDb hLogger pool (Right _) =
    catch
        (do logInfo hLogger "Cheking Admin user"
            n <-
                query_WithPool
                    pool
                    "select admin_mark from users where admin_mark = True" :: IO [Only Bool]
            if null n
                then do
                    logInfo
                        hLogger
                        "Admin user not exist.Start creating Admin user."
                    createAdmin hLogger pool
                else do
                    logInfo hLogger "Admin user already exist."
                    return $ Right ()) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            "Database cheking admin error " <> T.pack (show errStateInt)
        return $ Left DatabaseError

checkDb :: LoggerHandle IO -> Pool Connection -> IO (Either SomeError ())
checkDb hLogger pool =
    checkDbExist hLogger pool >>= migrations hLogger pool >>=
    checkAdminInDb hLogger pool
