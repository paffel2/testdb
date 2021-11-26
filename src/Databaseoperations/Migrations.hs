{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.Migrations where

import           Control.Monad.Except       (MonadError (..), MonadIO (..),
                                             when)
import           Data.Maybe                 (isNothing)
import           Data.Pool                  (Pool)
import           Database.PostgreSQL.Simple (Connection, Only)
import           Databaseoperations.Users   (firstToken)
import           HelpFunction               (getMaybeLine)
import           Logger                     (LoggerHandle, logError, logInfo)
import           PostgreSqlWithPool         (executeWithPool,
                                             existSchemaMigrationWithPool,
                                             initMigration, query_WithPool,
                                             runMigrationWithPool)
import           Types.Other                (MonadIOWithError,
                                             SomeError (OtherError),
                                             Token (getToken))
import           Types.Users                (AdminData (AdminData, adminFirstName, adminLastName, adminLogin, adminMark, adminPassword),
                                             Login (Login), Password (Password))

---------------------------------------------------------------------------------------------------------------------
checkDbExist :: MonadIOWithError m => LoggerHandle IO -> Pool Connection -> m ()
checkDbExist hLogger pool =
    catchError
        (do n <- query_WithPool pool "select 1"
            when (null (n :: [Only Int])) $ do
                liftIO $ logError hLogger "Database not exist or unavailable"
                throwError $ OtherError "Database not exist or unavailable") $ \e -> do
        liftIO $ logError hLogger "Database not exist or unavailable"
        throwError e

initMigrations ::
       MonadIOWithError m => LoggerHandle IO -> Pool Connection -> m ()
initMigrations hLogger pool = do
    liftIO $ logInfo hLogger "Initialization"
    initMigration pool
    liftIO $ logInfo hLogger "Adding another migrations"
    runMigrationWithPool pool "sql/fill_Db"

anotherMigration :: MonadIOWithError m => Pool Connection -> m ()
anotherMigration pool = runMigrationWithPool pool "sql/fill_Db"

migrations :: MonadIOWithError m => LoggerHandle IO -> Pool Connection -> m ()
migrations hLogger pool = do
    check_migrations <- existSchemaMigrationWithPool pool
    if check_migrations
        then do
            liftIO $
                logInfo
                    hLogger
                    "Database already exist. Adding another migrations."
            anotherMigration pool
        else do
            liftIO $ logInfo hLogger "Database not exist."
            initMigrations hLogger pool

checkAdminInDb ::
       MonadIOWithError m => LoggerHandle IO -> Pool Connection -> m ()
checkAdminInDb hLogger pool = do
    n <-
        query_WithPool
            pool
            "select admin_mark from users where admin_mark = True"
    if null (n :: [Only Bool])
        then createAdmin hLogger pool
        else do
            liftIO $ logInfo hLogger "Admin already exist."
            return ()

createAdmin :: MonadIOWithError m => LoggerHandle IO -> Pool Connection -> m ()
createAdmin hLogger pool = do
    liftIO $ logInfo hLogger "Admin not exist. Creating admin."
    liftIO $ logInfo hLogger "Input new admin login"
    newAdminLogin <- liftIO getMaybeLine
    liftIO $ logInfo hLogger "Input new admin password"
    newAdminPassword <- liftIO getMaybeLine
    liftIO $ logInfo hLogger "Input new admin first name"
    newAdminFirstName <- liftIO getMaybeLine
    liftIO $ logInfo hLogger "Input new admin last name"
    newAdminLastName <- liftIO getMaybeLine
    let adminInformation =
            AdminData
                { adminLogin = Login <$> newAdminLogin
                , adminPassword = Password <$> newAdminPassword
                , adminFirstName = newAdminFirstName
                , adminLastName = newAdminLastName
                , adminMark = True
                }
    if isNothing newAdminLogin || isNothing newAdminPassword
        then do
            liftIO $ logError hLogger "Bad login or password"
            throwError $ OtherError "Bad login or password"
        else do
            n <-
                executeWithPool
                    pool
                    "insert into users (login,user_password,first_name,last_name,admin_mark,creation_date) values (?,crypt(?,gen_salt('md5')),?,?,?,now())"
                    adminInformation
            if n > 0
                then do
                    token <-
                        firstToken
                            pool
                            (adminLogin adminInformation)
                            (adminPassword adminInformation)
                    liftIO $ logInfo hLogger $ "Admin token " <> getToken token
                    return ()
                else do
                    liftIO $ logError hLogger "Registration failed"
                    throwError $ OtherError "Registration failed"

checkDb :: MonadIOWithError m => LoggerHandle IO -> Pool Connection -> m ()
checkDb hLogger pool = do
    checkDbExist hLogger pool
    migrations hLogger pool
    checkAdminInDb hLogger pool
