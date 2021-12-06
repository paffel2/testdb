{-# LANGUAGE FlexibleContexts #-}

module PostgreSqlWithPool where

import           Control.Exception                    (try)
import           Control.Monad.Except                 (MonadError (throwError),
                                                       MonadIO (..))
import           Data.Int                             (Int64)
import           Data.Maybe                           (fromMaybe)
import           Data.Pool                            (Pool, withResource)
import           Database.PostgreSQL.Simple           (Connection, FromRow,
                                                       Query,
                                                       SqlError (sqlState),
                                                       ToRow, execute,
                                                       executeMany, execute_,
                                                       query, query_, returning,
                                                       withTransaction)
import           Database.PostgreSQL.Simple.Migration (MigrationCommand (MigrationDirectory, MigrationInitialization),
                                                       MigrationContext (MigrationContext),
                                                       MigrationResult (MigrationError, MigrationSuccess),
                                                       runMigration)
import           Database.PostgreSQL.Simple.Util      (existsTable)
import           HelpFunction                         (readByteStringToInt)
import           Types.Other                          (MonadIOWithError,
                                                       SomeError (DatabaseError, OtherError))

----------------------------------------------------------------------
executeWithPool ::
       (MonadIO m, ToRow q, MonadError SomeError m)
    => Pool Connection
    -> Query
    -> q
    -> m Int64
executeWithPool pool q inf = do
    someAnswer <- liftIO $ try (withResource pool $ \conn -> execute conn q inf)
    case someAnswer of
        Left sqlError -> do
            let errStateInt =
                    fromMaybe 0 (readByteStringToInt $ sqlState sqlError)
            throwError $ DatabaseError errStateInt
        Right ans -> return ans

query_WithPool ::
       (MonadIO m, FromRow r, MonadError SomeError m)
    => Pool Connection
    -> Query
    -> m [r]
query_WithPool pool q = do
    someAnswer <- liftIO $ try (withResource pool $ \conn -> query_ conn q)
    case someAnswer of
        Left sqlError -> do
            let errStateInt =
                    fromMaybe 0 (readByteStringToInt $ sqlState sqlError)
            throwError $ DatabaseError errStateInt
        Right ans -> return ans

queryWithPool ::
       (MonadIO m, ToRow q, FromRow r, MonadError SomeError m)
    => Pool Connection
    -> Query
    -> q
    -> m [r]
queryWithPool pool q inf = do
    someAnswer <- liftIO $ try (withResource pool $ \conn -> query conn q inf)
    case someAnswer of
        Left sqlError -> do
            let errStateInt =
                    fromMaybe 0 (readByteStringToInt $ sqlState sqlError)
            throwError $ DatabaseError errStateInt
        Right ans -> return ans

returningWithPool ::
       (MonadIO m, ToRow q, FromRow r, MonadError SomeError m)
    => Pool Connection
    -> Query
    -> [q]
    -> m [r]
returningWithPool pool q inf = do
    someAnswer <-
        liftIO $ try (withResource pool $ \conn -> returning conn q inf)
    case someAnswer of
        Left sqlError -> do
            let errStateInt =
                    fromMaybe 0 (readByteStringToInt $ sqlState sqlError)
            throwError $ DatabaseError errStateInt
        Right ans -> return ans

execute_WithPool ::
       (MonadIO m, MonadError SomeError m)
    => Pool Connection
    -> Query
    -> m Int64
execute_WithPool pool q = do
    someAnswer <- liftIO $ try (withResource pool $ \conn -> execute_ conn q)
    case someAnswer of
        Left sqlError -> do
            let errStateInt =
                    fromMaybe 0 (readByteStringToInt $ sqlState sqlError)
            throwError $ DatabaseError errStateInt
        Right ans -> return ans

executeManyWithPool ::
       (MonadIO m, MonadError SomeError m, ToRow q)
    => Pool Connection
    -> Query
    -> [q]
    -> m Int64
executeManyWithPool pool q s = do
    someAnswer <-
        liftIO $ try (withResource pool $ \conn -> executeMany conn q s)
    case someAnswer of
        Left sqlError -> do
            let errStateInt =
                    fromMaybe 0 (readByteStringToInt $ sqlState sqlError)
            throwError $ DatabaseError errStateInt
        Right ans -> return ans

initMigration :: MonadIOWithError m => Pool Connection -> m ()
initMigration pool = do
    someResult <-
        liftIO $
        withResource pool $ \conn ->
            withTransaction conn $
            runMigration $ MigrationContext MigrationInitialization False conn
    case someResult of
        MigrationError err -> throwError $ OtherError err
        MigrationSuccess   -> return ()

runMigrationWithPool ::
       MonadIOWithError m => Pool Connection -> FilePath -> m ()
runMigrationWithPool pool dir = do
    someResult <-
        liftIO $
        withResource pool $ \conn ->
            withTransaction conn $
            runMigration $ MigrationContext (MigrationDirectory dir) False conn
    case someResult of
        MigrationError err -> throwError $ OtherError err
        MigrationSuccess   -> return ()

existSchemaMigrationWithPool :: MonadIOWithError m => Pool Connection -> m Bool
existSchemaMigrationWithPool pool = do
    someResult <-
        liftIO $
        try (withResource pool $ \conn -> existsTable conn "schema_migrations")
    case someResult of
        Left sqlError ->
            throwError $
            DatabaseError
                (fromMaybe 0 (readByteStringToInt $ sqlState sqlError))
        Right b -> return b
