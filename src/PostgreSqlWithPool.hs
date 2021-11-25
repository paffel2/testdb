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
                                                       MigrationResult,
                                                       runMigration)
import           Database.PostgreSQL.Simple.Util      (existsTable)
import           HelpFunction                         (readByteStringToInt)
import           Types.Other                          (SomeError (DatabaseErrorNew))

executeWithPool :: ToRow q => Pool Connection -> Query -> q -> IO Int64
executeWithPool pool q inf = withResource pool $ \conn -> execute conn q inf

queryWithPool :: (ToRow q, FromRow r) => Pool Connection -> Query -> q -> IO [r]
queryWithPool pool q inf = withResource pool $ \conn -> query conn q inf

returningWithPool ::
       (ToRow q, FromRow r) => Pool Connection -> Query -> [q] -> IO [r]
returningWithPool pool q inf = withResource pool $ \conn -> returning conn q inf

query_WithPool :: FromRow r => Pool Connection -> Query -> IO [r]
query_WithPool pool q = withResource pool $ \conn -> query_ conn q

execute_WithPool :: Pool Connection -> Query -> IO Int64
execute_WithPool pool q = withResource pool $ \conn -> execute_ conn q

executeManyWithPool :: ToRow q => Pool Connection -> Query -> [q] -> IO Int64
executeManyWithPool pool q s = withResource pool $ \conn -> executeMany conn q s

runMigrationWithPool ::
       Pool Connection -> FilePath -> IO (MigrationResult String)
runMigrationWithPool pool dir =
    withResource pool $ \conn ->
        withTransaction conn $
        runMigration $ MigrationContext (MigrationDirectory dir) False conn

existSchemaMigrationWithPool :: Pool Connection -> IO Bool
existSchemaMigrationWithPool pool =
    withResource pool $ \conn -> existsTable conn "schema_migrations"

initMigration :: Pool Connection -> IO (MigrationResult String)
initMigration pool =
    withResource pool $ \conn ->
        withTransaction conn $
        runMigration $ MigrationContext MigrationInitialization False conn

----------------------------------------------------------------------
executeWithPoolNew ::
       (MonadIO m, ToRow q, MonadError SomeError m)
    => Pool Connection
    -> Query
    -> q
    -> m Int64
executeWithPoolNew pool q inf = do
    someAnswer <- liftIO $ try (withResource pool $ \conn -> execute conn q inf)
    case someAnswer of
        Left sqlError -> do
            let errStateInt =
                    fromMaybe 0 (readByteStringToInt $ sqlState sqlError)
            throwError $ DatabaseErrorNew errStateInt
        Right ans -> return ans

query_WithPoolNew ::
       (MonadIO m, FromRow r, MonadError SomeError m)
    => Pool Connection
    -> Query
    -> m [r]
query_WithPoolNew pool q = do
    someAnswer <- liftIO $ try (withResource pool $ \conn -> query_ conn q)
    case someAnswer of
        Left sqlError -> do
            let errStateInt =
                    fromMaybe 0 (readByteStringToInt $ sqlState sqlError)
            throwError $ DatabaseErrorNew errStateInt
        Right ans -> return ans

queryWithPoolNew ::
       (MonadIO m, ToRow q, FromRow r, MonadError SomeError m)
    => Pool Connection
    -> Query
    -> q
    -> m [r]
queryWithPoolNew pool q inf = do
    someAnswer <- liftIO $ try (withResource pool $ \conn -> query conn q inf) --  $ queryWithPool pool q inf
    case someAnswer of
        Left sqlError -> do
            let errStateInt =
                    fromMaybe 0 (readByteStringToInt $ sqlState sqlError)
            throwError $ DatabaseErrorNew errStateInt
        Right ans -> return ans

returningWithPoolNew ::
       (MonadIO m, ToRow q, FromRow r, MonadError SomeError m)
    => Pool Connection
    -> Query
    -> [q]
    -> m [r]
returningWithPoolNew pool q inf = do
    someAnswer <-
        liftIO $ try (withResource pool $ \conn -> returning conn q inf) --  $ queryWithPool pool q inf
    case someAnswer of
        Left sqlError -> do
            let errStateInt =
                    fromMaybe 0 (readByteStringToInt $ sqlState sqlError)
            throwError $ DatabaseErrorNew errStateInt
        Right ans -> return ans

execute_WithPoolNew ::
       (MonadIO m, MonadError SomeError m)
    => Pool Connection
    -> Query
    -> m Int64
execute_WithPoolNew pool q = do
    someAnswer <- liftIO $ try (withResource pool $ \conn -> execute_ conn q)
    case someAnswer of
        Left sqlError -> do
            let errStateInt =
                    fromMaybe 0 (readByteStringToInt $ sqlState sqlError)
            throwError $ DatabaseErrorNew errStateInt
        Right ans -> return ans

executeManyWithPoolNew ::
       (MonadIO m, MonadError SomeError m, ToRow q)
    => Pool Connection
    -> Query
    -> [q]
    -> m Int64
executeManyWithPoolNew pool q s = do
    someAnswer <-
        liftIO $ try (withResource pool $ \conn -> executeMany conn q s)
    case someAnswer of
        Left sqlError -> do
            let errStateInt =
                    fromMaybe 0 (readByteStringToInt $ sqlState sqlError)
            throwError $ DatabaseErrorNew errStateInt
        Right ans -> return ans
