{-# LANGUAGE FlexibleContexts #-}

module PostgreSqlWithPool where

import           Control.Exception
import           Control.Monad.Except
import           Data.Int                             (Int64)
import           Data.Maybe
import           Data.Pool                            (Pool, withResource)
import           Database.PostgreSQL.Simple           (Connection, FromRow,
                                                       Query, SqlError, ToRow,
                                                       execute, executeMany,
                                                       execute_, query, query_,
                                                       returning, sqlState,
                                                       withTransaction)
import           Database.PostgreSQL.Simple.Migration (MigrationCommand (MigrationDirectory, MigrationInitialization),
                                                       MigrationContext (MigrationContext),
                                                       MigrationResult,
                                                       runMigration)
import           Database.PostgreSQL.Simple.Util      (existsTable)
import           HelpFunction
import           Types.Other

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
{-executeWithPoolNew ::
       (ToRow q, MonadIO m, MonadError SomeErrorNew  m)
    => Pool Connection
    -> Query
    -> q
    -> m Int64 -}
--executeWithPoolNew :: (MonadIO m, ToRow q) => Pool Connection -> Query -> q -> m b
executeWithPoolNew ::
       (MonadIO m, ToRow q, MonadError SomeError m)
    => Pool Connection
    -> Query
    -> q
    -> m Int64
executeWithPoolNew pool q inf = do
    someAnswer <- liftIO $ try $ executeWithPool pool q inf
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
    someAnswer <- liftIO $ try $ query_WithPool pool q
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
    someAnswer <- liftIO $ try $ queryWithPool pool q inf
    case someAnswer of
        Left sqlError -> do
            let errStateInt =
                    fromMaybe 0 (readByteStringToInt $ sqlState sqlError)
            throwError $ DatabaseErrorNew errStateInt
        Right ans -> return ans

returningWithPoolNew ::
       (ToRow q, FromRow r) => Pool Connection -> Query -> [q] -> IO [r]
returningWithPoolNew pool q inf =
    withResource pool $ \conn -> returning conn q inf

execute_WithPoolNew :: Pool Connection -> Query -> IO Int64
execute_WithPoolNew pool q = withResource pool $ \conn -> execute_ conn q

executeManyWithPoolNew :: ToRow q => Pool Connection -> Query -> [q] -> IO Int64
executeManyWithPoolNew pool q s =
    withResource pool $ \conn -> executeMany conn q s
