module PostgreSqlWithPool where

import           Data.Int                             (Int64)
import           Data.Pool                            (Pool, withResource)
import           Database.PostgreSQL.Simple           (Connection, FromRow,
                                                       Query, ToRow, execute,
                                                       executeMany, execute_,
                                                       query, query_, returning,
                                                       withTransaction)
import           Database.PostgreSQL.Simple.Migration (MigrationCommand (MigrationDirectory, MigrationInitialization),
                                                       MigrationContext (MigrationContext),
                                                       MigrationResult,
                                                       runMigration)
import           Database.PostgreSQL.Simple.Util      (existsTable)

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
