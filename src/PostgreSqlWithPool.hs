module PostgreSqlWithPool where
import Data.Pool
import Database.PostgreSQL.Simple
import Data.Int


executeWithPool :: ToRow q => Pool Connection -> Query -> q -> IO Int64
executeWithPool pool q inf = 
    withResource pool $ \conn -> execute conn q inf


queryWithPool :: (ToRow q, FromRow r) => Pool Connection -> Query -> q -> IO [r]
queryWithPool pool q inf =
    withResource pool $ \conn -> query conn q inf


returningWithPool :: (ToRow q, FromRow r) => Pool Connection -> Query -> [q] -> IO [r]
returningWithPool pool q inf =
    withResource pool $ \conn -> returning conn q inf

query_WithPool :: FromRow r => Pool Connection -> Query -> IO [r]
query_WithPool pool q =
    withResource pool $ \conn -> query_ conn q