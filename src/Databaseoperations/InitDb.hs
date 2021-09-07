{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.InitDb where

import Control.Exception (catch)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Pool (Pool, createPool, destroyAllResources)
import qualified Data.Text.Encoding as E
import Database.PostgreSQL.Simple
    ( Binary(Binary)
    , Connection
    , SqlError(sqlErrorMsg)
    , close
    , connectPostgreSQL
    )
import HelpFunction --(getFiles, sumStrings, toPairList, toQuery)
import Logger (Handle, logDebug, logError)
import PostgreSqlWithPool (executeWithPool, execute_WithPool)
import Types (DatabaseAddress)

createDb ::
       Handle
    -> Pool Connection
    -> DatabaseAddress
    -> IO (Either LBS.ByteString LBS.ByteString)
createDb hLogger pool db_add =
    catch
        (do logDebug hLogger "Creating connection"
            let conn' = connectPostgreSQL db_add
            logDebug hLogger "Creating pool"
            pool' <- createPool conn' close 1 10 10
            step_one <- initDb hLogger pool'
            destroyAllResources pool'
            c <- conn'
            close c
            fillDb step_one hLogger pool >>= fillConnections' hLogger pool >>= fillImages' hLogger pool
            >>= insertUsers' hLogger pool >>= insertAuthors' hLogger pool >>= insertCategories' hLogger pool
            >>= insertTags' hLogger pool >>= insertDrafts' hLogger pool >>= insertNews' hLogger pool >>= insertComments' hLogger pool
            ) $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        logError hLogger err
        return $ Left $ LBS.fromStrict $ sqlErrorMsg e

initDb :: Handle -> Pool Connection -> IO (Either LBS.ByteString LBS.ByteString)
initDb hLogger pool = do
    logDebug hLogger "Read script"
    script <- BC.readFile "sql/init_database.sql"
    let q = toQuery script
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Start creating"
    _ <- execute_WithPool pool q
    logDebug hLogger "Db created"
    return $ Right "Database created"

fillDb ::
       Either LBS.ByteString LBS.ByteString
    -> Handle
    -> Pool Connection
    -> IO (Either LBS.ByteString LBS.ByteString)
fillDb (Right _) hLogger pool = do
    logDebug hLogger "Read script"
    script <- BC.readFile "sql/fill_database.sql"
    let q = toQuery script
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Start filling"
    _ <- execute_WithPool pool q
    logDebug hLogger "Db filled"
    return $ Right "Database filled"
fillDb (Left mess) _ _ = return $ Left mess

fillConnections ::
       Either LBS.ByteString LBS.ByteString
    -> Handle
    -> Pool Connection
    -> IO (Either LBS.ByteString LBS.ByteString)
fillConnections (Right _) hLogger pool = do
    logDebug hLogger "Read script"
    script <- BC.readFile "sql/fill_connections.sql"
    let q = toQuery script
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Start filling connections"
    _ <- execute_WithPool pool q
    logDebug hLogger "Connections created"
    return $ Right "Connections created"
fillConnections (Left mess) _ _ = return $ Left mess


fillConnections' :: Handle
    -> Pool Connection
    -> Either LBS.ByteString LBS.ByteString
    -> IO (Either LBS.ByteString LBS.ByteString)
fillConnections' hLogger pool (Right _) = do
    logDebug hLogger "Read script"
    script <- BC.readFile "sql/fill_connections.sql"
    let q = toQuery script
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Start filling connections"
    _ <- execute_WithPool pool q
    logDebug hLogger "Connections created"
    return $ Right "Connections created"
fillConnections'  _ _ (Left mess) = return $ Left mess

fillImages ::
       Either LBS.ByteString LBS.ByteString
    -> Handle
    -> Pool Connection
    -> IO (Either LBS.ByteString LBS.ByteString)
fillImages (Right _) hLogger pool = do
    let q =
            toQuery
                "insert into images (image_name,image_b,content_type) values (?,? , 'image/jpeg')"
    files <- getFiles
    filesLBS <- mapM LBS.readFile $ ("sql/img/" ++) <$> files
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Start filling images"
    let sendFiles = zip files $ Binary <$> filesLBS
    _ <- loadImages q sendFiles
    logDebug hLogger "Images loaded"
    return $ Right "Images loaded"
  where
    loadImages quer (x:xs) = do
        _ <- executeWithPool pool quer x
        loadImages quer xs
    loadImages _ [] = return ()
fillImages (Left mess) _ _ = return $ Left mess

insertUsers ::
       Either LBS.ByteString LBS.ByteString
    -> Handle
    -> Pool Connection
    -> IO (Either LBS.ByteString LBS.ByteString)
insertUsers (Right _) hLogger pool = do
    logDebug hLogger "Read script"
    script <- BC.readFile "sql/insert_users.sql"
    let q = toQuery script
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Inserting users"
    _ <- execute_WithPool pool q
    logDebug hLogger "Users loaded"
    return $ Right "Users loaded"
insertUsers (Left mess) _ _ = return $ Left mess

insertAuthors ::
       Either LBS.ByteString LBS.ByteString
    -> Handle
    -> Pool Connection
    -> IO (Either LBS.ByteString LBS.ByteString)
insertAuthors (Right _) hLogger pool = do
    logDebug hLogger "Read script"
    script <- BC.readFile "sql/insert_authors.sql"
    let q = toQuery script
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Inserting authors"
    _ <- execute_WithPool pool q
    logDebug hLogger "Authors loaded"
    return $ Right "Authors loaded"
insertAuthors (Left mess) _ _ = return $ Left mess

insertCategories ::
       Either LBS.ByteString LBS.ByteString
    -> Handle
    -> Pool Connection
    -> IO (Either LBS.ByteString LBS.ByteString)
insertCategories (Right _) hLogger pool = do
    logDebug hLogger "Read script"
    script <- BC.readFile "sql/insert_categories.sql"
    let q = toQuery script
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Inserting categories"
    _ <- execute_WithPool pool q
    logDebug hLogger "Categories loaded"
    return $ Right "Categories loaded"
insertCategories (Left mess) _ _ = return $ Left mess

insertTags ::
       Either LBS.ByteString LBS.ByteString
    -> Handle
    -> Pool Connection
    -> IO (Either LBS.ByteString LBS.ByteString)
insertTags (Right _) hLogger pool = do
    logDebug hLogger "Read script"
    script <- BC.readFile "sql/insert_tags.sql"
    let q = toQuery script
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Inserting tags"
    _ <- execute_WithPool pool q
    logDebug hLogger "Tags loaded"
    return $ Right "Tags loaded"
insertTags (Left mess) _ _ = return $ Left mess

insertDrafts ::
       Either LBS.ByteString LBS.ByteString
    -> Handle
    -> Pool Connection
    -> IO (Either LBS.ByteString LBS.ByteString)
insertDrafts (Right _) hLogger pool = do
    logDebug hLogger "Read script"
    script <- BC.readFile "sql/insert_drafts.sql"
    let q = toQuery script
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Inserting drafts"
    _ <- execute_WithPool pool q
    logDebug hLogger "Drafts loaded"
    return $ Right "Drafts loaded"
insertDrafts (Left mess) _ _ = return $ Left mess

insertNews ::
       Either LBS.ByteString LBS.ByteString
    -> Handle
    -> Pool Connection
    -> IO (Either LBS.ByteString LBS.ByteString)
insertNews (Right _) hLogger pool = do
    logDebug hLogger "Read script"
    script <- BC.readFile "sql/insert_news.sql"
    let q = toQuery script
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Inserting news"
    _ <- execute_WithPool pool q
    logDebug hLogger "News loaded"
    return $ Right "Database created and filled tests data"
insertNews (Left mess) _ _ = return $ Left mess

insertComments  ::
       Either LBS.ByteString LBS.ByteString
    -> Handle
    -> Pool Connection
    -> IO (Either LBS.ByteString LBS.ByteString)
insertComments (Right _) hLogger pool = do
    logDebug hLogger "Read script"
    script <- BC.readFile "sql/insert_comments.sql"
    let q = toQuery script
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Inserting comments"
    _ <- execute_WithPool pool q
    logDebug hLogger "Comments loaded"
    return $ Right "Database created and filled tests data"
insertComments (Left mess) _ _ = return $ Left mess

------------------------------------

fillImages' ::  Handle
    -> Pool Connection
    -> Either LBS.ByteString LBS.ByteString
    -> IO (Either LBS.ByteString LBS.ByteString)
fillImages' hLogger pool (Right _) = do
    let q =
            toQuery
                "insert into images (image_name,image_b,content_type) values (?,? , 'image/jpeg')"
    files <- getFiles
    filesLBS <- mapM LBS.readFile $ ("sql/img/" ++) <$> files
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Start filling images"
    let sendFiles = zip files $ Binary <$> filesLBS
    _ <- loadImages q sendFiles
    logDebug hLogger "Images loaded"
    return $ Right "Images loaded"
  where
    loadImages quer (x:xs) = do
        _ <- executeWithPool pool quer x
        loadImages quer xs
    loadImages _ [] = return ()
fillImages' _ _ (Left mess) = return $ Left mess

insertUsers' ::  Handle
    -> Pool Connection
    -> Either LBS.ByteString LBS.ByteString
    -> IO (Either LBS.ByteString LBS.ByteString)
insertUsers' hLogger pool (Right _) = do
    logDebug hLogger "Read script"
    script <- BC.readFile "sql/insert_users.sql"
    let q = toQuery script
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Inserting users"
    _ <- execute_WithPool pool q
    logDebug hLogger "Users loaded"
    return $ Right "Users loaded"
insertUsers' _ _ (Left mess) = return $ Left mess

insertAuthors' :: Handle
    -> Pool Connection
    -> Either LBS.ByteString LBS.ByteString
    -> IO (Either LBS.ByteString LBS.ByteString)
insertAuthors' hLogger pool (Right _) = do
    logDebug hLogger "Read script"
    script <- BC.readFile "sql/insert_authors.sql"
    let q = toQuery script
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Inserting authors"
    _ <- execute_WithPool pool q
    logDebug hLogger "Authors loaded"
    return $ Right "Authors loaded"
insertAuthors' _ _ (Left mess) = return $ Left mess

insertCategories' ::  Handle
    -> Pool Connection
    -> Either LBS.ByteString LBS.ByteString
    -> IO (Either LBS.ByteString LBS.ByteString)
insertCategories' hLogger pool (Right _) = do
    logDebug hLogger "Read script"
    script <- BC.readFile "sql/insert_categories.sql"
    let q = toQuery script
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Inserting categories"
    _ <- execute_WithPool pool q
    logDebug hLogger "Categories loaded"
    return $ Right "Categories loaded"
insertCategories' _ _ (Left mess) = return $ Left mess

insertTags' ::  Handle
    -> Pool Connection
    -> Either LBS.ByteString LBS.ByteString
    -> IO (Either LBS.ByteString LBS.ByteString)
insertTags' hLogger pool (Right _)= do
    logDebug hLogger "Read script"
    script <- BC.readFile "sql/insert_tags.sql"
    let q = toQuery script
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Inserting tags"
    _ <- execute_WithPool pool q
    logDebug hLogger "Tags loaded"
    return $ Right "Tags loaded"
insertTags'  _ _ (Left mess) = return $ Left mess

insertDrafts' ::  Handle
    -> Pool Connection
    -> Either LBS.ByteString LBS.ByteString
    -> IO (Either LBS.ByteString LBS.ByteString)
insertDrafts' hLogger pool (Right _) = do
    logDebug hLogger "Read script"
    script <- BC.readFile "sql/insert_drafts.sql"
    let q = toQuery script
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Inserting drafts"
    _ <- execute_WithPool pool q
    logDebug hLogger "Drafts loaded"
    return $ Right "Drafts loaded"
insertDrafts' _ _ (Left mess) = return $ Left mess

insertNews' ::  Handle
    -> Pool Connection
    -> Either LBS.ByteString LBS.ByteString
    -> IO (Either LBS.ByteString LBS.ByteString)
insertNews' hLogger pool (Right _) = do
    logDebug hLogger "Read script"
    script <- BC.readFile "sql/insert_news.sql"
    let q = toQuery script
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Inserting news"
    _ <- execute_WithPool pool q
    logDebug hLogger "News loaded"
    return $ Right "Database created and filled tests data"
insertNews' _ _ (Left mess) = return $ Left mess

insertComments'  :: Handle
    -> Pool Connection
    -> Either LBS.ByteString LBS.ByteString
    -> IO (Either LBS.ByteString LBS.ByteString)
insertComments' hLogger pool (Right _) = do
    logDebug hLogger "Read script"
    script <- BC.readFile "sql/insert_comments.sql"
    let q = toQuery script
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Inserting comments"
    _ <- execute_WithPool pool q
    logDebug hLogger "Comments loaded"
    return $ Right "Database created and filled tests data"
insertComments' _ _ (Left mess) = return $ Left mess