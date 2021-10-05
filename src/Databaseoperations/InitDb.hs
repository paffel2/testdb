{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.InitDb where

import Control.Exception (catch)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Pool (Pool)
import qualified Data.Text.Encoding as E
import Database.PostgreSQL.Simple
    ( Binary(Binary)
    , Connection
    , SqlError(sqlErrorMsg)
    )

import HelpFunction (getFiles, toQuery)
import Logger (Handle, logDebug, logError)
import PostgreSqlWithPool (executeWithPool, execute_WithPool)

createDb ::
       Handle IO -> Pool Connection -> IO (Either LBS.ByteString LBS.ByteString)
createDb hLogger pool =
    catch
        (do fillDb hLogger pool >>= fillConnections hLogger pool >>=
                fillImages hLogger pool >>=
                insertUsers hLogger pool >>=
                insertAuthors hLogger pool >>=
                insertCategories hLogger pool >>=
                insertTags hLogger pool >>=
                insertDrafts hLogger pool >>=
                insertNews hLogger pool >>=
                insertComments hLogger pool) $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        logError hLogger err
        return $ Left $ LBS.fromStrict $ sqlErrorMsg e

fillDb ::
       Handle IO -> Pool Connection -> IO (Either LBS.ByteString LBS.ByteString)
fillDb hLogger pool = do
    logDebug hLogger "Read script"
    script <- BC.readFile "sql/fill_database.sql"
    let q = toQuery script
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Start filling"
    _ <- execute_WithPool pool q
    logDebug hLogger "Db filled"
    return $ Right "Database filled"

fillConnections ::
       Handle IO
    -> Pool Connection
    -> Either LBS.ByteString LBS.ByteString
    -> IO (Either LBS.ByteString LBS.ByteString)
fillConnections hLogger pool (Right _) = do
    logDebug hLogger "Read script"
    script <- BC.readFile "sql/fill_connections.sql"
    let q = toQuery script
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Start filling connections"
    _ <- execute_WithPool pool q
    logDebug hLogger "Connections created"
    return $ Right "Connections created"
fillConnections _ _ (Left mess) = return $ Left mess

fillImages ::
       Handle IO
    -> Pool Connection
    -> Either LBS.ByteString LBS.ByteString
    -> IO (Either LBS.ByteString LBS.ByteString)
fillImages hLogger pool (Right _) = do
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
fillImages _ _ (Left mess) = return $ Left mess

insertUsers ::
       Handle IO
    -> Pool Connection
    -> Either LBS.ByteString LBS.ByteString
    -> IO (Either LBS.ByteString LBS.ByteString)
insertUsers hLogger pool (Right _) = do
    logDebug hLogger "Read script"
    script <- BC.readFile "sql/insert_users.sql"
    let q = toQuery script
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Inserting users"
    _ <- execute_WithPool pool q
    logDebug hLogger "Users loaded"
    return $ Right "Users loaded"
insertUsers _ _ (Left mess) = return $ Left mess

insertAuthors ::
       Handle IO
    -> Pool Connection
    -> Either LBS.ByteString LBS.ByteString
    -> IO (Either LBS.ByteString LBS.ByteString)
insertAuthors hLogger pool (Right _) = do
    logDebug hLogger "Read script"
    script <- BC.readFile "sql/insert_authors.sql"
    let q = toQuery script
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Inserting authors"
    _ <- execute_WithPool pool q
    logDebug hLogger "Authors loaded"
    return $ Right "Authors loaded"
insertAuthors _ _ (Left mess) = return $ Left mess

insertCategories ::
       Handle IO
    -> Pool Connection
    -> Either LBS.ByteString LBS.ByteString
    -> IO (Either LBS.ByteString LBS.ByteString)
insertCategories hLogger pool (Right _) = do
    logDebug hLogger "Read script"
    script <- BC.readFile "sql/insert_categories.sql"
    let q = toQuery script
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Inserting categories"
    _ <- execute_WithPool pool q
    logDebug hLogger "Categories loaded"
    return $ Right "Categories loaded"
insertCategories _ _ (Left mess) = return $ Left mess

insertTags ::
       Handle IO
    -> Pool Connection
    -> Either LBS.ByteString LBS.ByteString
    -> IO (Either LBS.ByteString LBS.ByteString)
insertTags hLogger pool (Right _) = do
    logDebug hLogger "Read script"
    script <- BC.readFile "sql/insert_tags.sql"
    let q = toQuery script
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Inserting tags"
    _ <- execute_WithPool pool q
    logDebug hLogger "Tags loaded"
    return $ Right "Tags loaded"
insertTags _ _ (Left mess) = return $ Left mess

insertDrafts ::
       Handle IO
    -> Pool Connection
    -> Either LBS.ByteString LBS.ByteString
    -> IO (Either LBS.ByteString LBS.ByteString)
insertDrafts hLogger pool (Right _) = do
    logDebug hLogger "Read script"
    script <- BC.readFile "sql/insert_drafts.sql"
    let q = toQuery script
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Inserting drafts"
    _ <- execute_WithPool pool q
    logDebug hLogger "Drafts loaded"
    return $ Right "Drafts loaded"
insertDrafts _ _ (Left mess) = return $ Left mess

insertNews ::
       Handle IO
    -> Pool Connection
    -> Either LBS.ByteString LBS.ByteString
    -> IO (Either LBS.ByteString LBS.ByteString)
insertNews hLogger pool (Right _) = do
    logDebug hLogger "Read script"
    script <- BC.readFile "sql/insert_news.sql"
    let q = toQuery script
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Inserting news"
    _ <- execute_WithPool pool q
    logDebug hLogger "News loaded"
    return $ Right "Database created and filled tests data"
insertNews _ _ (Left mess) = return $ Left mess

insertComments ::
       Handle IO
    -> Pool Connection
    -> Either LBS.ByteString LBS.ByteString
    -> IO (Either LBS.ByteString LBS.ByteString)
insertComments hLogger pool (Right _) = do
    logDebug hLogger "Read script"
    script <- BC.readFile "sql/insert_comments.sql"
    let q = toQuery script
    logDebug hLogger "Script readed and translated to query"
    logDebug hLogger "Inserting comments"
    _ <- execute_WithPool pool q
    logDebug hLogger "Comments loaded"
    return $ Right "Database created and filled tests data"
insertComments _ _ (Left mess) = return $ Left mess
