{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Databaseoperations.Tags where

import           Control.Exception             (try)
import           Control.Monad.Except          (MonadError (throwError),
                                                MonadIO (..))
import           Data.Maybe                    (fromMaybe)
import           Data.Pool                     (Pool)
import qualified Data.Text                     as T
import           Database.PostgreSQL.Simple    (Connection, Only (..),
                                                SqlError (sqlState))
import           Databaseoperations.CheckAdmin (checkAdmin, checkAdmin')
import           HelpFunction                  (pageToBS, readByteStringToInt,
                                                toQuery)
import           Logger                        (LoggerHandle, logError, logInfo)
import           PostgreSqlWithPool            (executeWithPool, queryWithPool,
                                                query_WithPool)
import           Types.Other                   (Page,
                                                SomeError (DatabaseError, NotAdmin, OtherError),
                                                Token, TokenLifeTime)
import           Types.Tags                    (EditTag (..),
                                                TagName (getTagName),
                                                TagsList (TagsList))

createTagInDb ::
       (MonadIO m, MonadError SomeError m)
    => Pool Connection
    -> TokenLifeTime
    -> LoggerHandle IO
    -> Maybe Token
    -> Maybe TagName
    -> m Int
createTagInDb _ _ hLogger _ Nothing = do
    liftIO $ logError hLogger "Tag not created. No tag_name parameter"
    throwError $ OtherError "No tag_name parameter"
createTagInDb pool tokenLifeTime hLogger token (Just tagName) = do
    ch <- checkAdmin' hLogger pool tokenLifeTime token
    if ch
        then (do rows <-
                     liftIO $
                     try
                         (queryWithPool
                              pool
                              "insert into tags (tag_name) values (?) returning tag_id"
                              [tagName])
                 case rows of
                     Left e -> errorHandle e
                     Right [] -> do
                         liftIO $ logError hLogger "Tag not created."
                         throwError $ OtherError "Tag not created."
                     Right ((Only n):_) -> do
                         return n)
        else throwError NotAdmin
  where
    errorHandle sqlError = do
        let errState = sqlState sqlError
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        case errStateInt of
            23505 -> do
                liftIO $ logError hLogger "Tag already exist"
                throwError $ OtherError "Tag already exist"
            _ -> do
                liftIO $
                    logError hLogger $
                    "Database error " <> T.pack (show errStateInt)
                throwError DatabaseError

deleteTagFromDb ::
       (MonadIO m, MonadError SomeError m)
    => Pool Connection
    -> TokenLifeTime
    -> LoggerHandle IO
    -> Maybe Token
    -> Maybe TagName
    -> m ()
deleteTagFromDb _ _ hLogger _ Nothing = do
    liftIO $ logError hLogger "Tag not deleted. No tag_name parameter"
    throwError $ OtherError "Tag not deleted. No tag_name parameter"
deleteTagFromDb pool tokenLifeTime hLogger token (Just tagName) = do
    liftIO $ logInfo hLogger $ T.concat ["Deleting tag ", getTagName tagName]
    ch <- checkAdmin' hLogger pool tokenLifeTime token
    if ch
        then (do n <-
                     liftIO $
                     try
                         (executeWithPool
                              pool
                              "delete from tags where tag_name = ?"
                              [tagName])
                 case n of
                     Left e -> errorHandle e
                     Right k ->
                         if k > 0
                             then do
                                 liftIO $
                                     logInfo hLogger $
                                     "Tag " <> getTagName tagName <> " deleted"
                                 return ()
                             else do
                                 liftIO $
                                     logError hLogger $
                                     "Tag " <>
                                     getTagName tagName <> " not deleted"
                                 throwError $ OtherError "Tag not deleted")
        else throwError NotAdmin
  where
    errorHandle sqlError = do
        let errState = sqlState sqlError
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        liftIO $
            logError hLogger $ "Database error " <> T.pack (show errStateInt)
        throwError DatabaseError

-----------------------------------------------------------------------------------------------------
getTagsListFromDb ::
       (MonadIO m, MonadError SomeError m)
    => Pool Connection
    -> LoggerHandle IO
    -> Maybe Page
    -> m TagsList
getTagsListFromDb pool hLogger page = do
    rows <- liftIO $ try (query_WithPool pool q)
    case rows of
        Left (_ :: SqlError) -> do
            liftIO $ logError hLogger "Database error"
            throwError DatabaseError
        Right tagList -> do
            liftIO $ logInfo hLogger "List of tags sended"
            return $ TagsList tagList
  where
    q = toQuery $ "select tag_name from tags order by tag_name" <> pageToBS page

---------------------------------------------------------------------------------
editTagInDb ::
       (MonadIO m, MonadError SomeError m)
    => Pool Connection
    -> TokenLifeTime
    -> LoggerHandle IO
    -> Maybe Token
    -> EditTag
    -> m ()
editTagInDb _ _ hLogger _ EditTag {editTagNewName = Nothing} = do
    liftIO $ logError hLogger "Tag not edited. No new_tag_name field"
    throwError $ OtherError "No new_tag_name field"
editTagInDb _ _ hLogger _ EditTag {editTagOldName = Nothing} = do
    liftIO $ logError hLogger "Tag not edited. No old_tag_name field"
    throwError $ OtherError "Tag not edited. No old_tag_name field"
editTagInDb _ _ hLogger Nothing _ = do
    liftIO $ logError hLogger "Tag not edited. No token param"
    throwError $ OtherError "No token param"
editTagInDb pool tokenLifetime hLogger token editTagParams@(EditTag (Just newTagName) (Just oldTagName)) = do
    ch <- checkAdmin' hLogger pool tokenLifetime token
    if ch
        then do
            n <-
                liftIO $
                try
                    (executeWithPool
                         pool
                         "update tags set tag_name = ? where tag_name = ?"
                         editTagParams)
            case n of
                Left e -> errorHandle e
                Right k ->
                    if k > 0
                        then do
                            liftIO $
                                logInfo hLogger $
                                "Tag '" <>
                                getTagName oldTagName <>
                                "' renaimed to '" <>
                                getTagName newTagName <> "'"
                            return ()
                        else do
                            liftIO $ logError hLogger "Tag not exist"
                            throwError $ OtherError "Tag not exist"
        else throwError NotAdmin
  where
    errorHandle sqlError = do
        let errState = sqlState sqlError
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        liftIO $
            logError hLogger $ "Database error " <> T.pack (show errStateInt)
        throwError DatabaseError
