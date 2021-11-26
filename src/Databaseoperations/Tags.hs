{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Databaseoperations.Tags where

import           Control.Monad.Except          (MonadError (catchError, throwError))
import           Data.Pool                     (Pool)
import           Database.PostgreSQL.Simple    (Connection, Only (fromOnly))
import           Databaseoperations.CheckAdmin (checkAdmin)
import           HelpFunction                  (pageToBS, someErrorToInt,
                                                toQuery)
import           PostgreSqlWithPool            (executeWithPool, queryWithPool,
                                                query_WithPool)
import           Types.Other                   (MonadIOWithError, Page,
                                                SomeError (OtherError), Token,
                                                TokenLifeTime)
import           Types.Tags                    (EditTag (..), TagName,
                                                TagsList (TagsList))

-----------------------------------------------------------------------------------------------------
createTagInDb ::
       MonadIOWithError m
    => Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> Maybe TagName
    -> m Int
createTagInDb _ _ _ Nothing = do
    throwError $ OtherError "No tag_name parameter"
createTagInDb pool tokenLifeTime token (Just tagName) = do
    checkAdmin pool tokenLifeTime token
    rows <-
        catchError
            (queryWithPool
                 pool
                 "insert into tags (tag_name) values (?) returning tag_id"
                 [tagName]) $ \e ->
            case someErrorToInt e of
                23505 -> throwError $ OtherError "Tag already exist"
                _     -> throwError e
    if Prelude.null rows
        then throwError $ OtherError "Tag not created."
        else return $ fromOnly $ Prelude.head rows

----------------------------------------------------------------------------
deleteTagFromDb ::
       MonadIOWithError m
    => Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> Maybe TagName
    -> m ()
deleteTagFromDb _ _ _ Nothing = do
    throwError $ OtherError "Tag not deleted. No tag_name parameter"
deleteTagFromDb pool tokenLifeTime token (Just tagName) = do
    checkAdmin pool tokenLifeTime token
    n <- executeWithPool pool "delete from tags where tag_name = ?" [tagName]
    if n > 0
        then return ()
        else throwError $ OtherError "Tag not deleted"

-----------------------------------------------------------------------------
getTagsListFromDb ::
       MonadIOWithError m => Pool Connection -> Maybe Page -> m TagsList
getTagsListFromDb pool page = do
    rows <- query_WithPool pool q
    return $ TagsList rows
  where
    q = toQuery $ "select tag_name from tags order by tag_name" <> pageToBS page

---------------------------------------------------------------------------------
editTagInDb ::
       MonadIOWithError m
    => Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> EditTag
    -> m ()
editTagInDb _ _ _ EditTag {editTagNewName = Nothing} = do
    throwError $ OtherError "No new_tag_name field"
editTagInDb _ _ _ EditTag {editTagOldName = Nothing} = do
    throwError $ OtherError "Tag not edited. No old_tag_name field"
editTagInDb _ _ Nothing _ = do
    throwError $ OtherError "No token param"
editTagInDb pool tokenLifetime token editTagParams =
    catchError
        (do checkAdmin pool tokenLifetime token
            n <-
                executeWithPool
                    pool
                    "update tags set tag_name = ? where tag_name = ?"
                    editTagParams
            if n > 0
                then return ()
                else throwError $ OtherError "Tag not exist") $ \e ->
        case someErrorToInt e of
            23505 -> throwError $ OtherError "Tag already exist"
            _     -> throwError e
