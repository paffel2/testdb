{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.Categories where

import           Control.Monad.Except          (MonadError (catchError, throwError))
import           Data.Pool                     (Pool)
import qualified Data.Text                     as T
import           Database.PostgreSQL.Simple    (Connection, Only (fromOnly))
import           Databaseoperations.CheckAdmin (checkAdmin)
import           HelpFunction                  (numOnlyHead, pageToBS,
                                                someErrorToInt, toQuery)
import           PostgreSqlWithPool            (executeWithPool, queryWithPool,
                                                query_WithPool,
                                                returningWithPool)
import           Types.Categories              (CategoryName (..),
                                                CreateCategory (..),
                                                EditCategory (EditCategory, editCategoryName),
                                                ListOfCategories (ListOfCategories))
import           Types.Other                   (MonadIOWithError, Page, SendId,
                                                SomeError (OtherError), Token,
                                                TokenLifeTime)

-----------------------------------------------------------------------------------------
getCategoriesListFromDb ::
       MonadIOWithError m => Pool Connection -> Maybe Page -> m ListOfCategories
getCategoriesListFromDb pool pageParam = do
    rows <- query_WithPool pool q
    return (ListOfCategories rows)
  where
    q =
        toQuery $
        "select category_name from categories order by category_name" <>
        pageToBS pageParam

----------------------------------------------------------------------------------------
createCategoryOnDb ::
       MonadIOWithError m
    => Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> CreateCategory
    -> m SendId
createCategoryOnDb _ _ _ CreateCategory {createCategroryName = Nothing} = do
    throwError $ OtherError "No category_name field"
createCategoryOnDb _ _ _ CreateCategory {createCategroryMaternalCategory = Nothing} = do
    throwError $ OtherError "No maternal_category_name field"
createCategoryOnDb pool tokenLifeTime token (CreateCategory (Just categoryName) (Just maternalName)) = do
    checkAdmin pool tokenLifeTime token
    catchError
        (if getCategoryName maternalName == ""
             then createWithoutMaternal
             else createWithMaternal) $ \e ->
        case someErrorToInt e of
            23505 ->
                throwError $
                OtherError "Category not created. Category already exist"
            _ -> throwError e
  where
    q =
        "insert into categories (category_name, maternal_category) values (?,?) returning category_id"
    checkMaternal = "select category_id from categories where category_name = ?"
    createWithoutMaternal = do
        rows <-
            returningWithPool pool q [(categoryName, Nothing :: Maybe T.Text)]
        return (fromOnly $ Prelude.head rows)
    createWithMaternal = do
        catId <- queryWithPool pool checkMaternal [maternalName]
        if Prelude.null (catId :: [Only SendId])
            then throwError $ OtherError "Maternal category not exist"
            else do
                rows <-
                    returningWithPool
                        pool
                        q
                        [(categoryName, fromOnly $ Prelude.head catId)]
                return (fromOnly $ Prelude.head rows)

deleteCategoryFromDb ::
       MonadIOWithError m
    => Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> Maybe CategoryName
    -> m ()
deleteCategoryFromDb _ _ _ Nothing = do
    throwError $ OtherError "Category not deleted.No category_name parametr"
deleteCategoryFromDb pool tokenLifeTime token (Just categoryName) = do
    checkAdmin pool tokenLifeTime token
    n <-
        executeWithPool
            pool
            "delete from categories where category_name = ?"
            [categoryName]
    if n > 0
        then return ()
        else throwError $ OtherError "Category not exist"

editCategory ::
       MonadIOWithError m
    => Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> EditCategory
    -> m ()
editCategory _ _ _ EditCategory {editCategoryName = Nothing} =
    throwError $ OtherError "Category not edited. No old_name parametr"
editCategory _ _ _ (EditCategory (Just (CategoryName "")) (Just _) (Just _)) =
    throwError $ OtherError "Empty old name parameter"
editCategory pool tokenLifeTime token (EditCategory (Just oldName) (Just newName) (Just (CategoryName ""))) = do
    checkAdmin pool tokenLifeTime token
    n <-
        executeWithPool
            pool
            "update categories set category_name = ? where category_name = ?"
            (newName, oldName)
    if n > 0
        then return ()
        else throwError $ OtherError "Category not exist"
editCategory pool tokenLifeTime token (EditCategory (Just oldName) (Just (CategoryName "")) (Just newMaternal)) = do
    checkAdmin pool tokenLifeTime token
    maternalId <-
        queryWithPool
            pool
            "select category_id from categories where category_name = ?"
            [newMaternal]
    n <-
        executeWithPool
            pool
            "update categories set maternal_category = ? where category_name = ?"
            (fromOnly $ numOnlyHead (maternalId :: [Only Int]), oldName)
    if n > 0
        then return ()
        else throwError $ OtherError "Category not exist"
editCategory pool tokenLifeTime token (EditCategory (Just oldName) (Just newName) (Just newMaternal)) = do
    checkAdmin pool tokenLifeTime token
    maternalId <-
        queryWithPool
            pool
            "select category_id from categories where category_name = ?"
            [newMaternal]
    n <-
        executeWithPool
            pool
            "update categories set category_name = ?, maternal_category = ? where category_name = ?"
            ( newName
            , fromOnly $ numOnlyHead (maternalId :: [Only Int])
            , oldName)
    if n > 0
        then return ()
        else throwError $ OtherError "Category not exist"
editCategory _ _ _ _ = throwError $ OtherError "No update parameters"

editCategoryOnDb ::
       MonadIOWithError m
    => Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> EditCategory
    -> m ()
editCategoryOnDb pool tokenLifeTime token editParams =
    catchError (editCategoryOnDb pool tokenLifeTime token editParams) $ \e -> do
        case someErrorToInt e of
            23505 -> throwError $ OtherError "Category already exist"
            23503 -> throwError $ OtherError "Maternal category not exist"
            _     -> throwError e
