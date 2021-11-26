{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Answers.Categories where

import           Answer                    (AnswerHandle (..))
import           Control.Monad.Except      (MonadError (throwError))
import           FromRequest               (takeToken, toCategoryName,
                                            toCreateCategory, toEditCategory,
                                            toPage)
import           Network.HTTP.Types.Method (methodDelete, methodGet, methodPost,
                                            methodPut)
import           Network.Wai               (Request (requestMethod))
import           OperationsHandle          (CategoriesHandle (chCreateCategoryOnDb, chDeleteCategoryFromDb, chEditCategoryOnDb, chGetCategoriesListFromDb, chParseRequestBody))
import           Types.Categories          (CategoryName, CreateCategory,
                                            EditCategory, ListOfCategories)
import           Types.Other               (MonadIOWithError, Page, SendId,
                                            SomeError (BadMethod), Token)

getCategoriesListParseInformation ::
       MonadIOWithError m => CategoriesHandle m -> Request -> m (Maybe Page)
getCategoriesListParseInformation _ request =
    if requestMethod request == methodGet
        then return $ toPage request
        else throwError BadMethod

getCategoriesListDatabaseOperation ::
       MonadIOWithError m
    => CategoriesHandle m
    -> Maybe Page
    -> m ListOfCategories
getCategoriesListDatabaseOperation = chGetCategoriesListFromDb

getCategoriesListHandle ::
       MonadIOWithError m
    => CategoriesHandle m
    -> AnswerHandle m (Maybe Page) ListOfCategories
getCategoriesListHandle authorHandle =
    AnswerHandle
        { parseInformation = getCategoriesListParseInformation authorHandle
        , databaseOperation = getCategoriesListDatabaseOperation authorHandle
        }

----------------------------------------------------------------------------------
createCategoryParseInformation ::
       MonadIOWithError m
    => CategoriesHandle m
    -> Request
    -> m (Maybe Token, CreateCategory)
createCategoryParseInformation handler request =
    if requestMethod request /= methodPost
        then throwError BadMethod
        else do
            (i, _) <- chParseRequestBody handler request
            let token = takeToken request
            let createCategoryParams = toCreateCategory i
            return (token, createCategoryParams)

createCategoryDatabaseOperation ::
       MonadIOWithError m
    => CategoriesHandle m
    -> (Maybe Token, CreateCategory)
    -> m SendId
createCategoryDatabaseOperation categoryHandle (token, createCategoryParams) =
    chCreateCategoryOnDb categoryHandle token createCategoryParams

createCategoryHandle ::
       MonadIOWithError m
    => CategoriesHandle m
    -> AnswerHandle m (Maybe Token, CreateCategory) SendId
createCategoryHandle authorHandle =
    AnswerHandle
        { parseInformation = createCategoryParseInformation authorHandle
        , databaseOperation = createCategoryDatabaseOperation authorHandle
        }

deleteCategoryParseInformation ::
       MonadIOWithError m
    => CategoriesHandle m
    -> Request
    -> m (Maybe Token, Maybe CategoryName)
deleteCategoryParseInformation handler request =
    if requestMethod request /= methodDelete
        then throwError BadMethod
        else do
            let token = takeToken request
            (i, _) <- chParseRequestBody handler request
            let categoryName = toCategoryName i
            return (token, categoryName)

deleteCategoryDatabaseOperation ::
       MonadIOWithError m
    => CategoriesHandle m
    -> (Maybe Token, Maybe CategoryName)
    -> m ()
deleteCategoryDatabaseOperation categoryHandle (token, categoryName) =
    chDeleteCategoryFromDb categoryHandle token categoryName

deleteCategoryHandle ::
       MonadIOWithError m
    => CategoriesHandle m
    -> AnswerHandle m (Maybe Token, Maybe CategoryName) ()
deleteCategoryHandle authorHandle =
    AnswerHandle
        { parseInformation = deleteCategoryParseInformation authorHandle
        , databaseOperation = deleteCategoryDatabaseOperation authorHandle
        }

---------------------------------------------------------------------------------------
updateCategoryParseInformation ::
       MonadIOWithError m
    => CategoriesHandle m
    -> Request
    -> m (Maybe Token, EditCategory)
updateCategoryParseInformation handler request =
    if requestMethod request /= methodPut
        then throwError BadMethod
        else do
            let token = takeToken request
            (i, _) <- chParseRequestBody handler request
            let editCategoryParameters = toEditCategory i
            return (token, editCategoryParameters)

updateCategoryDatabaseOperation ::
       MonadIOWithError m
    => CategoriesHandle m
    -> (Maybe Token, EditCategory)
    -> m ()
updateCategoryDatabaseOperation categoryHandle (token, editInfo) =
    chEditCategoryOnDb categoryHandle token editInfo

updateCategoryHandle ::
       MonadIOWithError m
    => CategoriesHandle m
    -> AnswerHandle m (Maybe Token, EditCategory) ()
updateCategoryHandle authorHandle =
    AnswerHandle
        { parseInformation = updateCategoryParseInformation authorHandle
        , databaseOperation = updateCategoryDatabaseOperation authorHandle
        }
