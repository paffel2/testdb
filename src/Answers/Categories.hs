{-# LANGUAGE OverloadedStrings #-}

module Answers.Categories where

import           Answer                    (AnswerHandle (..))
import           Data.Aeson                (encode)
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as LBS
import           FromRequest               (takeToken, toCategoryName,
                                            toCreateCategory, toEditCategory,
                                            toPage)
import           Logger                    (logError, logInfo)
import           Network.HTTP.Types.Method (methodDelete, methodGet, methodPost,
                                            methodPut)
import           Network.Wai               (Request (requestMethod))
import           OperationsHandle          (CategoriesHandle (chCreateCategoryOnDb, chDeleteCategoryFromDb, chEditCategoryOnDb, chGetCategoriesListFromDb, chLogger, chParseRequestBody))
import           Responses                 (toResponseErrorMessage)
import           Types.Categories          (CategoryName, CreateCategory,
                                            EditCategory, ListOfCategories)
import           Types.Other               (Page, ResponseErrorMessage,
                                            ResponseOkMessage (Created, OkJSON, OkMessage),
                                            SendId, SomeError (BadMethod),
                                            Token)

updateCategoryParseInformation ::
       Monad m
    => CategoriesHandle m
    -> Request
    -> m (Either SomeError (Maybe Token, EditCategory))
updateCategoryParseInformation handler request =
    if requestMethod request /= methodPut
        then do
            logError (chLogger handler) "Bad request method"
            return $ Left BadMethod
        else do
            logInfo (chLogger handler) "Preparing data for editing category"
            let token = takeToken request
            (i, _) <- chParseRequestBody handler request
            let editCategoryParameters = toEditCategory i
            return $ Right (token, editCategoryParameters)

updateCategoryDatabaseOperation ::
       Monad m
    => CategoriesHandle m
    -> Either SomeError (Maybe Token, EditCategory)
    -> m (Either SomeError ())
updateCategoryDatabaseOperation _ (Left message) = return $ Left message
updateCategoryDatabaseOperation categoryHandle (Right (token, editInfo)) =
    chEditCategoryOnDb categoryHandle token editInfo

updateCategorySendResult ::
       Monad m
    => Either SomeError ()
    -> m (Either ResponseErrorMessage ResponseOkMessage)
updateCategorySendResult result =
    case result of
        Left someError ->
            return $
            Left $ toResponseErrorMessage "Category not edited." someError
        Right _ -> do
            return $ Right $ OkMessage "Category edited."

updateCategoryHandle ::
       Monad m
    => CategoriesHandle m
    -> AnswerHandle m (Either SomeError (Maybe Token, EditCategory)) ()
updateCategoryHandle authorHandle =
    AnswerHandle
        { parseInformation = updateCategoryParseInformation authorHandle
        , databaseOperation = updateCategoryDatabaseOperation authorHandle
        , sendResult = updateCategorySendResult
        }

--------------------------------------------------------------------------------------------------------------------
getCategoriesListParseInformation ::
       Monad m
    => CategoriesHandle m
    -> Request
    -> m (Either SomeError (Maybe Page))
getCategoriesListParseInformation handler request =
    if requestMethod request == methodGet
        then do
            logInfo
                (chLogger handler)
                "Preparing data for sending categories list"
            return $ Right $ toPage request
        else do
            logError (chLogger handler) "Bad request method"
            return $ Left BadMethod

getCategoriesListDatabaseOperation ::
       Monad m
    => CategoriesHandle m
    -> Either SomeError (Maybe Page)
    -> m (Either SomeError ListOfCategories)
getCategoriesListDatabaseOperation _ (Left message) = return $ Left message
getCategoriesListDatabaseOperation categoryHandle (Right page) =
    chGetCategoriesListFromDb categoryHandle page

getCategoriesListSendResult ::
       Monad m
    => Either SomeError ListOfCategories
    -> m (Either ResponseErrorMessage ResponseOkMessage)
getCategoriesListSendResult result =
    case result of
        Left someError ->
            return $
            Left $
            toResponseErrorMessage "List of categories not sended." someError
        Right loc -> do
            return $ Right $ OkJSON $ encode loc

getCategoriesListHandle ::
       Monad m
    => CategoriesHandle m
    -> AnswerHandle m (Either SomeError (Maybe Page)) ListOfCategories
getCategoriesListHandle authorHandle =
    AnswerHandle
        { parseInformation = getCategoriesListParseInformation authorHandle
        , databaseOperation = getCategoriesListDatabaseOperation authorHandle
        , sendResult = getCategoriesListSendResult
        }

----------------------------------------------------------------------------------------------------------------
createCategoryParseInformation ::
       Monad m
    => CategoriesHandle m
    -> Request
    -> m (Either SomeError (Maybe Token, CreateCategory))
createCategoryParseInformation handler request =
    if requestMethod request /= methodPost
        then do
            logError (chLogger handler) "Bad request method"
            return $ Left BadMethod
        else do
            logInfo (chLogger handler) "Preparing data for creating category"
            (i, _) <- chParseRequestBody handler request
            let token = takeToken request
            let createCategoryParams = toCreateCategory i
            return $ Right (token, createCategoryParams)

createCategoryDatabaseOperation ::
       Monad m
    => CategoriesHandle m
    -> Either SomeError (Maybe Token, CreateCategory)
    -> m (Either SomeError SendId)
createCategoryDatabaseOperation _ (Left message) = return $ Left message
createCategoryDatabaseOperation categoryHandle (Right (token, createCategoryParams)) =
    chCreateCategoryOnDb categoryHandle token createCategoryParams

createCategorySendResult ::
       Monad m
    => Either SomeError SendId
    -> m (Either ResponseErrorMessage ResponseOkMessage)
createCategorySendResult result =
    case result of
        Left someError ->
            return $
            Left $ toResponseErrorMessage "Category not created." someError
        Right categoryId -> do
            return $
                Right $ Created $ LBS.fromStrict $ BC.pack $ show categoryId

createCategoryHandle ::
       Monad m
    => CategoriesHandle m
    -> AnswerHandle m (Either SomeError (Maybe Token, CreateCategory)) SendId
createCategoryHandle authorHandle =
    AnswerHandle
        { parseInformation = createCategoryParseInformation authorHandle
        , databaseOperation = createCategoryDatabaseOperation authorHandle
        , sendResult = createCategorySendResult
        }

-----------------------------------------------------------------------------------------------------------------
deleteCategoryParseInformation ::
       Monad m
    => CategoriesHandle m
    -> Request
    -> m (Either SomeError (Maybe Token, Maybe CategoryName))
deleteCategoryParseInformation handler request =
    if requestMethod request /= methodDelete
        then do
            logError (chLogger handler) "Bad request method"
            return $ Left BadMethod
        else do
            logInfo (chLogger handler) "Preparing data for deleting category"
            let token = takeToken request
            (i, _) <- chParseRequestBody handler request
            let categoryName = toCategoryName i
            return $ Right (token, categoryName)

deleteCategoryDatabaseOperation ::
       Monad m
    => CategoriesHandle m
    -> Either SomeError (Maybe Token, Maybe CategoryName)
    -> m (Either SomeError ())
deleteCategoryDatabaseOperation _ (Left message) = return $ Left message
deleteCategoryDatabaseOperation categoryHandle (Right (token, categoryName)) =
    chDeleteCategoryFromDb categoryHandle token categoryName

deleteCategoryDatabaseOperationCategorySendResult ::
       Monad m
    => Either SomeError ()
    -> m (Either ResponseErrorMessage ResponseOkMessage)
deleteCategoryDatabaseOperationCategorySendResult result =
    case result of
        Left someError ->
            return $
            Left $ toResponseErrorMessage "Category not deleted." someError
        Right _ -> do
            return $ Right $ OkMessage "Category deleted."

deleteCategoryHandle ::
       Monad m
    => CategoriesHandle m
    -> AnswerHandle m (Either SomeError (Maybe Token, Maybe CategoryName)) ()
deleteCategoryHandle authorHandle =
    AnswerHandle
        { parseInformation = deleteCategoryParseInformation authorHandle
        , databaseOperation = deleteCategoryDatabaseOperation authorHandle
        , sendResult = deleteCategoryDatabaseOperationCategorySendResult
        }
