{-# LANGUAGE OverloadedStrings #-}

module Controllers.Categories where

import           Control.Monad.IO.Class    (MonadIO (..))
import           Data.Aeson                (encode)
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as LBS
import           FromRequest               (takeToken, toCategoryName,
                                            toCreateCategory, toEditCategory,
                                            toPage)
import           Logger                    (logError, logInfo)
import           Network.HTTP.Types.Method (methodDelete, methodGet, methodPost,
                                            methodPut)
import           Network.Wai               (Request (rawPathInfo, requestMethod),
                                            Response)
import           Network.Wai.Parse         (lbsBackEnd, parseRequestBody)
import           OperationsHandle          (CategoriesHandle (categories_logger, create_category_on_db, delete_category_from_db, edit_category_on_db, get_categories_list_from_db))
import           Responses                 (badResponse, responseCreated,
                                            responseMethodNotAllowed,
                                            responseNotFound, responseOKJSON,
                                            responseOk)

sendCategoriesList :: MonadIO m => CategoriesHandle m -> Request -> m Response
sendCategoriesList operations req =
    if requestMethod req == methodGet
        then do
            logInfo
                (categories_logger operations)
                "Preparing data for sending categories list"
            result <-
                get_categories_list_from_db
                    operations
                    (categories_logger operations)
                    pageParam
            case result of
                Left someError -> return $ badResponse "" someError
                Right loc -> do
                    return $ responseOKJSON $ encode loc
        else do
            logError (categories_logger operations) "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
  where
    pageParam = toPage req

createCategory :: MonadIO m => CategoriesHandle m -> Request -> m Response
createCategory operations req =
    if requestMethod req /= methodPost
        then do
            logError (categories_logger operations) "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo
                (categories_logger operations)
                "Preparing data for creating category"
            (i, _) <- liftIO $ parseRequestBody lbsBackEnd req
            let token' = takeToken req
            let create_category_params = toCreateCategory i
            result <-
                create_category_on_db
                    operations
                    (categories_logger operations)
                    token'
                    create_category_params
            case result of
                Left someError ->
                    return $ badResponse "Category not created." someError
                Right category_id -> do
                    logInfo (categories_logger operations) "Category created."
                    return $
                        responseCreated $
                        LBS.fromStrict $ BC.pack $ show category_id

deleteCategory :: MonadIO m => CategoriesHandle m -> Request -> m Response
deleteCategory operations req =
    if requestMethod req /= methodDelete
        then do
            logError (categories_logger operations) "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo
                (categories_logger operations)
                "Preparing data for deleting category"
            let token' = takeToken req
            (i, _) <- liftIO $ parseRequestBody lbsBackEnd req
            let category_name = toCategoryName i
            result <-
                delete_category_from_db
                    operations
                    (categories_logger operations)
                    token'
                    category_name
            case result of
                Left someError ->
                    return $ badResponse "Category not deleted." someError
                Right _ -> do
                    return $ responseOk "Category deleted."

editCategory :: MonadIO m => CategoriesHandle m -> Request -> m Response
editCategory operations req = do
    if requestMethod req /= methodPut
        then do
            logError (categories_logger operations) "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo
                (categories_logger operations)
                "Preparing data for editing category"
            let token' = takeToken req
            (i, _) <- liftIO $ parseRequestBody lbsBackEnd req
            let edit_category_parameters = toEditCategory i
            result <-
                edit_category_on_db
                    operations
                    (categories_logger operations)
                    token'
                    edit_category_parameters
            case result of
                Left someError ->
                    return $ badResponse "Category not edited." someError
                Right _ -> do
                    return $ responseCreated "Category edited."

categoriesRouter :: MonadIO m => CategoriesHandle m -> Request -> m Response
categoriesRouter operations req
    | pathElemsC == 1 = sendCategoriesList operations req
    | pathElemsC == 2 =
        case last pathElems of
            "delete_category" -> deleteCategory operations req
            "create_category" -> createCategory operations req
            "edit_category"   -> editCategory operations req
            _                 -> return $ responseNotFound "Not Found"
    | otherwise = return $ responseNotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
