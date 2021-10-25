{-# LANGUAGE OverloadedStrings #-}

module Controllers.Categories where

import           Control.Monad.IO.Class    (MonadIO (..))
import           Data.Aeson                (encode)
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as LBS
import           FromRequest               (takeToken, toCategoryName,
                                            toCreateCategory, toEditCategory,
                                            toPage)
import           Logger                    (LoggerHandle, logError, logInfo)
import           Network.HTTP.Types.Method (methodDelete, methodGet, methodPost,
                                            methodPut)
import           Network.Wai               (Request (rawPathInfo, requestMethod),
                                            Response)
import           Network.Wai.Parse         (lbsBackEnd, parseRequestBody)
import           OperationsHandle          (CategoriesHandle (create_category_on_db, delete_category_from_db, edit_category_on_db, get_categories_list_from_db))
import           Responses                 (badResponse, responseCreated,
                                            responseMethodNotAllowed,
                                            responseNotFound, responseOKJSON,
                                            responseOk)
import           Types.Other               (TokenLifeTime)

sendCategoriesList ::
       MonadIO m
    => LoggerHandle m
    -> CategoriesHandle m
    -> Request
    -> m Response
sendCategoriesList hLogger operations req =
    if requestMethod req == methodGet
        then do
            logInfo hLogger "Preparing data for sending categories list"
            result <- get_categories_list_from_db operations hLogger pageParam
            case result of
                Left someError -> return $ badResponse "" someError
                Right loc -> do
                    return $ responseOKJSON $ encode loc
        else do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
  where
    pageParam = toPage req

createCategory ::
       MonadIO m
    => LoggerHandle m
    -> CategoriesHandle m
    -> TokenLifeTime
    -> Request
    -> m Response
createCategory hLogger operations token_lifetime req =
    if requestMethod req /= methodPost
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo hLogger "Preparing data for creating category"
            (i, _) <- liftIO $ parseRequestBody lbsBackEnd req
            let token' = takeToken req
            let create_category_params = toCreateCategory i
            result <-
                create_category_on_db
                    operations
                    hLogger
                    token_lifetime
                    token'
                    create_category_params
            case result of
                Left someError ->
                    return $ badResponse "Category not created." someError
                Right category_id -> do
                    logInfo hLogger "Category created."
                    return $
                        responseCreated $
                        LBS.fromStrict $ BC.pack $ show category_id

deleteCategory ::
       MonadIO m
    => LoggerHandle m
    -> CategoriesHandle m
    -> TokenLifeTime
    -> Request
    -> m Response
deleteCategory hLogger operations token_lifetime req =
    if requestMethod req /= methodDelete
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo hLogger "Preparing data for deleting category"
            let token' = takeToken req
            (i, _) <- liftIO $ parseRequestBody lbsBackEnd req
            let category_name = toCategoryName i
            result <-
                delete_category_from_db
                    operations
                    hLogger
                    token_lifetime
                    token'
                    category_name
            case result of
                Left someError ->
                    return $ badResponse "Category not deleted." someError
                Right _ -> do
                    return $ responseOk "Category deleted."

editCategory ::
       MonadIO m
    => LoggerHandle m
    -> CategoriesHandle m
    -> TokenLifeTime
    -> Request
    -> m Response
editCategory hLogger operations token_lifetime req = do
    if requestMethod req /= methodPut
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo hLogger "Preparing data for editing category"
            let token' = takeToken req
            (i, _) <- liftIO $ parseRequestBody lbsBackEnd req
            let edit_category_parameters = toEditCategory i
            result <-
                edit_category_on_db
                    operations
                    hLogger
                    token_lifetime
                    token'
                    edit_category_parameters
            case result of
                Left someError ->
                    return $ badResponse "Category not edited." someError
                Right _ -> do
                    return $ responseCreated "Category edited."

categoriesRouter ::
       MonadIO m
    => LoggerHandle m
    -> CategoriesHandle m
    -> TokenLifeTime
    -> Request
    -> m Response
categoriesRouter hLogger operations token_lifetime req
    | pathElemsC == 1 = sendCategoriesList hLogger operations req
    | pathElemsC == 2 =
        case last pathElems of
            "delete_category" ->
                deleteCategory hLogger operations token_lifetime req
            "create_category" ->
                createCategory hLogger operations token_lifetime req
            "edit_category" ->
                editCategory hLogger operations token_lifetime req
            _ -> return $ responseNotFound "Not Found"
    | otherwise = return $ responseNotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
