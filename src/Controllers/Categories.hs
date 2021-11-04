{-# LANGUAGE OverloadedStrings #-}

module Controllers.Categories where

import           Data.Aeson                (encode)
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as LBS
import           FromRequest               (takeToken, toCategoryName,
                                            toCreateCategory, toEditCategory,
                                            toPage)
import           Logger                    (logError, logInfo)
import           Network.HTTP.Types.Method (methodDelete, methodGet, methodPost,
                                            methodPut)
import           Network.Wai               (Request (rawPathInfo, requestMethod))
import           OperationsHandle          (CategoriesHandle (chCreateCategoryOnDb, chDeleteCategoryFromDb, chEditCategoryOnDb, chGetCategoriesListFromDb, chLogger, chParseRequestBody))
import           Responses                 (toResponseErrorMessage)
import           Types.Other               (ResponseErrorMessage (MethodNotAllowed, NotFound),
                                            ResponseOkMessage (Created, OkJSON, OkMessage))

getCategoriesList ::
       Monad m
    => CategoriesHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
getCategoriesList operations req =
    if requestMethod req == methodGet
        then do
            logInfo
                (chLogger operations)
                "Preparing data for sending categories list"
            result <- chGetCategoriesListFromDb operations pageParam
            case result of
                Left someError ->
                    return $
                    Left $
                    toResponseErrorMessage
                        "List of categories not sended."
                        someError
                Right loc -> do
                    return $ Right $ OkJSON $ encode loc
        else do
            logError (chLogger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
  where
    pageParam = toPage req

postCategory ::
       Monad m
    => CategoriesHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
postCategory operations req =
    if requestMethod req /= methodPost
        then do
            logError (chLogger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo (chLogger operations) "Preparing data for creating category"
            (i, _) <- chParseRequestBody operations req
            let token = takeToken req
            let createCategoryParams = toCreateCategory i
            result <- chCreateCategoryOnDb operations token createCategoryParams
            case result of
                Left someError ->
                    return $
                    Left $
                    toResponseErrorMessage "Category not created." someError
                Right categoryId -> do
                    logInfo (chLogger operations) "Category created."
                    return $
                        Right $
                        Created $ LBS.fromStrict $ BC.pack $ show categoryId

deleteCategory ::
       Monad m
    => CategoriesHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
deleteCategory operations req =
    if requestMethod req /= methodDelete
        then do
            logError (chLogger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo (chLogger operations) "Preparing data for deleting category"
            let token = takeToken req
            (i, _) <- chParseRequestBody operations req
            let categoryName = toCategoryName i
            result <- chDeleteCategoryFromDb operations token categoryName
            case result of
                Left someError ->
                    return $
                    Left $
                    toResponseErrorMessage "Category not deleted." someError
                Right _ -> do
                    return $ Right $ OkMessage "Category deleted."

updateCategory ::
       Monad m
    => CategoriesHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
updateCategory operations req = do
    if requestMethod req /= methodPut
        then do
            logError (chLogger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo (chLogger operations) "Preparing data for editing category"
            let token = takeToken req
            (i, _) <- chParseRequestBody operations req
            let editCategoryParameters = toEditCategory i
            result <- chEditCategoryOnDb operations token editCategoryParameters
            case result of
                Left someError ->
                    return $
                    Left $
                    toResponseErrorMessage "Category not edited." someError
                Right _ -> do
                    return $ Right $ OkMessage "Category edited."

categoriesRouter ::
       Monad m
    => CategoriesHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
categoriesRouter operations req
    | pathElemsC == 1 = getCategoriesList operations req
    | pathElemsC == 2 =
        case last pathElems of
            "delete_category" -> deleteCategory operations req
            "create_category" -> postCategory operations req
            "edit_category"   -> updateCategory operations req
            _                 -> return $ Left $ NotFound "Not Found"
    | otherwise = return $ Left $ NotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
