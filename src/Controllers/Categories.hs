{-# LANGUAGE OverloadedStrings #-}

module Controllers.Categories where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Database.PostgreSQL.Simple (Connection)
import FromRequest
import Logger (Handle, logError, logInfo)
import Network.HTTP.Types.Method
    ( methodDelete
    , methodGet
    , methodPost
    , methodPut
    )
import Network.Wai (Request(queryString, rawPathInfo, requestMethod), Response)
import Network.Wai.Parse (lbsBackEnd, parseRequestBody)
import OperationsHandle
    ( CategoriesHandle(create_category_on_db, delete_category_from_db,
                 edit_category_on_db, get_categories_list_from_db)
    )
import Responses
    ( responseBadRequest
    , responseCreated
    , responseForbidden
    , responseMethodNotAllowed
    , responseNotFound
    , responseOKJSON
    , responseOk
    )
import Types (TokenLifeTime)

sendCategoriesList ::
       (Monad m, MonadIO m)
    => Handle m
    -> CategoriesHandle m
    -> Pool Connection
    -> Request
    -> m Response
sendCategoriesList hLogger operations pool req =
    if requestMethod req == methodGet
        then do
            logInfo hLogger "Preparing data for sending categories list"
            result <-
                get_categories_list_from_db operations hLogger pool pageParam
            case result of
                Left bs -> do
                    logError hLogger "Categories list not sended."
                    return $ responseBadRequest bs
                Right loc -> do
                    logInfo hLogger "Categories list sended."
                    return $ responseOKJSON $ encode loc
        else do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
  where
    pageParam = toPage req

createCategory ::
       MonadIO m
    => Handle m
    -> CategoriesHandle m
    -> Pool Connection
    -> TokenLifeTime
    -> Request
    -> m Response
createCategory hLogger operations pool token_lifetime req =
    if requestMethod req /= methodPost
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo hLogger "Preparing data for creating category"
            (i, _) <- liftIO $ parseRequestBody lbsBackEnd req
            let token' = takeToken req
            let category_name =
                    T.toLower . E.decodeUtf8 <$> lookup "category_name" i
            let maternal_category_name =
                    T.toLower . E.decodeUtf8 <$>
                    lookup "maternal_category_name" i
            result <-
                create_category_on_db
                    operations
                    hLogger
                    pool
                    token_lifetime
                    token'
                    category_name
                    maternal_category_name
            case result of
                Left "Not admin" -> do
                    logError hLogger "Category not created. Not admin."
                    return $ responseForbidden "Not admin"
                Left "Bad token" -> do
                    logError hLogger "Category not created. Bad token."
                    return $ responseForbidden "Bad token"
                Left bs -> do
                    logError hLogger "Category not created."
                    return $ responseCreated bs
                Right bs -> do
                    logInfo hLogger "Category created."
                    return $ responseOk bs

deleteCategory ::
       MonadIO m
    => Handle m
    -> CategoriesHandle m
    -> Pool Connection
    -> TokenLifeTime
    -> Request
    -> m Response
deleteCategory hLogger operations pool token_lifetime req =
    if requestMethod req /= methodDelete
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo hLogger "Preparing data for deleting category"
            let token' = takeToken req
            (i, _) <- liftIO $ parseRequestBody lbsBackEnd req
            let category_name = E.decodeUtf8 <$> lookup "category_name" i
            result <-
                delete_category_from_db
                    operations
                    hLogger
                    pool
                    token_lifetime
                    token'
                    category_name
            case result of
                Left "Not admin" -> do
                    logError hLogger "Category not deleted. Not admin."
                    return $ responseForbidden "Not admin"
                Left "Bad token" -> do
                    logError hLogger "Category not deleted. Bad token."
                    return $ responseForbidden "Bad token"
                Left bs -> do
                    logError hLogger "Category not deleted."
                    return $ responseCreated bs
                Right bs -> do
                    logInfo hLogger "Category deleted."
                    return $ responseOk bs

editCategory ::
       MonadIO m
    => Handle m
    -> CategoriesHandle m
    -> Pool Connection
    -> TokenLifeTime
    -> Request
    -> m Response
editCategory hLogger operations pool token_lifetime req = do
    if requestMethod req /= methodPut
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo hLogger "Preparing data for editing category"
            let token' = takeToken req
            (i, _) <- liftIO $ parseRequestBody lbsBackEnd req
            let category_name = E.decodeUtf8 <$> lookup "category_name" i
            let new_maternal_parametr = E.decodeUtf8 <$> lookup "new_maternal" i
            let new_name_parametr = E.decodeUtf8 <$> lookup "new_name" i
            result <-
                edit_category_on_db
                    operations
                    hLogger
                    pool
                    token_lifetime
                    token'
                    category_name
                    new_name_parametr
                    new_maternal_parametr
            case result of
                Left "Not admin" -> do
                    logError hLogger "Category not edited. Not admin."
                    return $ responseForbidden "Not admin"
                Left "Bad token" -> do
                    logError hLogger "Category not edited. Bad token."
                    return $ responseForbidden "Bad token"
                Left bs -> do
                    logError hLogger "Category not edited."
                    return $ responseCreated bs
                Right bs -> do
                    logInfo hLogger "Category edited."
                    return $ responseOk bs

categoriesRouter ::
       MonadIO m
    => Handle m
    -> CategoriesHandle m
    -> Pool Connection
    -> TokenLifeTime
    -> Request
    -> m Response
categoriesRouter hLogger operations pool token_lifetime req
    | pathElemsC == 1 = sendCategoriesList hLogger operations pool req
    | pathElemsC == 2 =
        case last pathElems of
            "delete_category" ->
                deleteCategory hLogger operations pool token_lifetime req
            "create_category" ->
                createCategory hLogger operations pool token_lifetime req
            "edit_category" ->
                editCategory hLogger operations pool token_lifetime req
            _ -> return $ responseNotFound "Not Found"
    | otherwise = return $ responseNotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
