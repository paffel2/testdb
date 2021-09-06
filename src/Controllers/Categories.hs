{-# LANGUAGE OverloadedStrings #-}

module Controllers.Categories where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Database.PostgreSQL.Simple (Connection)
import Databaseoperations.Categories
    ( createCategoryOnDb
    , deleteCategoryFromDb
    , editCategoryOnDb
    , getCategoriesListFromDb
    )
import FromRequest (takeToken)
import Logger (Handle, logError)
import Network.HTTP.Types.Method
    ( methodDelete
    , methodGet
    , methodPost
    , methodPut
    )
import Network.Wai (Request(queryString, rawPathInfo, requestMethod), Response)
import Network.Wai.Parse (lbsBackEnd, parseRequestBody)
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

sendCategoriesList :: Handle -> Pool Connection -> Request -> IO Response
sendCategoriesList hLogger pool req =
    if requestMethod req == methodGet
        then do
            result <- getCategoriesListFromDb hLogger pool pageParam
            case result of
                Left bs -> return $ responseBadRequest bs
                Right loc -> return $ responseOKJSON $ encode loc
        else do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
  where
    queryParams = queryString req
    pageParam = fromMaybe Nothing (lookup "page" queryParams)

createCategory ::
       Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
createCategory hLogger pool token_lifetime req =
    if requestMethod req /= methodPost
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            (i, _) <- parseRequestBody lbsBackEnd req
            let token' = E.decodeUtf8 <$> takeToken req
            let category_name =
                    T.toLower . E.decodeUtf8 <$> lookup "category_name" i
            let maternal_category_name =
                    T.toLower . E.decodeUtf8 <$>
                    lookup "maternal_category_name" i
            result <-
                createCategoryOnDb
                    hLogger
                    pool
                    token_lifetime
                    token'
                    category_name
                    maternal_category_name
            case result of
                Left "Not admin" -> return $ responseForbidden "Not admin"
                Left "Bad token" -> return $ responseForbidden "Bad token"
                Left bs -> return $ responseCreated bs
                Right bs -> return $ responseOk bs

deleteCategory ::
       Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
deleteCategory hLogger pool token_lifetime req =
    if requestMethod req /= methodDelete
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            let token' = E.decodeUtf8 <$> takeToken req
            (i, _) <- parseRequestBody lbsBackEnd req
            let category_name = E.decodeUtf8 <$> lookup "category_name" i
            result <-
                deleteCategoryFromDb
                    hLogger
                    pool
                    token_lifetime
                    token'
                    category_name
            case result of
                Left "Not admin" -> return $ responseForbidden "Not admin"
                Left "Bad token" -> return $ responseForbidden "Bad token"
                Left bs -> return $ responseBadRequest bs
                Right bs -> return $ responseOk bs

editCategory ::
       Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
editCategory hLogger pool token_lifetime req = do
    if requestMethod req /= methodPut
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            let token' = E.decodeUtf8 <$> takeToken req
            (i, _) <- parseRequestBody lbsBackEnd req
            let category_name = E.decodeUtf8 <$> lookup "category_name" i
            let new_maternal_parametr = E.decodeUtf8 <$> lookup "new_maternal" i
            let new_name_parametr = E.decodeUtf8 <$> lookup "new_name" i
            result <-
                editCategoryOnDb
                    hLogger
                    pool
                    token_lifetime
                    token'
                    category_name
                    new_name_parametr
                    new_maternal_parametr
            case result of
                Left "Not admin" -> return $ responseForbidden "Not admin"
                Left "Bad token" -> return $ responseForbidden "Bad token"
                Left bs -> return $ responseBadRequest bs
                Right bs -> return $ responseOk bs

categoriesBlock ::
       Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
categoriesBlock hLogger pool token_lifetime req
    | pathElemsC == 1 = sendCategoriesList hLogger pool req
    | pathElemsC == 2 =
        case last pathElems of
            "delete_category" -> deleteCategory hLogger pool token_lifetime req
            "create_category" -> createCategory hLogger pool token_lifetime req
            "edit_category" -> editCategory hLogger pool token_lifetime req
            _ -> return $ responseNotFound "Not Found"
    | otherwise = return $ responseNotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
