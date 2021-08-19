{-# LANGUAGE OverloadedStrings #-}

module Categories where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Database.PostgreSQL.Simple (Connection)
import Databaseoperations
    ( checkAdmin'
    , createCategoryOnDb
    , deleteCategoryFromDb
    , editCategoryOnDb
    , getCategoriesListFromDb
    )
import FromRequest (takeToken)
import Logger (Handle)
import Network.Wai (Request(queryString), Response)
import Network.Wai.Parse (lbsBackEnd, parseRequestBody)
import Responses (responseBadRequest, responseOk)
import Types (TokenLifeTime)

sendCategoriesList :: Handle -> Pool Connection -> Request -> IO Response
sendCategoriesList hLogger pool req = do
    result <- getCategoriesListFromDb hLogger pool pageParam
    case result of
        Left bs -> return $ responseBadRequest bs
        Right loc -> return $ responseOk $ encode loc
  where
    queryParams = queryString req
    pageParam = fromMaybe Nothing (lookup "page" queryParams)

createCategory ::
       Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
createCategory hLogger pool token_lifetime req = do
    let token' = E.decodeUtf8 <$> takeToken req
    ct <- checkAdmin' hLogger pool token_lifetime token'
    case ct of
        (False, bs) -> return $ responseBadRequest bs
        (True, _) -> do
            (i, _) <- parseRequestBody lbsBackEnd req
            let category_name =
                    T.toLower . E.decodeUtf8 <$> lookup "category_name" i
            let maternal_category_name =
                    T.toLower . E.decodeUtf8 <$>
                    lookup "maternal_category_name" i
            result <-
                createCategoryOnDb
                    hLogger
                    pool
                    category_name
                    maternal_category_name
            case result of
                Left bs -> return $ responseBadRequest bs
                Right bs -> return $ responseOk bs

deleteCategory ::
       Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
deleteCategory hLogger pool token_lifetime req = do
    let token' = E.decodeUtf8 <$> takeToken req
    ct <- checkAdmin' hLogger pool token_lifetime token'
    case ct of
        (False, bs) -> return $ responseBadRequest bs
        (True, _) -> do
            (i, _) <- parseRequestBody lbsBackEnd req
            let category_name = E.decodeUtf8 <$> lookup "category_name" i
            result <- deleteCategoryFromDb hLogger pool category_name
            case result of
                Left bs -> return $ responseBadRequest bs
                Right bs -> return $ responseOk bs

editCategory ::
       Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
editCategory hLogger pool token_lifetime req = do
    let token' = E.decodeUtf8 <$> takeToken req
    ct <- checkAdmin' hLogger pool token_lifetime token'
    case ct of
        (False, bs) -> return $ responseBadRequest bs
        (True, _) -> do
            (i, _) <- parseRequestBody lbsBackEnd req
            let category_name = E.decodeUtf8 <$> lookup "category_name" i
            let new_maternal_parametr = E.decodeUtf8 <$> lookup "new_maternal" i
            let new_name_parametr = E.decodeUtf8 <$> lookup "new_name" i
            result <-
                editCategoryOnDb
                    hLogger
                    pool
                    category_name
                    new_name_parametr
                    new_maternal_parametr
            case result of
                Left bs -> return $ responseBadRequest bs
                Right bs -> return $ responseOk bs

categoriesBlock ::
       Handle
    -> Pool Connection
    -> TokenLifeTime
    -> [BC.ByteString]
    -> Request
    -> IO Response
categoriesBlock hLogger pool token_lifetime pathElems req
    | pathElemsC == 1 = sendCategoriesList hLogger pool req
    | pathElemsC == 2 =
        case last pathElems of
            "delete_category" -> deleteCategory hLogger pool token_lifetime req
            "create_category" -> createCategory hLogger pool token_lifetime req
            "edit_category" -> editCategory hLogger pool token_lifetime req
            _ -> return $ responseBadRequest "bad request"
    | otherwise = return $ responseBadRequest "bad request"
  where
    pathElemsC = length pathElems
