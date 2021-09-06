{-# LANGUAGE OverloadedStrings #-}

module Controllers.Authors where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import qualified Data.Text.Encoding as E
import Database.PostgreSQL.Simple (Connection)
import Databaseoperations.Authors
    ( createAuthorInDb
    , deleteAuthorInDb
    , editAuthorInDb
    , getAuthorsList
    )
import FromRequest (takeToken)
import HelpFunction (readByteStringToInt)
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

newAuthor ::
       Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
newAuthor hLogger pool token_lifetime req =
    if requestMethod req /= methodPost
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad request method"
        else do
            let token' = E.decodeUtf8 <$> takeToken req
            (i, _) <- parseRequestBody lbsBackEnd req
            let author_login = E.decodeUtf8 <$> lookup "author_login" i
            let description = E.decodeUtf8 <$> lookup "description" i
            result <-
                createAuthorInDb
                    hLogger
                    pool
                    token_lifetime
                    token'
                    author_login
                    description
            case result of
                Left "Not admin" -> return $ responseForbidden "Not admin"
                Left "Bad token" -> return $ responseForbidden "Bad token"
                Left bs -> return $ responseCreated bs
                Right n ->
                    return $ responseOk $ LBS.fromStrict $ BC.pack $ show n

deleteAuthor ::
       Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
deleteAuthor hLogger pool token_lifetime req =
    if requestMethod req /= methodDelete
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad request method"
        else do
            let token' = E.decodeUtf8 <$> takeToken req
            (i, _) <- parseRequestBody lbsBackEnd req
            let author_login = E.decodeUtf8 <$> lookup "author_login" i
            result <-
                deleteAuthorInDb hLogger pool token_lifetime token' author_login
            case result of
                Left "Not admin" -> return $ responseForbidden "Not admin"
                Left "Bad token" -> return $ responseForbidden "Bad token"
                Left bs -> return $ responseBadRequest bs
                Right bs -> return $ responseOk bs

sendAuthorsList :: Handle -> Pool Connection -> Request -> IO Response
sendAuthorsList hLogger pool req = do
    if requestMethod req /= methodGet
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad request method"
        else do
            result <- getAuthorsList hLogger pool pageParam
            case result of
                Left bs -> return $ responseBadRequest bs
                Right al -> return $ responseOKJSON $ encode al
  where
    pageParam = fromMaybe Nothing (lookup "page" $ queryString req)

editAuthor ::
       Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
editAuthor hLogger pool token_lifetime req = do
    if requestMethod req /= methodPut
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad request method"
        else do
            let token' = E.decodeUtf8 <$> takeToken req
            (i, _) <- parseRequestBody lbsBackEnd req
            let new_description = E.decodeUtf8 <$> lookup "new_description" i
            let a_id = readByteStringToInt =<< lookup "author_id" i
            result <-
                editAuthorInDb
                    hLogger
                    pool
                    token_lifetime
                    token'
                    a_id
                    new_description
            case result of
                Left "Not admin" -> return $ responseForbidden "Not admin"
                Left "Bad token" -> return $ responseForbidden "Bad token"
                Left bs -> return $ responseBadRequest bs
                Right bs -> return $ responseOk bs

authorsBlock ::
       Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
authorsBlock hLogger pool token_lifetime req
    | pathElemsC == 1 = sendAuthorsList hLogger pool req
    | pathElemsC == 2 =
        case last pathElems of
            "delete_author" -> deleteAuthor hLogger pool token_lifetime req
            "create_author" -> newAuthor hLogger pool token_lifetime req
            "edit_author" -> editAuthor hLogger pool token_lifetime req
            _ -> return $ responseNotFound "Not Found"
    | otherwise = return $ responseNotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
