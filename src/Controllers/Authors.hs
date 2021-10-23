{-# LANGUAGE OverloadedStrings #-}

module Controllers.Authors where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import FromRequest
    ( takeToken
    , toAuthorLogin
    , toCreateAuthor
    , toEditAuthor
    , toPage
    )
import Logger (Handle, logDebug, logError, logInfo)
import Network.HTTP.Types.Method
    ( methodDelete
    , methodGet
    , methodPost
    , methodPut
    )
import Network.Wai (Request(rawPathInfo, requestMethod), Response)
import Network.Wai.Parse (lbsBackEnd, parseRequestBody)
import OperationsHandle
    ( AuthorsHandle(create_author_in_db, delete_author_in_db,
              edit_author_in_db, get_authors_list)
    )
import Responses
    ( responseBadRequest
    , responseCreated
    , responseForbidden
    , responseMethodNotAllowed
    , responseNotFound
    , responseOKJSON
    )

import Types.Other (TokenLifeTime)

newAuthor ::
       MonadIO m
    => Handle m
    -> AuthorsHandle m
    -> Pool Connection
    -> TokenLifeTime
    -> Request
    -> m Response
newAuthor hLogger methods pool token_lifetime req =
    if requestMethod req /= methodPost
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad request method"
        else do
            logInfo hLogger "Preparing parameters for creating new author."
            let token' = takeToken req
            (i, _) <- liftIO $ parseRequestBody lbsBackEnd req
            let create_author_params = toCreateAuthor i
            logDebug hLogger "Creating Author on database"
            result <-
                create_author_in_db
                    methods
                    hLogger
                    pool
                    token_lifetime
                    token'
                    create_author_params
            case result of
                Left "Not admin" -> do
                    logError hLogger "Author not created. Not admin."
                    return $ responseForbidden "Not admin"
                Left "Bad token" -> do
                    logError hLogger "Author not created. Bad token."
                    return $ responseForbidden "Bad token"
                Left bs -> do
                    logError hLogger "Author not created."
                    return $ responseBadRequest bs
                Right n -> do
                    logInfo hLogger "Author created."
                    return $ responseCreated $ LBS.fromStrict $ BC.pack $ show n

deleteAuthor ::
       MonadIO m
    => Handle m
    -> AuthorsHandle m
    -> Pool Connection
    -> TokenLifeTime
    -> Request
    -> m Response
deleteAuthor hLogger methods pool token_lifetime req =
    if requestMethod req /= methodDelete
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad request method"
        else do
            logInfo hLogger "Preparing parameters for deleting author."
            let token' = takeToken req
            (i, _) <- liftIO $ parseRequestBody lbsBackEnd req
            let author_login' = toAuthorLogin i
            result <-
                delete_author_in_db
                    methods
                    hLogger
                    pool
                    token_lifetime
                    token'
                    author_login'
            case result of
                Left "Not admin" -> do
                    logError hLogger "Author not deleted. Not admin."
                    return $ responseForbidden "Not admin"
                Left "Bad token" -> do
                    logError hLogger "Author not deleted. Bad token."
                    return $ responseForbidden "Bad token"
                Left bs -> do
                    logError hLogger "Author not deleted."
                    return $ responseBadRequest bs
                Right bs -> do
                    logInfo hLogger "Author deleted."
                    return $ responseCreated bs

sendAuthorsList ::
       MonadIO m
    => Handle m
    -> AuthorsHandle m
    -> Pool Connection
    -> Request
    -> m Response
sendAuthorsList hLogger methods pool req = do
    if requestMethod req /= methodGet
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad request method"
        else do
            logInfo hLogger "Preparing data for sending authors list"
            result <- get_authors_list methods hLogger pool pageParam
            case result of
                Left bs -> do
                    logError hLogger "Authors list not sended."
                    return $ responseBadRequest bs
                Right al -> do
                    logInfo hLogger "Authors list sended."
                    return $ responseOKJSON $ encode al
  where
    pageParam = toPage req

editAuthor ::
       MonadIO m
    => Handle m
    -> AuthorsHandle m
    -> Pool Connection
    -> TokenLifeTime
    -> Request
    -> m Response
editAuthor hLogger methods pool token_lifetime req = do
    if requestMethod req /= methodPut
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad request method"
        else do
            logInfo hLogger "Preparing data for editing author's description."
            let token' = takeToken req
            (i, _) <- liftIO $ parseRequestBody lbsBackEnd req
            let edit_params = toEditAuthor i
            result <-
                edit_author_in_db
                    methods
                    hLogger
                    pool
                    token_lifetime
                    token'
                    edit_params
            case result of
                Left "Not admin" -> do
                    logError hLogger "Author not edited. Not admin."
                    return $ responseForbidden "Not admin"
                Left "Bad token" -> do
                    logError hLogger "Author not edited. Bad token."
                    return $ responseForbidden "Bad token"
                Left bs -> do
                    logError hLogger "Author not edited."
                    return $ responseBadRequest bs
                Right bs -> do
                    logInfo hLogger "Author edited."
                    return $ responseCreated bs

authorsRouter ::
       MonadIO m
    => Handle m
    -> AuthorsHandle m
    -> Pool Connection
    -> TokenLifeTime
    -> Request
    -> m Response
authorsRouter hLogger methods pool token_lifetime req
    | pathElemsC == 1 = sendAuthorsList hLogger methods pool req
    | pathElemsC == 2 =
        case last pathElems of
            "delete_author" ->
                deleteAuthor hLogger methods pool token_lifetime req
            "create_author" -> newAuthor hLogger methods pool token_lifetime req
            "edit_author" -> editAuthor hLogger methods pool token_lifetime req
            _ -> return $ responseNotFound "Not Found"
    | otherwise = return $ responseNotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
