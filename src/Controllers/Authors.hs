{-# LANGUAGE OverloadedStrings #-}

module Controllers.Authors where

import           Control.Monad.IO.Class    (MonadIO (..))
import           Data.Aeson                (encode)
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as LBS
import           FromRequest               (takeToken, toAuthorLogin,
                                            toCreateAuthor, toEditAuthor,
                                            toPage)
import           Logger                    (logDebug, logError, logInfo)
import           Network.HTTP.Types.Method (methodDelete, methodGet, methodPost,
                                            methodPut)
import           Network.Wai               (Request (rawPathInfo, requestMethod),
                                            Response)
import           Network.Wai.Parse         (lbsBackEnd, parseRequestBody)
import           OperationsHandle          (AuthorsHandle (authors_logger, create_author_in_db, delete_author_in_db, edit_author_in_db, get_authors_list))
import           Responses                 (badResponse, responseCreated,
                                            responseMethodNotAllowed,
                                            responseNotFound, responseOKJSON)

newAuthor :: MonadIO m => AuthorsHandle m -> Request -> m Response
newAuthor methods req =
    if requestMethod req /= methodPost
        then do
            logError (authors_logger methods) "Bad request method"
            return $ responseMethodNotAllowed "Bad request method"
        else do
            logInfo
                (authors_logger methods)
                "Preparing parameters for creating new author."
            let token' = takeToken req
            (i, _) <- liftIO $ parseRequestBody lbsBackEnd req
            let create_author_params = toCreateAuthor i
            logDebug (authors_logger methods) "Creating Author on database"
            result <- create_author_in_db methods token' create_author_params
            case result of
                Left someError ->
                    return $ badResponse "Author not created." someError
                Right n -> do
                    return $ responseCreated $ LBS.fromStrict $ BC.pack $ show n

deleteAuthor :: MonadIO m => AuthorsHandle m -> Request -> m Response
deleteAuthor methods req =
    if requestMethod req /= methodDelete
        then do
            logError (authors_logger methods) "Bad request method"
            return $ responseMethodNotAllowed "Bad request method"
        else do
            logInfo
                (authors_logger methods)
                "Preparing parameters for deleting author."
            let token' = takeToken req
            (i, _) <- liftIO $ parseRequestBody lbsBackEnd req
            let author_login' = toAuthorLogin i
            result <- delete_author_in_db methods token' author_login'
            case result of
                Left someError ->
                    return $ badResponse "Author not deleted." someError
                Right _ -> do
                    return $ responseCreated "Author deleted."

sendAuthorsList :: MonadIO m => AuthorsHandle m -> Request -> m Response
sendAuthorsList methods req = do
    if requestMethod req /= methodGet
        then do
            logError (authors_logger methods) "Bad request method"
            return $ responseMethodNotAllowed "Bad request method"
        else do
            logInfo
                (authors_logger methods)
                "Preparing data for sending authors list"
            result <- get_authors_list methods pageParam
            case result of
                Left someError -> return $ badResponse "" someError
                Right al -> do
                    return $ responseOKJSON $ encode al
  where
    pageParam = toPage req

editAuthor :: MonadIO m => AuthorsHandle m -> Request -> m Response
editAuthor methods req = do
    if requestMethod req /= methodPut
        then do
            logError (authors_logger methods) "Bad request method"
            return $ responseMethodNotAllowed "Bad request method"
        else do
            logInfo
                (authors_logger methods)
                "Preparing data for editing author's description."
            let token' = takeToken req
            (i, _) <- liftIO $ parseRequestBody lbsBackEnd req
            let edit_params = toEditAuthor i
            result <- edit_author_in_db methods token' edit_params
            case result of
                Left someError ->
                    return $ badResponse "Author not edited." someError
                Right _ -> do
                    return $ responseCreated "Author edited."

authorsRouter :: MonadIO m => AuthorsHandle m -> Request -> m Response
authorsRouter methods req
    | pathElemsC == 1 = sendAuthorsList methods req
    | pathElemsC == 2 =
        case last pathElems of
            "delete_author" -> deleteAuthor methods req
            "create_author" -> newAuthor methods req
            "edit_author"   -> editAuthor methods req
            _               -> return $ responseNotFound "Not Found"
    | otherwise = return $ responseNotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
