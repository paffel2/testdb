{-# LANGUAGE OverloadedStrings #-}

module Controllers.Authors where

import           Data.Aeson                (encode)
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as LBS
import           FromRequest               (takeToken, toAuthorLogin,
                                            toCreateAuthor, toEditAuthor,
                                            toPage)
import           Logger                    (logDebug, logError, logInfo)
import           Network.HTTP.Types.Method (methodDelete, methodGet, methodPost,
                                            methodPut)
import           Network.Wai               (Request (rawPathInfo, requestMethod))
import           OperationsHandle          (AuthorsHandle (ahCreateAuthorInDb, ahDeleteAuthorInDb, ahEditAuthorInDb, ahGetAuthorsList, ahLogger, ahParseRequestBody))
import           Responses                 (toResponseErrorMessage)
import           Types.Other               (ResponseErrorMessage (MethodNotAllowed, NotFound),
                                            ResponseOkMessage (Created, OkJSON, OkMessage))

postAuthor ::
       Monad m
    => AuthorsHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
postAuthor methods req =
    if requestMethod req /= methodPost
        then do
            logError (ahLogger methods) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo
                (ahLogger methods)
                "Preparing parameters for creating new author."
            let token = takeToken req
            (i, _) <- ahParseRequestBody methods req
            let createAuthorParams = toCreateAuthor i
            logDebug (ahLogger methods) "Creating Author on database"
            result <- ahCreateAuthorInDb methods token createAuthorParams
            case result of
                Left someError ->
                    return $
                    Left $
                    toResponseErrorMessage "Author not created." someError
                Right n -> do
                    return $ Right $ Created $ LBS.fromStrict $ BC.pack $ show n

deleteAuthor ::
       Monad m
    => AuthorsHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
deleteAuthor methods req =
    if requestMethod req /= methodDelete
        then do
            logError (ahLogger methods) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo
                (ahLogger methods)
                "Preparing parameters for deleting author."
            let token = takeToken req
            (i, _) <- ahParseRequestBody methods req
            let authorLogin = toAuthorLogin i
            result <- ahDeleteAuthorInDb methods token authorLogin
            case result of
                Left someError ->
                    return $
                    Left $
                    toResponseErrorMessage "Author not deleted." someError
                Right _ -> do
                    return $ Right $ OkMessage "Author deleted."

getAuthorsList ::
       Monad m
    => AuthorsHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
getAuthorsList methods req = do
    if requestMethod req /= methodGet
        then do
            logError (ahLogger methods) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo (ahLogger methods) "Preparing data for sending authors list"
            result <- ahGetAuthorsList methods pageParam
            case result of
                Left someError ->
                    return $
                    Left $
                    toResponseErrorMessage
                        "List of authors not sended."
                        someError
                Right al -> do
                    return $ Right $ OkJSON (encode al)
  where
    pageParam = toPage req

updateAuthor ::
       Monad m
    => AuthorsHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
updateAuthor methods req = do
    if requestMethod req /= methodPut
        then do
            logError (ahLogger methods) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo
                (ahLogger methods)
                "Preparing data for editing author's description."
            let token = takeToken req
            (i, _) <- ahParseRequestBody methods req
            let editParams = toEditAuthor i
            result <- ahEditAuthorInDb methods token editParams
            case result of
                Left someError ->
                    return $
                    Left $ toResponseErrorMessage "Author not edited." someError
                Right _ -> do
                    return $ Right $ OkMessage "Author edited."

authorsRouter ::
       Monad m
    => AuthorsHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
authorsRouter methods req
    | pathElemsC == 1 = getAuthorsList methods req
    | pathElemsC == 2 =
        case last pathElems of
            "delete_author" -> deleteAuthor methods req
            "create_author" -> postAuthor methods req
            "edit_author"   -> updateAuthor methods req
            _               -> return $ Left $ NotFound "Not Found"
    | otherwise = return $ Left $ NotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
