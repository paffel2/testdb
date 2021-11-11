{-# LANGUAGE OverloadedStrings #-}

module Answers.Authors where

import           Answer                    (AnswerHandle (..))
import           Data.Aeson                (encode)
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as LBS
import           FromRequest               (takeToken, toAuthorLogin,
                                            toCreateAuthor, toEditAuthor,
                                            toPage)
import           Logger                    (logError, logInfo)
import           Network.HTTP.Types.Method (methodDelete, methodGet, methodPost,
                                            methodPut)
import           Network.Wai               (Request (requestMethod))
import           OperationsHandle          (AuthorsHandle (ahCreateAuthorInDb, ahDeleteAuthorInDb, ahEditAuthorInDb, ahGetAuthorsList, ahLogger, ahParseRequestBody))
import           Responses                 (toResponseErrorMessage)
import           Types.Authors             (AuthorLogin, AuthorsList,
                                            CreateAuthor, EditAuthor)
import           Types.Other               (Page, ResponseErrorMessage,
                                            ResponseOkMessage (Created, OkJSON, OkMessage),
                                            SendId, SomeError (BadMethod),
                                            Token)

createAuthorParseInformation ::
       Monad m
    => AuthorsHandle m
    -> Request
    -> m (Either SomeError (Maybe Token, CreateAuthor))
createAuthorParseInformation handler request =
    if requestMethod request /= methodPost
        then do
            logError (ahLogger handler) "Bad request method"
            return $ Left BadMethod
        else do
            logInfo
                (ahLogger handler)
                "Preparing parameters for creating new author."
            (i, _) <- ahParseRequestBody handler request
            let token = takeToken request
            let createAuthorParams = toCreateAuthor i
            return $ Right (token, createAuthorParams)

createAuthorSendResult ::
       Monad m
    => Either SomeError SendId
    -> m (Either ResponseErrorMessage ResponseOkMessage)
createAuthorSendResult result =
    case result of
        Left someError ->
            return $
            Left $ toResponseErrorMessage "Author not created." someError
        Right n -> do
            return $ Right $ Created $ LBS.fromStrict $ BC.pack $ show n

createAuthorDatabaseOperation ::
       Monad m
    => AuthorsHandle m
    -> Either SomeError (Maybe Token, CreateAuthor)
    -> m (Either SomeError SendId)
createAuthorDatabaseOperation _ (Left message) = return $ Left message
createAuthorDatabaseOperation authorHandle (Right (token, craeteInfo)) =
    ahCreateAuthorInDb authorHandle token craeteInfo

createAuthorHandle ::
       Monad m
    => AuthorsHandle m
    -> AnswerHandle m (Either SomeError (Maybe Token, CreateAuthor)) SendId
createAuthorHandle authorHandle =
    AnswerHandle
        { parseInformation = createAuthorParseInformation authorHandle
        , databaseOperation = createAuthorDatabaseOperation authorHandle
        , sendResult = createAuthorSendResult
        }

---------------------------------------------------------------------------------------------------------------------
deleteAuthorParseInformation ::
       Monad m
    => AuthorsHandle m
    -> Request
    -> m (Either SomeError (Maybe Token, Maybe AuthorLogin))
deleteAuthorParseInformation handler request =
    if requestMethod request /= methodDelete
        then do
            logError (ahLogger handler) "Bad request method"
            return $ Left BadMethod
        else do
            logInfo
                (ahLogger handler)
                "Preparing parameters for deleting author."
            let token = takeToken request
            (i, _) <- ahParseRequestBody handler request
            let authorLogin = toAuthorLogin i
            return $ Right (token, authorLogin)

deleteAuthorDatabaseOperation ::
       Monad m
    => AuthorsHandle m
    -> Either SomeError (Maybe Token, Maybe AuthorLogin)
    -> m (Either SomeError ())
deleteAuthorDatabaseOperation _ (Left message) = return $ Left message
deleteAuthorDatabaseOperation authorHandle (Right (token, authorLogin)) =
    ahDeleteAuthorInDb authorHandle token authorLogin

deleteAuthorSendResult ::
       Monad m
    => Either SomeError b
    -> m (Either ResponseErrorMessage ResponseOkMessage)
deleteAuthorSendResult result =
    case result of
        Left someError ->
            return $
            Left $ toResponseErrorMessage "Author not deleted." someError
        Right _ -> do
            return $ Right $ OkMessage "Author deleted."

deleteAuthorHandle ::
       Monad m
    => AuthorsHandle m
    -> AnswerHandle m (Either SomeError (Maybe Token, Maybe AuthorLogin)) ()
deleteAuthorHandle authorHandle =
    AnswerHandle
        { parseInformation = deleteAuthorParseInformation authorHandle
        , databaseOperation = deleteAuthorDatabaseOperation authorHandle
        , sendResult = deleteAuthorSendResult
        }

-----------------------------------------------------------------------------------------------------------------------
authorsListParseInformation ::
       Monad m
    => AuthorsHandle m
    -> Request
    -> m (Either SomeError (Maybe Page))
authorsListParseInformation handler request =
    if requestMethod request /= methodGet
        then do
            logError (ahLogger handler) "Bad request method"
            return $ Left BadMethod
        else do
            logInfo (ahLogger handler) "Preparing data for sending authors list"
            return $ Right (toPage request)

authorsListDatabaseOperation ::
       Monad m
    => AuthorsHandle m
    -> Either SomeError (Maybe Page)
    -> m (Either SomeError AuthorsList)
authorsListDatabaseOperation _ (Left message) = return $ Left message
authorsListDatabaseOperation authorHandle (Right page) =
    ahGetAuthorsList authorHandle page

authorsListSendResult ::
       Monad m
    => Either SomeError AuthorsList
    -> m (Either ResponseErrorMessage ResponseOkMessage)
authorsListSendResult result =
    case result of
        Left someError ->
            return $
            Left $
            toResponseErrorMessage "List of authors not sended." someError
        Right al -> do
            return $ Right $ OkJSON (encode al)

authorsListHandle ::
       Monad m
    => AuthorsHandle m
    -> AnswerHandle m (Either SomeError (Maybe Page)) AuthorsList
authorsListHandle authorHandle =
    AnswerHandle
        { parseInformation = authorsListParseInformation authorHandle
        , databaseOperation = authorsListDatabaseOperation authorHandle
        , sendResult = authorsListSendResult
        }

-----------------------------------------------------------------------------------------------------------------
updateAuthorParseInformation ::
       Monad m
    => AuthorsHandle m
    -> Request
    -> m (Either SomeError (Maybe Token, EditAuthor))
updateAuthorParseInformation handler request =
    if requestMethod request /= methodPut
        then do
            logError (ahLogger handler) "Bad request method"
            return $ Left BadMethod
        else do
            logInfo
                (ahLogger handler)
                "Preparing data for editing author's description."
            let token = takeToken request
            (i, _) <- ahParseRequestBody handler request
            let editInfo = toEditAuthor i
            return $ Right (token, editInfo)

updateAuthorDatabaseOperation ::
       Monad m
    => AuthorsHandle m
    -> Either SomeError (Maybe Token, EditAuthor)
    -> m (Either SomeError ())
updateAuthorDatabaseOperation _ (Left message) = return $ Left message
updateAuthorDatabaseOperation authorHandle (Right (token, editInfo)) =
    ahEditAuthorInDb authorHandle token editInfo

updateAuthorSendResult ::
       Monad m
    => Either SomeError ()
    -> m (Either ResponseErrorMessage ResponseOkMessage)
updateAuthorSendResult result =
    case result of
        Left someError ->
            return $
            Left $ toResponseErrorMessage "Author not edited." someError
        Right _ -> do
            return $ Right $ OkMessage "Author edited."

updateAuthorHandle ::
       Monad m
    => AuthorsHandle m
    -> AnswerHandle m (Either SomeError (Maybe Token, EditAuthor)) ()
updateAuthorHandle authorHandle =
    AnswerHandle
        { parseInformation = updateAuthorParseInformation authorHandle
        , databaseOperation = updateAuthorDatabaseOperation authorHandle
        , sendResult = updateAuthorSendResult
        }
