{-# LANGUAGE OverloadedStrings #-}

module Answers.Authors where

import           Answer
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as LBS
import           FromRequest
import           Logger
import           Network.HTTP.Types.Method
import           Network.Wai
import           Network.Wai.Parse
import           OperationsHandle
import           Responses
import           Types.Authors
import           Types.Other

{-createAuthorInformation ::
       LoggerHandle IO
    -> Request
    -> IO (Either SomeError (Maybe Token, CreateAuthor)) -}
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
