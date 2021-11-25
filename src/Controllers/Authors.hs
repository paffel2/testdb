{-# LANGUAGE OverloadedStrings #-}

module Controllers.Authors where

import           Answer                (answer)
import           Answers.Authors       (authorsListHandle, createAuthorHandle,
                                        deleteAuthorHandle, updateAuthorHandle)
import           Control.Monad.Except  (ExceptT, MonadIO, runExceptT)
import           Data.Aeson            (encode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as LBS
import           Logger                (LoggerHandle, logInfo)
import           Network.Wai           (Request (rawPathInfo))
import           OperationsHandle      (AuthorsHandle)
import           Responses             (toResponseErrorMessage')
import           Types.Authors         (AuthorsList)
import           Types.Other           (ResponseErrorMessage (NotFound),
                                        ResponseOkMessage (Created, OkJSON, OkMessage),
                                        SendId, SomeError)

authorsRouter ::
       MonadIO m
    => AuthorsHandle (ExceptT SomeError m)
    -> LoggerHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
authorsRouter methods hLogger req
    | pathElemsC == 1 =
        authorsListSendResult hLogger $ answer req (authorsListHandle methods)
    | pathElemsC == 2 =
        case last pathElems of
            "delete_author" ->
                deleteAuthorSendResult hLogger $
                answer req (deleteAuthorHandle methods)
            "create_author" ->
                createAuthorSendResult hLogger $
                answer req (createAuthorHandle methods)
            "edit_author" ->
                updateAuthorSendResult hLogger $
                answer req (updateAuthorHandle methods)
            _ -> return $ Left $ NotFound "Not Found"
    | otherwise = return $ Left $ NotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems

authorsListSendResult ::
       Monad m
    => LoggerHandle m
    -> ExceptT SomeError m AuthorsList
    -> m (Either ResponseErrorMessage ResponseOkMessage)
authorsListSendResult hLogger result = do
    a <- runExceptT result
    case a of
        Left someError ->
            Left <$>
            toResponseErrorMessage'
                hLogger
                "List of authors not sended."
                someError
        Right someList -> do
            logInfo hLogger "List of authors sended."
            return $ Right $ OkJSON $ encode someList

updateAuthorSendResult ::
       Monad m
    => LoggerHandle m
    -> ExceptT SomeError m ()
    -> m (Either ResponseErrorMessage ResponseOkMessage)
updateAuthorSendResult hLogger result = do
    a <- runExceptT result
    case a of
        Left someError ->
            Left <$>
            toResponseErrorMessage' hLogger "Author not edited." someError
        Right _ -> do
            logInfo hLogger "Author edited."
            return $ Right $ OkMessage "Author edited."

deleteAuthorSendResult ::
       Monad m
    => LoggerHandle m
    -> ExceptT SomeError m ()
    -> m (Either ResponseErrorMessage ResponseOkMessage)
deleteAuthorSendResult hLogger result = do
    a <- runExceptT result
    case a of
        Left someError ->
            Left <$>
            toResponseErrorMessage' hLogger "Author not deleted." someError
        Right _ -> do
            logInfo hLogger "Author deleted."
            return $ Right $ OkMessage "Author deleted."

createAuthorSendResult ::
       Monad m
    => LoggerHandle m
    -> ExceptT SomeError m SendId
    -> m (Either ResponseErrorMessage ResponseOkMessage)
createAuthorSendResult hLogger result = do
    a <- runExceptT result
    case a of
        Left someError ->
            Left <$>
            toResponseErrorMessage' hLogger "Author not created." someError
        Right someId -> do
            logInfo hLogger "Author created."
            return $ Right $ Created $ LBS.fromStrict $ BC.pack $ show someId
