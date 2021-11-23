{-# LANGUAGE OverloadedStrings #-}

module Answers.Tags where

import           Answer                    (AnswerHandle' (..))
import           Control.Monad.Except      (ExceptT, MonadError (throwError),
                                            MonadIO (liftIO), runExceptT)
import           Data.Aeson                (encode)
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as LBS
import           FromRequest               (parseRequestBodyLBS, takeToken,
                                            toEditTag, toPage, toTagName)
import           Logger                    (logError, logInfo)
import           Network.HTTP.Types.Method (methodDelete, methodGet, methodPost,
                                            methodPut)
import           Network.Wai               (Request (requestMethod))
import           OperationsHandle          (TagsHandle (thCreateTagInDb, thDeleteTagFromDb, thEditTagInDb, thGetTagsListFromDb, thLogger))
import           Responses                 (toResponseErrorMessage)
import           Types.Other               (MonadWithError, Page,
                                            ResponseErrorMessage,
                                            ResponseOkMessage (Created, OkJSON, OkMessage),
                                            SendId, SomeError (BadMethod),
                                            Token)
import           Types.Tags                (EditTag, TagName, TagsList)

createTagParseInformation ::
       TagsHandle MonadWithError IO
    -> Request
    -> MonadWithError (Maybe Token, Maybe TagName)
createTagParseInformation handler request =
    if requestMethod request /= methodPost
        then do
            liftIO $ logError (thLogger handler) "Bad request method"
            throwError BadMethod
        else do
            liftIO $
                logInfo (thLogger handler) "Preparing data for creating tag."
            let token = takeToken request
            let tagNameParam = toTagName request
            return (token, tagNameParam)

createTagDatabaseOperation ::
       TagsHandle MonadWithError IO
    -> (Maybe Token, Maybe TagName)
    -> MonadWithError SendId
createTagDatabaseOperation tagHandle (token, tagNameParam) =
    thCreateTagInDb tagHandle token tagNameParam

createTagSendResult ::
       MonadWithError SendId
    -> IO (Either ResponseErrorMessage ResponseOkMessage)
createTagSendResult result = do
    a <- runExceptT result
    case a of
        Left someError ->
            return $ Left $ toResponseErrorMessage "Tag not created." someError
        Right n -> do
            return $ Right $ Created $ LBS.fromStrict $ BC.pack $ show n

createTagHandle ::
       TagsHandle MonadWithError IO
    -> AnswerHandle' (ExceptT SomeError IO) (Maybe Token, Maybe TagName) SendId IO
createTagHandle tagHandle =
    AnswerHandle'
        { parseInformation' = createTagParseInformation tagHandle
        , databaseOperation' = createTagDatabaseOperation tagHandle
        , sendResult' = createTagSendResult
        }

--------------------------------------------------------------------------------------------------------------------
deleteTagParseInformation ::
       TagsHandle MonadWithError IO
    -> Request
    -> MonadWithError (Maybe Token, Maybe TagName)
deleteTagParseInformation handler request =
    if requestMethod request /= methodDelete
        then do
            liftIO $ logError (thLogger handler) "Bad request method"
            throwError BadMethod
        else do
            liftIO $
                logInfo (thLogger handler) "Preparing data for deleting tag."
            let token = takeToken request
            let tagNameParam = toTagName request
            return (token, tagNameParam)

deleteTagDatabaseOperation ::
       TagsHandle MonadWithError IO
    -> (Maybe Token, Maybe TagName)
    -> MonadWithError ()
deleteTagDatabaseOperation tagHandle (token, tagNameParam) =
    thDeleteTagFromDb tagHandle token tagNameParam

deleteTagSendResult ::
       MonadWithError () -> IO (Either ResponseErrorMessage ResponseOkMessage)
deleteTagSendResult result = do
    a <- runExceptT result
    case a of
        Left someError ->
            return $ Left $ toResponseErrorMessage "Tag not deleted." someError
        Right _ -> do
            return $ Right $ OkMessage "Tag deleted."

deleteTagHandle ::
       TagsHandle MonadWithError IO
    -> AnswerHandle' (ExceptT SomeError IO) (Maybe Token, Maybe TagName) () IO
deleteTagHandle tagHandle =
    AnswerHandle'
        { parseInformation' = deleteTagParseInformation tagHandle
        , databaseOperation' = deleteTagDatabaseOperation tagHandle
        , sendResult' = deleteTagSendResult
        }

------------------------------------------------------------------------------------------------------
getTagsListParseInformation ::
       TagsHandle MonadWithError IO -> Request -> MonadWithError (Maybe Page)
getTagsListParseInformation handler request =
    if requestMethod request /= methodGet
        then do
            liftIO $ logError (thLogger handler) "Bad request method"
            throwError BadMethod
        else do
            liftIO $
                logInfo
                    (thLogger handler)
                    "Preparing parameters for sending tags list."
            return (toPage request)

getTagsListDatabaseOperation ::
       TagsHandle MonadWithError IO -> Maybe Page -> MonadWithError TagsList
getTagsListDatabaseOperation = thGetTagsListFromDb

getTagsListSendResult ::
       MonadWithError TagsList
    -> IO (Either ResponseErrorMessage ResponseOkMessage)
getTagsListSendResult result = do
    a <- runExceptT result
    case a of
        Left someError ->
            return $
            Left $ toResponseErrorMessage "List of tags not sended." someError
        Right someList -> do
            return $ Right $ OkJSON $ encode someList

getTagsListHandle ::
       TagsHandle MonadWithError IO
    -> AnswerHandle' (ExceptT SomeError IO) (Maybe Page) TagsList IO
getTagsListHandle tagHandle =
    AnswerHandle'
        { parseInformation' = getTagsListParseInformation tagHandle
        , databaseOperation' = getTagsListDatabaseOperation tagHandle
        , sendResult' = getTagsListSendResult
        }

------------------------------------------------------------------------------------------------------------------------------------
updateTagParseInformation ::
       TagsHandle MonadWithError IO
    -> Request
    -> MonadWithError (Maybe Token, EditTag)
updateTagParseInformation handler request =
    if requestMethod request /= methodPut
        then do
            liftIO $ logError (thLogger handler) "Bad request method"
            throwError BadMethod
        else do
            liftIO $
                logInfo (thLogger handler) "Preparing data for editing tag."
            let token = takeToken request
            (i, _) <- liftIO $ parseRequestBodyLBS request
            let tagEditParams = toEditTag i
            return (token, tagEditParams)

updateTagDatabaseOperation ::
       TagsHandle MonadWithError IO
    -> (Maybe Token, EditTag)
    -> MonadWithError ()
updateTagDatabaseOperation tagHandle (token, tagEditParams) =
    thEditTagInDb tagHandle token tagEditParams

updateTagSendResult ::
       MonadWithError () -> IO (Either ResponseErrorMessage ResponseOkMessage)
updateTagSendResult result = do
    a <- runExceptT result
    case a of
        Left someError ->
            return $ Left $ toResponseErrorMessage "Tag not edited." someError
        Right _ -> do
            return $ Right $ OkMessage "Tag edited."

updateTagHandle ::
       TagsHandle MonadWithError IO
    -> AnswerHandle' (ExceptT SomeError IO) (Maybe Token, EditTag) () IO
updateTagHandle tagHandle =
    AnswerHandle'
        { parseInformation' = updateTagParseInformation tagHandle
        , databaseOperation' = updateTagDatabaseOperation tagHandle
        , sendResult' = updateTagSendResult
        }
