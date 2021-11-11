{-# LANGUAGE OverloadedStrings #-}

module Answers.Tags where

import           Answer                    (AnswerHandle (..))
import           Data.Aeson                (encode)
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as LBS
import           FromRequest               (takeToken, toEditTag, toPage,
                                            toTagName)
import           Logger                    (logError, logInfo)
import           Network.HTTP.Types.Method (methodDelete, methodGet, methodPost,
                                            methodPut)
import           Network.Wai               (Request (requestMethod))
import           OperationsHandle          (TagsHandle (thCreateTagInDb, thDeleteTagFromDb, thEditTagInDb, thGetTagsListFromDb, thLogger, thParseRequestBody))
import           Responses                 (toResponseErrorMessage)
import           Types.Other               (Page, ResponseErrorMessage,
                                            ResponseOkMessage (Created, OkJSON, OkMessage),
                                            SendId, SomeError (BadMethod),
                                            Token)
import           Types.Tags                (EditTag, TagName, TagsList)

updateTagParseInformation ::
       Monad m
    => TagsHandle m
    -> Request
    -> m (Either SomeError (Maybe Token, EditTag))
updateTagParseInformation handler request =
    if requestMethod request /= methodPut
        then do
            logError (thLogger handler) "Bad request method"
            return $ Left BadMethod
        else do
            logInfo (thLogger handler) "Preparing data for editing tag."
            let token = takeToken request
            (i, _) <- thParseRequestBody handler request
            let tagEditParams = toEditTag i
            return $ Right (token, tagEditParams)

updateTagDatabaseOperation ::
       Monad m
    => TagsHandle m
    -> Either SomeError (Maybe Token, EditTag)
    -> m (Either SomeError ())
updateTagDatabaseOperation _ (Left message) = return $ Left message
updateTagDatabaseOperation tagHandle (Right (token, tagEditParams)) =
    thEditTagInDb tagHandle token tagEditParams

updateTagSendResult ::
       Monad m
    => Either SomeError ()
    -> m (Either ResponseErrorMessage ResponseOkMessage)
updateTagSendResult result =
    case result of
        Left someError ->
            return $ Left $ toResponseErrorMessage "Tag not edited." someError
        Right _ -> do
            return $ Right $ OkMessage "Tag edited."

updateTagHandle ::
       Monad m
    => TagsHandle m
    -> AnswerHandle m (Either SomeError (Maybe Token, EditTag)) ()
updateTagHandle tagHandle =
    AnswerHandle
        { parseInformation = updateTagParseInformation tagHandle
        , databaseOperation = updateTagDatabaseOperation tagHandle
        , sendResult = updateTagSendResult
        }

--------------------------------------------------------------------------------------------------------------------
getTagsListParseInformation ::
       Monad m => TagsHandle m -> Request -> m (Either SomeError (Maybe Page))
getTagsListParseInformation handler request =
    if requestMethod request /= methodGet
        then do
            logError (thLogger handler) "Bad request method"
            return $ Left BadMethod
        else do
            logInfo
                (thLogger handler)
                "Preparing parameters for sending tags list."
            return $ Right (toPage request)

getTagsListDatabaseOperation ::
       Monad m
    => TagsHandle m
    -> Either SomeError (Maybe Page)
    -> m (Either SomeError TagsList)
getTagsListDatabaseOperation _ (Left message) = return $ Left message
getTagsListDatabaseOperation tagHandle (Right page) =
    thGetTagsListFromDb tagHandle page

getTagsListSendResult ::
       Monad m
    => Either SomeError TagsList
    -> m (Either ResponseErrorMessage ResponseOkMessage)
getTagsListSendResult result =
    case result of
        Left someError ->
            return $
            Left $ toResponseErrorMessage "List of tags not sended." someError
        Right someList -> do
            return $ Right $ OkJSON $ encode someList

getTagsListHandle ::
       Monad m
    => TagsHandle m
    -> AnswerHandle m (Either SomeError (Maybe Page)) TagsList
getTagsListHandle tagHandle =
    AnswerHandle
        { parseInformation = getTagsListParseInformation tagHandle
        , databaseOperation = getTagsListDatabaseOperation tagHandle
        , sendResult = getTagsListSendResult
        }

---------------------------------------------------------------------------------------------------------
createTagParseInformation ::
       Monad m
    => TagsHandle m
    -> Request
    -> m (Either SomeError (Maybe Token, Maybe TagName))
createTagParseInformation handler request =
    if requestMethod request /= methodPost
        then do
            logError (thLogger handler) "Bad request method"
            return $ Left BadMethod
        else do
            logInfo (thLogger handler) "Preparing data for creating tag."
            let token = takeToken request
            let tagNameParam = toTagName request
            return $ Right (token, tagNameParam)

createTagDatabaseOperation ::
       Monad m
    => TagsHandle m
    -> Either SomeError (Maybe Token, Maybe TagName)
    -> m (Either SomeError SendId)
createTagDatabaseOperation _ (Left message) = return $ Left message
createTagDatabaseOperation tagHandle (Right (token, tagNameParam)) =
    thCreateTagInDb tagHandle token tagNameParam

createTagSendResult ::
       Monad m
    => Either SomeError SendId
    -> m (Either ResponseErrorMessage ResponseOkMessage)
createTagSendResult result =
    case result of
        Left someError ->
            return $ Left $ toResponseErrorMessage "Tag not created." someError
        Right n -> do
            return $ Right $ Created $ LBS.fromStrict $ BC.pack $ show n

createTagHandle ::
       Monad m
    => TagsHandle m
    -> AnswerHandle m (Either SomeError (Maybe Token, Maybe TagName)) SendId
createTagHandle tagHandle =
    AnswerHandle
        { parseInformation = createTagParseInformation tagHandle
        , databaseOperation = createTagDatabaseOperation tagHandle
        , sendResult = createTagSendResult
        }

--------------------------------------------------------------------------------------------------------------------
deleteTagParseInformation ::
       Monad m
    => TagsHandle m
    -> Request
    -> m (Either SomeError (Maybe Token, Maybe TagName))
deleteTagParseInformation handler request =
    if requestMethod request /= methodDelete
        then do
            logError (thLogger handler) "Bad request method"
            return $ Left BadMethod
        else do
            logInfo (thLogger handler) "Preparing data for deleting tag."
            let token = takeToken request
            let tagNameParam = toTagName request
            return $ Right (token, tagNameParam)

deleteTagDatabaseOperation ::
       Monad m
    => TagsHandle m
    -> Either SomeError (Maybe Token, Maybe TagName)
    -> m (Either SomeError ())
deleteTagDatabaseOperation _ (Left message) = return $ Left message
deleteTagDatabaseOperation tagHandle (Right (token, tagNameParam)) =
    thDeleteTagFromDb tagHandle token tagNameParam

deleteTagSendResult ::
       Monad m
    => Either SomeError ()
    -> m (Either ResponseErrorMessage ResponseOkMessage)
deleteTagSendResult result =
    case result of
        Left someError ->
            return $ Left $ toResponseErrorMessage "Tag not deleted." someError
        Right _ -> do
            return $ Right $ OkMessage "Tag deleted."

deleteTagHandle ::
       Monad m
    => TagsHandle m
    -> AnswerHandle m (Either SomeError (Maybe Token, Maybe TagName)) ()
deleteTagHandle tagHandle =
    AnswerHandle
        { parseInformation = deleteTagParseInformation tagHandle
        , databaseOperation = deleteTagDatabaseOperation tagHandle
        , sendResult = deleteTagSendResult
        }
--------------------------------------------------------------------------------------------------------------------
