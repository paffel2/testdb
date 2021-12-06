{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Answers.Tags where

import           Answer                    (AnswerHandle (..))
import           Control.Monad.Except      (MonadError (throwError))
import           FromRequest               (takeToken, toEditTag, toPage,
                                            toTagName)
import           Network.HTTP.Types.Method (methodDelete, methodGet, methodPost,
                                            methodPut)
import           Network.Wai               (Request (requestMethod))
import           OperationsHandle          (TagsHandle (thCreateTagInDb, thDeleteTagFromDb, thEditTagInDb, thGetTagsListFromDb, thParseRequestBody))
import           Types.Other               (MonadIOWithError, Page, SendId,
                                            SomeError (BadMethod), Token)
import           Types.Tags                (EditTag, TagName, TagsList)

-------------------------------------------------------------------------------------
createTagParseInformation ::
       MonadIOWithError m
    => TagsHandle m
    -> Request
    -> m (Maybe Token, Maybe TagName)
createTagParseInformation _ request =
    if requestMethod request /= methodPost
        then throwError BadMethod
        else do
            let token = takeToken request
            let tagNameParam = toTagName request
            return (token, tagNameParam)

createTagDatabaseOperation ::
       MonadIOWithError m
    => TagsHandle m
    -> (Maybe Token, Maybe TagName)
    -> m SendId
createTagDatabaseOperation tagHandle (token, tagNameParam) =
    thCreateTagInDb tagHandle token tagNameParam

createTagHandle ::
       MonadIOWithError m
    => TagsHandle m
    -> AnswerHandle m (Maybe Token, Maybe TagName) SendId
createTagHandle tagHandle =
    AnswerHandle
        { parseInformation = createTagParseInformation tagHandle
        , databaseOperation = createTagDatabaseOperation tagHandle
        }

deleteTagParseInformation ::
       MonadIOWithError m
    => TagsHandle m
    -> Request
    -> m (Maybe Token, Maybe TagName)
deleteTagParseInformation _ request =
    if requestMethod request /= methodDelete
        then throwError BadMethod
        else do
            let token = takeToken request
            let tagNameParam = toTagName request
            return (token, tagNameParam)

deleteTagDatabaseOperation ::
       MonadIOWithError m
    => TagsHandle m
    -> (Maybe Token, Maybe TagName)
    -> m ()
deleteTagDatabaseOperation tagHandle (token, tagNameParam) =
    thDeleteTagFromDb tagHandle token tagNameParam

deleteTagHandle ::
       MonadIOWithError m
    => TagsHandle m
    -> AnswerHandle m (Maybe Token, Maybe TagName) ()
deleteTagHandle tagHandle =
    AnswerHandle
        { parseInformation = deleteTagParseInformation tagHandle
        , databaseOperation = deleteTagDatabaseOperation tagHandle
        }

---------------------------------------------------------------------------
getTagsListParseInformation ::
       MonadIOWithError m => TagsHandle m -> Request -> m (Maybe Page)
getTagsListParseInformation _ request =
    if requestMethod request /= methodGet
        then throwError BadMethod
        else return (toPage request)

getTagsListDatabaseOperation ::
       MonadIOWithError m => TagsHandle m -> Maybe Page -> m TagsList
getTagsListDatabaseOperation = thGetTagsListFromDb

getTagsListHandle ::
       MonadIOWithError m
    => TagsHandle m
    -> AnswerHandle m (Maybe Page) TagsList
getTagsListHandle tagHandle =
    AnswerHandle
        { parseInformation = getTagsListParseInformation tagHandle
        , databaseOperation = getTagsListDatabaseOperation tagHandle
        }

---------------------------------------------------------------------------
updateTagParseInformation ::
       MonadIOWithError m => TagsHandle m -> Request -> m (Maybe Token, EditTag)
updateTagParseInformation handler request =
    if requestMethod request /= methodPut
        then throwError BadMethod
        else do
            let token = takeToken request
            (i, _) <- thParseRequestBody handler request
            let tagEditParams = toEditTag i
            return (token, tagEditParams)

updateTagDatabaseOperation ::
       MonadIOWithError m => TagsHandle m -> (Maybe Token, EditTag) -> m ()
updateTagDatabaseOperation tagHandle (token, tagEditParams) =
    thEditTagInDb tagHandle token tagEditParams

updateTagHandle ::
       MonadIOWithError m
    => TagsHandle m
    -> AnswerHandle m (Maybe Token, EditTag) ()
updateTagHandle tagHandle =
    AnswerHandle
        { parseInformation = updateTagParseInformation tagHandle
        , databaseOperation = updateTagDatabaseOperation tagHandle
        }
