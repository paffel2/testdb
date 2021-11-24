{-# LANGUAGE OverloadedStrings #-}

module Controllers.Tags where

import           Answer                (answer'')
import           Answers.Tags          (createTagHandle, deleteTagHandle,
                                        getTagsListHandle, updateTagHandle)
import           Control.Monad.Except  (ExceptT, MonadIO, runExceptT)
import           Data.Aeson            (encode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as LBS
import           Logger                (LoggerHandle, logInfo)
import           Network.Wai           (Request (rawPathInfo))
import           OperationsHandle      (TagsHandle)
import           Responses             (toResponseErrorMessage')
import           Types.Other           (ResponseErrorMessage (NotFound),
                                        ResponseOkMessage (Created, OkJSON, OkMessage),
                                        SendId, SomeError)
import           Types.Tags            (TagsList)

tagsRouter ::
       MonadIO m
    => TagsHandle (ExceptT SomeError m)
    -> LoggerHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
tagsRouter operations hLogger req
    | pathElemsC == 1 =
        getTagsListSendResult hLogger $
        answer'' req (getTagsListHandle operations)
    | pathElemsC == 2 =
        case last pathElems of
            "create_tag" ->
                createTagSendResult hLogger $
                answer'' req (createTagHandle operations)
            "delete_tag" ->
                deleteTagSendResult hLogger $
                answer'' req (deleteTagHandle operations)
            "edit_tag" ->
                updateTagSendResult hLogger $
                answer'' req (updateTagHandle operations)
            _ -> return $ Left $ NotFound "Not Found"
    | otherwise = return $ Left $ NotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems

createTagSendResult ::
       Monad m
    => LoggerHandle m
    -> ExceptT SomeError m SendId
    -> m (Either ResponseErrorMessage ResponseOkMessage)
createTagSendResult hLogger result = do
    a <- runExceptT result
    case a of
        Left someError ->
            Left <$>
            toResponseErrorMessage' hLogger "Tag not created." someError
        Right n -> do
            logInfo hLogger "Tag created."
            return $ Right $ Created $ LBS.fromStrict $ BC.pack $ show n

deleteTagSendResult ::
       Monad m
    => LoggerHandle m
    -> ExceptT SomeError m ()
    -> m (Either ResponseErrorMessage ResponseOkMessage)
deleteTagSendResult hLogger result = do
    a <- runExceptT result
    case a of
        Left someError ->
            Left <$>
            toResponseErrorMessage' hLogger "Tag not deleted." someError
        Right _ -> do
            logInfo hLogger "Tag deleted."
            return $ Right $ OkMessage "Tag deleted."

getTagsListSendResult ::
       Monad m
    => LoggerHandle m
    -> ExceptT SomeError m TagsList
    -> m (Either ResponseErrorMessage ResponseOkMessage)
getTagsListSendResult hLogger result = do
    a <- runExceptT result
    case a of
        Left someError ->
            Left <$>
            toResponseErrorMessage' hLogger "List of tags not sended." someError
        Right someList -> do
            logInfo hLogger "List of tags sended."
            return $ Right $ OkJSON $ encode someList

updateTagSendResult ::
       Monad m
    => LoggerHandle m
    -> ExceptT SomeError m ()
    -> m (Either ResponseErrorMessage ResponseOkMessage)
updateTagSendResult hLogger result = do
    a <- runExceptT result
    case a of
        Left someError ->
            Left <$> toResponseErrorMessage' hLogger "Tag not edited." someError
        Right _ -> do
            logInfo hLogger "Tag edited."
            return $ Right $ OkMessage "Tag edited."
