{-# LANGUAGE OverloadedStrings #-}

module Controllers.Tags where

import           Answer                (answer)
import           Answers.Tags          (createTagHandle, deleteTagHandle,
                                        getTagsListHandle, updateTagHandle)
import           Control.Monad.Except  (ExceptT, MonadIO)
import           Data.Aeson            (encode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as LBS
import           HelpFunction          (sendResult)
import           Logger                (LoggerHandle)
import           Network.Wai           (Request (rawPathInfo))
import           OperationsHandle      (TagsHandle)
import           Types.Other           (ResponseErrorMessage (NotFound),
                                        ResponseOkMessage (Created, OkJSON, OkMessage),
                                        SomeError)

tagsRouter ::
       MonadIO m
    => TagsHandle (ExceptT SomeError m)
    -> LoggerHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
tagsRouter operations hLogger req
    | pathElemsC == 1 =
        sendResult hLogger "List of tags not sended." tagListOK $
        answer req (getTagsListHandle operations)
    | pathElemsC == 2 =
        case last pathElems of
            "create_tag" ->
                sendResult hLogger "Tag not created." createTagOK $
                answer req (createTagHandle operations)
            "delete_tag" ->
                sendResult hLogger "Tag not deleted." deleteTag $
                answer req (deleteTagHandle operations)
            "edit_tag" ->
                sendResult hLogger "Tag not edited." updateTag $
                answer req (updateTagHandle operations)
            _ -> return $ Left $ NotFound "Not Found"
    | otherwise = return $ Left $ NotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
    tagListOK someList = OkJSON $ encode someList
    createTagOK someId = Created $ LBS.fromStrict $ BC.pack $ show someId
    deleteTag _ = OkMessage "Tag deleted."
    updateTag _ = OkMessage "Tag edited."
