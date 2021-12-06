{-# LANGUAGE OverloadedStrings #-}

module Controllers.Drafts where

import           Answer                (answer)
import           Answers.Drafts        (createDraftHandle, deleteDraftHandle,
                                        getDraftByIdHandle, getDraftsHandle,
                                        postNewsHandle, updateDraftHandle)
import           Control.Monad.Except  (ExceptT, MonadIO)
import           Data.Aeson            (encode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as LBS
import           HelpFunction          (readByteStringToId, sendResult)
import           Logger                (LoggerHandle)
import           Network.Wai           (Request (rawPathInfo))
import           OperationsHandle      (DraftsHandle)
import           Types.Other           (ResponseErrorMessage (BadRequest, NotFound),
                                        ResponseOkMessage (Created, OkJSON, OkMessage),
                                        SomeError)

draftsRouter ::
       MonadIO m
    => DraftsHandle (ExceptT SomeError m)
    -> LoggerHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
draftsRouter operations hLogger req
    | pathElemsC == 1 =
        sendResult hLogger "List of drafts not sended." listOfDraftsOK $
        answer req (getDraftsHandle operations)
    | pathElemsC == 2 =
        case readByteStringToId $ last pathElems of
            Just _ ->
                sendResult hLogger "Draft not sended." getDraftOk $
                answer req (getDraftByIdHandle operations)
            Nothing ->
                case last pathElems of
                    "delete_draft" ->
                        sendResult hLogger "Draft not deleted." deleteDraftOK $
                        answer req (deleteDraftHandle operations)
                    _ -> return $ Left $ NotFound "Not Found"
    | pathElemsC == 3 =
        case readByteStringToId $ head $ tail pathElems of
            Nothing -> return $ Left $ BadRequest "Bad draft id"
            Just _ ->
                case last pathElems of
                    "update_draft" ->
                        sendResult hLogger "Draft not updated." updateDraftOk $
                        answer req (updateDraftHandle operations)
                    "public_news" ->
                        sendResult hLogger "News not created." postNewsOk $
                        answer req (postNewsHandle operations)
                    _ -> return $ Left $ NotFound "Not Found"
    | otherwise = return $ Left $ NotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
    listOfDraftsOK someList = OkJSON $ encode someList
    getDraftOk dr = OkJSON $ encode dr
    deleteDraftOK _ = OkMessage "Draft deleted."
    updateDraftOk _ = OkMessage "Draft updated"
    postNewsOk newsId = Created $ LBS.fromStrict $ BC.pack $ show newsId

createDraft ::
       MonadIO m
    => DraftsHandle (ExceptT SomeError m)
    -> LoggerHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
createDraft operations hLogger req =
    sendResult hLogger "Draft not created." createDraftOk $
    answer req (createDraftHandle operations)
  where
    createDraftOk draftId = Created $ LBS.fromStrict $ BC.pack $ show draftId
