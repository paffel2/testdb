{-# LANGUAGE OverloadedStrings #-}

module Controllers.Drafts where

import           Answer                (answer'')
import           Answers.Drafts        (createDraftHandle, deleteDraftHandle,
                                        getDraftByIdHandle, getDraftsHandle,
                                        postNewsHandle, updateDraftHandle)
import           Control.Monad.Except  (ExceptT, MonadIO, runExceptT)
import           Data.Aeson            (encode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as LBS
import           HelpFunction          (readByteStringToId)
import           Logger                (LoggerHandle, logInfo)
import           Network.Wai           (Request (rawPathInfo))
import           OperationsHandle      (DraftsHandle)
import           Responses             (toResponseErrorMessage')
import           Types.Drafts          (Draft, DraftArray)
import           Types.Other           (ResponseErrorMessage (BadRequest, NotFound),
                                        ResponseOkMessage (Created, OkJSON, OkMessage),
                                        SendId, SomeError)

draftsRouter ::
       MonadIO m
    => DraftsHandle (ExceptT SomeError m)
    -> LoggerHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
draftsRouter operations hLogger req
    | pathElemsC == 1 =
        getDraftsSendResult hLogger $ answer'' req (getDraftsHandle operations)
    | pathElemsC == 2 =
        case readByteStringToId $ last pathElems of
            Just _ ->
                getDraftByIdSendResult hLogger $
                answer'' req (getDraftByIdHandle operations)
            Nothing ->
                case last pathElems of
                    "delete_draft" ->
                        deleteDraftSendResult hLogger $
                        answer'' req (deleteDraftHandle operations)
                    _ -> return $ Left $ NotFound "Not Found"
    | pathElemsC == 3 =
        case readByteStringToId $ head $ tail pathElems of
            Nothing -> return $ Left $ BadRequest "Bad draft id"
            Just _ ->
                case last pathElems of
                    "update_draft" ->
                        updateDraftSendResult hLogger $
                        answer'' req (updateDraftHandle operations)
                    "public_news" ->
                        postNewsSendResult hLogger $
                        answer'' req (postNewsHandle operations)
                    _ -> return $ Left $ NotFound "Not Found"
    | otherwise = return $ Left $ NotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems

getDraftsSendResult ::
       Monad m
    => LoggerHandle m
    -> ExceptT SomeError m DraftArray
    -> m (Either ResponseErrorMessage ResponseOkMessage)
getDraftsSendResult hLogger result = do
    a <- runExceptT result
    case a of
        Left someError ->
            Left <$>
            toResponseErrorMessage'
                hLogger
                "List of drafts not sended."
                someError
        Right someList -> do
            logInfo hLogger "List of drafts sended."
            return $ Right $ OkJSON $ encode someList

getDraftByIdSendResult ::
       Monad m
    => LoggerHandle m
    -> ExceptT SomeError m Draft
    -> m (Either ResponseErrorMessage ResponseOkMessage)
getDraftByIdSendResult hLogger result = do
    a <- runExceptT result
    case a of
        Left someError ->
            Left <$>
            toResponseErrorMessage' hLogger "Draft not sended." someError
        Right someDraft -> do
            logInfo hLogger "Draft sended."
            return $ Right $ OkJSON $ encode someDraft

deleteDraftSendResult ::
       Monad m
    => LoggerHandle m
    -> ExceptT SomeError m ()
    -> m (Either ResponseErrorMessage ResponseOkMessage)
deleteDraftSendResult hLogger result = do
    a <- runExceptT result
    case a of
        Left someError ->
            Left <$>
            toResponseErrorMessage' hLogger "Draft not deleted." someError
        Right _ -> do
            logInfo hLogger "Draft deleted."
            return $ Right $ OkMessage "Draft deleted."

postNewsSendResult ::
       Monad m
    => LoggerHandle m
    -> ExceptT SomeError m SendId
    -> m (Either ResponseErrorMessage ResponseOkMessage)
postNewsSendResult hLogger result = do
    a <- runExceptT result
    case a of
        Left someError ->
            Left <$>
            toResponseErrorMessage' hLogger "News not created." someError
        Right newsId -> do
            logInfo hLogger "News created."
            return $ Right $ Created $ LBS.fromStrict $ BC.pack $ show newsId

createDraftSendResult ::
       Monad m
    => LoggerHandle m
    -> ExceptT SomeError m SendId
    -> m (Either ResponseErrorMessage ResponseOkMessage)
createDraftSendResult hLogger result = do
    a <- runExceptT result
    case a of
        Left someError ->
            Left <$>
            toResponseErrorMessage' hLogger "Draft not created." someError
        Right draftId -> do
            logInfo hLogger "Draft created."
            return $ Right $ Created $ LBS.fromStrict $ BC.pack $ show draftId

createDraft ::
       MonadIO m
    => DraftsHandle (ExceptT SomeError m)
    -> LoggerHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
createDraft operations hLogger req =
    createDraftSendResult hLogger $ answer'' req (createDraftHandle operations)

updateDraftSendResult ::
       Monad m
    => LoggerHandle m
    -> ExceptT SomeError m ()
    -> m (Either ResponseErrorMessage ResponseOkMessage)
updateDraftSendResult hLogger result = do
    a <- runExceptT result
    case a of
        Left someError ->
            Left <$>
            toResponseErrorMessage' hLogger "Draft not updated." someError
        Right _ -> do
            logInfo hLogger "Draft updated"
            return $ Right $ OkMessage "Draft updated"
