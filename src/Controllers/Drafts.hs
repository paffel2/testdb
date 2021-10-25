{-# LANGUAGE OverloadedStrings #-}

module Controllers.Drafts where

import           Control.Monad.IO.Class    (MonadIO (..))
import           Data.Aeson                (encode)
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as LBS
import           Data.Maybe                (isNothing)
import           FromRequest               (checkNotImageMaybe, checkNotImages,
                                            takeToken, toDraftId, toDraftInf,
                                            toDraftTags, toImage)
import           HelpFunction              (foundParametr, readByteStringToId,
                                            saveHead)
import           Logger                    (logError, logInfo)
import           Network.HTTP.Types.Method (methodDelete, methodGet, methodPost,
                                            methodPut)
import           Network.Wai               (Request (rawPathInfo, requestMethod),
                                            Response)
import           Network.Wai.Parse         (FileInfo (fileContent), lbsBackEnd,
                                            noLimitParseRequestBodyOptions,
                                            parseRequestBodyEx)
import           OperationsHandle          (DraftsHandle (create_draft_on_db, delete_draft_from_db, drafts_logger, get_draft_by_id_from_db, get_drafts_by_author_token, public_news_on_db, update_draft_in_db))
import           Responses                 (badResponse, responseBadRequest,
                                            responseCreated,
                                            responseMethodNotAllowed,
                                            responseNotFound, responseOKJSON,
                                            responseOk)
import           Types.Other               (Id)

sendDrafts :: MonadIO m => DraftsHandle m -> Request -> m Response
sendDrafts operations req =
    if requestMethod req /= methodGet
        then do
            logError (drafts_logger operations) "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo
                (drafts_logger operations)
                "Preparing data for sending drafts"
            let token' = takeToken req
            drafts' <- get_drafts_by_author_token operations token'
            case drafts' of
                Left someError ->
                    return $ badResponse "Drafts not sended." someError
                Right draftsA -> do
                    logInfo (drafts_logger operations) "Sending drafts to user"
                    return $ responseOKJSON (encode draftsA)

createDraft :: MonadIO m => DraftsHandle m -> Request -> m Response
createDraft operations req =
    if requestMethod req /= methodPost
        then do
            logError (drafts_logger operations) "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo
                (drafts_logger operations)
                "Preparing data for creating draft"
            (i, f) <-
                liftIO $
                parseRequestBodyEx noLimitParseRequestBodyOptions lbsBackEnd req
            let draft_inf = toDraftInf req i
            let list_of_tags = toDraftTags i
            let main'_image = foundParametr "main_image" f
            let images = foundParametr "images" f
            let main_image_triple =
                    if isNothing $ fileContent <$> saveHead main'_image
                        then Nothing
                        else Just $ toImage $ Prelude.head main'_image
            let images_list =
                    if isNothing $ fileContent <$> saveHead images
                        then Nothing
                        else Just $ toImage <$> images
            if checkNotImageMaybe main_image_triple ||
               checkNotImages images_list
                then do
                    logError (drafts_logger operations) "Bad image file"
                    return $ responseBadRequest "Bad image file"
                else do
                    result <-
                        create_draft_on_db
                            operations
                            draft_inf
                            list_of_tags
                            main_image_triple
                            images_list
                    case result of
                        Left someError ->
                            return $ badResponse "Draft not created." someError
                        Right n -> do
                            logInfo (drafts_logger operations) "Draft created."
                            return $
                                responseCreated $
                                LBS.fromStrict $ BC.pack $ show n

deleteDraft :: MonadIO m => DraftsHandle m -> Request -> m Response
deleteDraft operations req =
    if requestMethod req /= methodDelete
        then do
            logError (drafts_logger operations) "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo
                (drafts_logger operations)
                "Preparing data for deleting draft"
            let token' = takeToken req
            let draft_id = toDraftId req
            result <- delete_draft_from_db operations token' draft_id
            case result of
                Left someError ->
                    return $ badResponse "Draft not deleted." someError
                Right _ -> do
                    return $ responseOk "Draft deleted."

getDraftById :: MonadIO m => DraftsHandle m -> Id -> Request -> m Response
getDraftById operations draft_id req =
    if requestMethod req /= methodGet
        then do
            logError (drafts_logger operations) "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo
                (drafts_logger operations)
                "Preparing data for sending draft"
            let token' = takeToken req
            result <- get_draft_by_id_from_db operations token' draft_id
            case result of
                Left someError ->
                    return $ badResponse "Draft not sended." someError
                Right draft -> do
                    return $ responseOKJSON $ encode draft

updateDraft :: MonadIO m => DraftsHandle m -> Id -> Request -> m Response
updateDraft operations draft_id req =
    if requestMethod req /= methodPut
        then do
            logError (drafts_logger operations) "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo
                (drafts_logger operations)
                "Preparing data for updating draft"
            (i, f) <-
                liftIO $
                parseRequestBodyEx noLimitParseRequestBodyOptions lbsBackEnd req
            let dr_inf_update = toDraftInf req i
            let list_of_tags = toDraftTags i
            let main'_image = foundParametr "main_image" f
            let images = foundParametr "images" f
            let main_image_triple =
                    if isNothing $ fileContent <$> saveHead main'_image
                        then Nothing
                        else Just $ toImage $ Prelude.head main'_image
            let images_list =
                    if isNothing $ fileContent <$> saveHead images
                        then Nothing
                        else Just $ toImage <$> images
            if checkNotImageMaybe main_image_triple ||
               checkNotImages images_list
                then do
                    logError (drafts_logger operations) "Bad image file"
                    return $ responseBadRequest "Bad image file"
                else do
                    result <-
                        update_draft_in_db
                            operations
                            dr_inf_update
                            list_of_tags
                            main_image_triple
                            images_list
                            draft_id
                    case result of
                        Left someError ->
                            return $ badResponse "Draft not updated." someError
                        Right _ -> do
                            return $ responseOk "Draft updated"

publicNews :: MonadIO m => DraftsHandle m -> Id -> Request -> m Response
publicNews operations draft_id req =
    if requestMethod req /= methodPut
        then do
            logError (drafts_logger operations) "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo (drafts_logger operations) "Preparing data for public news"
            let token' = takeToken req
            result <- public_news_on_db operations token' draft_id
            case result of
                Left someError ->
                    return $ badResponse "News not created." someError
                Right n -> do
                    return $ responseCreated $ LBS.fromStrict $ BC.pack $ show n

draftsRouter :: MonadIO m => DraftsHandle m -> Request -> m Response
draftsRouter operations req
    | pathElemsC == 1 = sendDrafts operations req
    | pathElemsC == 2 =
        case readByteStringToId $ last pathElems of
            Just n -> getDraftById operations n req
            Nothing ->
                case last pathElems of
                    "delete_draft" -> deleteDraft operations req
                    _              -> return $ responseNotFound "Not Found"
    | pathElemsC == 3 =
        case readByteStringToId $ head $ tail pathElems of
            Nothing -> return $ responseBadRequest "bad request"
            Just n ->
                case last pathElems of
                    "update_draft" -> updateDraft operations n req
                    "public_news"  -> publicNews operations n req
                    _              -> return $ responseNotFound "Not Found"
    | otherwise = return $ responseNotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
