{-# LANGUAGE OverloadedStrings #-}

module Controllers.Drafts where

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
import           Network.Wai               (Request (rawPathInfo, requestMethod))
import           Network.Wai.Parse         (FileInfo (fileContent))
import           OperationsHandle          (DraftsHandle (create_draft_on_db, delete_draft_from_db, drafts_logger, drafts_parse_request_body, get_draft_by_id_from_db, get_drafts_by_author_token, public_news_on_db, update_draft_in_db))
import           Responses                 (toResponseErrorMessage)
import           Types.Other               (Id,
                                            ResponseErrorMessage (BadRequest, MethodNotAllowed, NotFound),
                                            ResponseOkMessage (Created, OkJSON, OkMessage))

getDrafts ::
       Monad m
    => DraftsHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
getDrafts operations req =
    if requestMethod req /= methodGet
        then do
            logError (drafts_logger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo
                (drafts_logger operations)
                "Preparing data for sending drafts"
            let token' = takeToken req
            drafts' <- get_drafts_by_author_token operations token'
            case drafts' of
                Left someError ->
                    return $
                    Left $
                    toResponseErrorMessage
                        "List of drafts not sended."
                        someError
                Right draftsA -> do
                    logInfo (drafts_logger operations) "Sending drafts to user"
                    return $ Right $ OkJSON (encode draftsA)

postDraft ::
       Monad m
    => DraftsHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
postDraft operations req =
    if requestMethod req /= methodPost
        then do
            logError (drafts_logger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo
                (drafts_logger operations)
                "Preparing data for creating draft"
            (i, f) <- drafts_parse_request_body operations req
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
                    return $ Left $ BadRequest "Bad image file"
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
                            return $
                            Left $
                            toResponseErrorMessage
                                "Draft not created."
                                someError
                        Right n -> do
                            logInfo (drafts_logger operations) "Draft created."
                            return $
                                Right $
                                Created $ LBS.fromStrict $ BC.pack $ show n

deleteDraft ::
       Monad m
    => DraftsHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
deleteDraft operations req =
    if requestMethod req /= methodDelete
        then do
            logError (drafts_logger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo
                (drafts_logger operations)
                "Preparing data for deleting draft"
            let token' = takeToken req
            let draft_id = toDraftId req
            result <- delete_draft_from_db operations token' draft_id
            case result of
                Left someError ->
                    return $
                    Left $ toResponseErrorMessage "Draft not deleted." someError
                Right _ -> do
                    return $ Right $ OkMessage "Draft deleted."

getDraftById ::
       Monad m
    => DraftsHandle m
    -> Id
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
getDraftById operations draft_id req =
    if requestMethod req /= methodGet
        then do
            logError (drafts_logger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo
                (drafts_logger operations)
                "Preparing data for sending draft"
            let token' = takeToken req
            result <- get_draft_by_id_from_db operations token' draft_id
            case result of
                Left someError ->
                    return $
                    Left $ toResponseErrorMessage "Draft not sended." someError
                Right draft -> do
                    return $ Right $ OkJSON $ encode draft

updateDraft ::
       Monad m
    => DraftsHandle m
    -> Id
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
updateDraft operations draft_id req =
    if requestMethod req /= methodPut
        then do
            logError (drafts_logger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo
                (drafts_logger operations)
                "Preparing data for updating draft"
            (i, f) <- drafts_parse_request_body operations req
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
                    return $ Left $ BadRequest "Bad image file"
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
                            return $
                            Left $
                            toResponseErrorMessage
                                "Draft not updated."
                                someError
                        Right _ -> do
                            return $ Right $ OkMessage "Draft updated"

postNews ::
       Monad m
    => DraftsHandle m
    -> Id
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
postNews operations draft_id req =
    if requestMethod req /= methodPut
        then do
            logError (drafts_logger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo (drafts_logger operations) "Preparing data for public news"
            let token' = takeToken req
            result <- public_news_on_db operations token' draft_id
            case result of
                Left someError ->
                    return $
                    Left $ toResponseErrorMessage "News not created." someError
                Right n -> do
                    return $ Right $ Created $ LBS.fromStrict $ BC.pack $ show n

draftsRouter ::
       Monad m
    => DraftsHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
draftsRouter operations req
    | pathElemsC == 1 = getDrafts operations req
    | pathElemsC == 2 =
        case readByteStringToId $ last pathElems of
            Just n -> getDraftById operations n req
            Nothing ->
                case last pathElems of
                    "delete_draft" -> deleteDraft operations req
                    _              -> return $ Left $ NotFound "Not Found"
    | pathElemsC == 3 =
        case readByteStringToId $ head $ tail pathElems of
            Nothing -> return $ Left $ BadRequest "Bad draft id"
            Just n ->
                case last pathElems of
                    "update_draft" -> updateDraft operations n req
                    "public_news"  -> postNews operations n req
                    _              -> return $ Left $ NotFound "Not Found"
    | otherwise = return $ Left $ NotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
