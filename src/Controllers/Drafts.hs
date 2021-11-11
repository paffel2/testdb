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
import           OperationsHandle          (DraftsHandle (dhCreateDraftOnDb, dhDeleteDraftFromDb, dhGetDraftByIdFromDb, dhGetDraftsByAuthorToken, dhLogger, dhParseRequestBody, dhPublicNewsOnDb, dhUpdateDraftInDb))
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
            logError (dhLogger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo (dhLogger operations) "Preparing data for sending drafts"
            let token = takeToken req
            listOfDrafts <- dhGetDraftsByAuthorToken operations token
            case listOfDrafts of
                Left someError ->
                    return $
                    Left $
                    toResponseErrorMessage
                        "List of drafts not sended."
                        someError
                Right draftsA -> do
                    logInfo (dhLogger operations) "Sending drafts to user"
                    return $ Right $ OkJSON (encode draftsA)

postDraft ::
       Monad m
    => DraftsHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
postDraft operations req =
    if requestMethod req /= methodPost
        then do
            logError (dhLogger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo (dhLogger operations) "Preparing data for creating draft"
            (i, f) <- dhParseRequestBody operations req
            let draftInf = toDraftInf req i
            let listOfTags = toDraftTags i
            let mainImageInfo = foundParametr "main_image" f
            let imagesInfo = foundParametr "images" f
            let mainImage =
                    if isNothing $ fileContent <$> saveHead mainImageInfo
                        then Nothing
                        else Just $ toImage $ Prelude.head mainImageInfo
            let imagesList =
                    if isNothing $ fileContent <$> saveHead imagesInfo
                        then Nothing
                        else Just $ toImage <$> imagesInfo
            if checkNotImageMaybe mainImage || checkNotImages imagesList
                then do
                    logError (dhLogger operations) "Bad image file"
                    return $ Left $ BadRequest "Bad image file"
                else do
                    result <-
                        dhCreateDraftOnDb
                            operations
                            draftInf
                            listOfTags
                            mainImage
                            imagesList
                    case result of
                        Left someError ->
                            return $
                            Left $
                            toResponseErrorMessage
                                "Draft not created."
                                someError
                        Right n -> do
                            logInfo (dhLogger operations) "Draft created."
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
            logError (dhLogger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo (dhLogger operations) "Preparing data for deleting draft"
            let token' = takeToken req
            let draftId = toDraftId req
            result <- dhDeleteDraftFromDb operations token' draftId
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
            logError (dhLogger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo (dhLogger operations) "Preparing data for sending draft"
            let token' = takeToken req
            result <- dhGetDraftByIdFromDb operations token' draft_id
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
updateDraft operations draftId req =
    if requestMethod req /= methodPut
        then do
            logError (dhLogger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo (dhLogger operations) "Preparing data for updating draft"
            (i, f) <- dhParseRequestBody operations req
            let draftInfUpdate = toDraftInf req i
            let listOfTags = toDraftTags i
            let mainImageInfo = foundParametr "main_image" f
            let imagesInfo = foundParametr "images" f
            let mainImage =
                    if isNothing $ fileContent <$> saveHead mainImageInfo
                        then Nothing
                        else Just $ toImage $ Prelude.head mainImageInfo
            let imagesList =
                    if isNothing $ fileContent <$> saveHead imagesInfo
                        then Nothing
                        else Just $ toImage <$> imagesInfo
            if checkNotImageMaybe mainImage || checkNotImages imagesList
                then do
                    logError (dhLogger operations) "Bad image file"
                    return $ Left $ BadRequest "Bad image file"
                else do
                    result <-
                        dhUpdateDraftInDb
                            operations
                            draftInfUpdate
                            listOfTags
                            mainImage
                            imagesList
                            draftId
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
postNews operations draftId req =
    if requestMethod req /= methodPut
        then do
            logError (dhLogger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo (dhLogger operations) "Preparing data for public news"
            let token = takeToken req
            result <- dhPublicNewsOnDb operations token draftId
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
