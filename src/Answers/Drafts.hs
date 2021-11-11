{-# LANGUAGE OverloadedStrings #-}

module Answers.Drafts where

import           Answer                    (AnswerHandle (..))
import           Data.Aeson                (encode)
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as LBS
import           Data.Maybe                (fromJust, isNothing)
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
import           Types.Drafts              (Draft, DraftArray, DraftInf,
                                            DraftTags)
import           Types.Images              (Image)
import           Types.Other               (Id, ResponseErrorMessage,
                                            ResponseOkMessage (Created, OkJSON, OkMessage),
                                            SendId,
                                            SomeError (BadMethod, OtherError),
                                            Token)

updateDraftParseInformation ::
       Monad m
    => DraftsHandle m
    -> Request
    -> m (Either SomeError ( DraftInf
                           , Maybe DraftTags
                           , Maybe Image
                           , Maybe [Image]
                           , Id))
updateDraftParseInformation handler request =
    if requestMethod request /= methodPut
        then do
            logError (dhLogger handler) "Bad request method"
            return $ Left BadMethod
        else do
            logInfo (dhLogger handler) "Preparing data for updating draft"
            (i, f) <- dhParseRequestBody handler request
            let draftInfUpdate = toDraftInf request i
            let listOfTags = toDraftTags i
            let mainImageInfo = foundParametr "main_image" f
            let imagesInfo = foundParametr "images" f
            let draftId = fromJust $ readByteStringToId $ last pathElems
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
                    logError (dhLogger handler) "Bad image file"
                    return $ Left $ OtherError "Bad image file"
                else return $
                     Right
                         ( draftInfUpdate
                         , listOfTags
                         , mainImage
                         , imagesList
                         , draftId)
  where
    path = BC.tail $ rawPathInfo request
    pathElems = BC.split '/' path

updateDraftDatabaseOperation ::
       Monad m
    => DraftsHandle m
    -> Either SomeError ( DraftInf
                        , Maybe DraftTags
                        , Maybe Image
                        , Maybe [Image]
                        , Id)
    -> m (Either SomeError ())
updateDraftDatabaseOperation _ (Left message) = return $ Left message
updateDraftDatabaseOperation draftHandle (Right (draftInfUpdate, listOfTags, mainImage, imagesList, draftId)) =
    dhUpdateDraftInDb
        draftHandle
        draftInfUpdate
        listOfTags
        mainImage
        imagesList
        draftId

updateDraftSendResult ::
       Monad m
    => Either SomeError ()
    -> m (Either ResponseErrorMessage ResponseOkMessage)
updateDraftSendResult result =
    case result of
        Left someError ->
            return $
            Left $ toResponseErrorMessage "Draft not updated." someError
        Right _ -> do
            return $ Right $ OkMessage "Draft updated"

updateDraftHandle ::
       Monad m
    => DraftsHandle m
    -> AnswerHandle m (Either SomeError ( DraftInf
                                        , Maybe DraftTags
                                        , Maybe Image
                                        , Maybe [Image]
                                        , Id)) ()
updateDraftHandle draftHandle =
    AnswerHandle
        { parseInformation = updateDraftParseInformation draftHandle
        , databaseOperation = updateDraftDatabaseOperation draftHandle
        , sendResult = updateDraftSendResult
        }

--------------------------------------------------------------------------------------------------------------------
getDraftsParseInformation ::
       Monad m
    => DraftsHandle m
    -> Request
    -> m (Either SomeError (Maybe Token))
getDraftsParseInformation handler request =
    if requestMethod request /= methodGet
        then do
            logError (dhLogger handler) "Bad request method"
            return $ Left BadMethod
        else do
            logInfo (dhLogger handler) "Preparing data for sending drafts"
            let token = takeToken request
            return $ Right token

getDraftsDatabaseOperation ::
       Monad m
    => DraftsHandle m
    -> Either SomeError (Maybe Token)
    -> m (Either SomeError DraftArray)
getDraftsDatabaseOperation _ (Left message) = return $ Left message
getDraftsDatabaseOperation draftHandle (Right token) =
    dhGetDraftsByAuthorToken draftHandle token

getDraftsSendResult ::
       Monad m
    => Either SomeError DraftArray
    -> m (Either ResponseErrorMessage ResponseOkMessage)
getDraftsSendResult result =
    case result of
        Left someError ->
            return $
            Left $ toResponseErrorMessage "List of drafts not sended." someError
        Right draftsA -> do
            return $ Right $ OkJSON (encode draftsA)

getDraftsHandle ::
       Monad m
    => DraftsHandle m
    -> AnswerHandle m (Either SomeError (Maybe Token)) DraftArray
getDraftsHandle draftHandle =
    AnswerHandle
        { parseInformation = getDraftsParseInformation draftHandle
        , databaseOperation = getDraftsDatabaseOperation draftHandle
        , sendResult = getDraftsSendResult
        }

--------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------
getDraftByIdInformation ::
       Monad m
    => DraftsHandle m
    -> Request
    -> m (Either SomeError (Maybe Token, Id))
getDraftByIdInformation handler request =
    if requestMethod request /= methodGet
        then do
            logError (dhLogger handler) "Bad request method"
            return $ Left BadMethod
        else do
            logInfo (dhLogger handler) "Preparing data for sending draft"
            let token = takeToken request
            let draftId = fromJust $ readByteStringToId $ last pathElems
            return $ Right (token, draftId)
  where
    path = BC.tail $ rawPathInfo request
    pathElems = BC.split '/' path

getDraftByIdDatabaseOperation ::
       Monad m
    => DraftsHandle m
    -> Either SomeError (Maybe Token, Id)
    -> m (Either SomeError Draft)
getDraftByIdDatabaseOperation _ (Left message) = return $ Left message
getDraftByIdDatabaseOperation draftHandle (Right (token, draftId)) =
    dhGetDraftByIdFromDb draftHandle token draftId

getDraftByIdSendResult ::
       Monad m
    => Either SomeError Draft
    -> m (Either ResponseErrorMessage ResponseOkMessage)
getDraftByIdSendResult result =
    case result of
        Left someError ->
            return $ Left $ toResponseErrorMessage "Draft not sended." someError
        Right draft -> do
            return $ Right $ OkJSON $ encode draft

getDraftByIdHandle ::
       Monad m
    => DraftsHandle m
    -> AnswerHandle m (Either SomeError (Maybe Token, Id)) Draft
getDraftByIdHandle draftHandle =
    AnswerHandle
        { parseInformation = getDraftByIdInformation draftHandle
        , databaseOperation = getDraftByIdDatabaseOperation draftHandle
        , sendResult = getDraftByIdSendResult
        }

--------------------------------------------------------------------------------------------------------------------
deleteDraftInformation ::
       Monad m
    => DraftsHandle m
    -> Request
    -> m (Either SomeError (Maybe Token, Maybe Id))
deleteDraftInformation handler request =
    if requestMethod request /= methodDelete
        then do
            logError (dhLogger handler) "Bad request method"
            return $ Left BadMethod
        else do
            logInfo (dhLogger handler) "Preparing data for deleting draft"
            let token = takeToken request
            let draftId = toDraftId request
            return $ Right (token, draftId)

deleteDraftDatabaseOperation ::
       Monad m
    => DraftsHandle m
    -> Either SomeError (Maybe Token, Maybe Id)
    -> m (Either SomeError ())
deleteDraftDatabaseOperation _ (Left message) = return $ Left message
deleteDraftDatabaseOperation draftHandle (Right (token, draftId)) =
    dhDeleteDraftFromDb draftHandle token draftId

deleteDraftSendResult ::
       Monad m
    => Either SomeError ()
    -> m (Either ResponseErrorMessage ResponseOkMessage)
deleteDraftSendResult result =
    case result of
        Left someError ->
            return $
            Left $ toResponseErrorMessage "Draft not deleted." someError
        Right _ -> do
            return $ Right $ OkMessage "Draft deleted."

deleteDraftHandle ::
       Monad m
    => DraftsHandle m
    -> AnswerHandle m (Either SomeError (Maybe Token, Maybe Id)) ()
deleteDraftHandle draftHandle =
    AnswerHandle
        { parseInformation = deleteDraftInformation draftHandle
        , databaseOperation = deleteDraftDatabaseOperation draftHandle
        , sendResult = deleteDraftSendResult
        }

--------------------------------------------------------------------------------------------------------------------
createDraftParseInformation ::
       Monad m
    => DraftsHandle m
    -> Request
    -> m (Either SomeError ( DraftInf
                           , Maybe DraftTags
                           , Maybe Image
                           , Maybe [Image]))
createDraftParseInformation handler request =
    if requestMethod request /= methodPost
        then do
            logError (dhLogger handler) "Bad request method"
            return $ Left BadMethod
        else do
            logInfo (dhLogger handler) "Preparing data for creating draft"
            (i, f) <- dhParseRequestBody handler request
            let draftInf = toDraftInf request i
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
                    logError (dhLogger handler) "Bad image file"
                    return $ Left $ OtherError "Bad image file"
                else return $
                     Right (draftInf, listOfTags, mainImage, imagesList)

createDraftDatabaseOperation ::
       Monad m
    => DraftsHandle m
    -> Either SomeError (DraftInf, Maybe DraftTags, Maybe Image, Maybe [Image])
    -> m (Either SomeError SendId)
createDraftDatabaseOperation _ (Left message) = return $ Left message
createDraftDatabaseOperation draftHandle (Right (draftInfUpdate, listOfTags, mainImage, imagesList)) =
    dhCreateDraftOnDb draftHandle draftInfUpdate listOfTags mainImage imagesList

createDraftSendResult ::
       Monad m
    => Either SomeError SendId
    -> m (Either ResponseErrorMessage ResponseOkMessage)
createDraftSendResult result =
    case result of
        Left someError ->
            return $
            Left $ toResponseErrorMessage "Draft not created." someError
        Right n -> return $ Right $ Created $ LBS.fromStrict $ BC.pack $ show n

createDraftHandle ::
       Monad m
    => DraftsHandle m
    -> AnswerHandle m (Either SomeError ( DraftInf
                                        , Maybe DraftTags
                                        , Maybe Image
                                        , Maybe [Image])) SendId
createDraftHandle draftHandle =
    AnswerHandle
        { parseInformation = createDraftParseInformation draftHandle
        , databaseOperation = createDraftDatabaseOperation draftHandle
        , sendResult = createDraftSendResult
        }

--------------------------------------------------------------------------------------------------------------------
postNewsParseInformation ::
       Monad m
    => DraftsHandle m
    -> Request
    -> m (Either SomeError (Maybe Token, Id))
postNewsParseInformation handler request =
    if requestMethod request /= methodPut
        then do
            logError (dhLogger handler) "Bad request method"
            return $ Left BadMethod
        else do
            logInfo (dhLogger handler) "Preparing data for public news"
            let token = takeToken request
            let draftId = fromJust $ readByteStringToId $ last pathElems
            return $ Right (token, draftId)
  where
    path = BC.tail $ rawPathInfo request
    pathElems = BC.split '/' path

postNewsDatabaseOperation ::
       Monad m
    => DraftsHandle m
    -> Either SomeError (Maybe Token, Id)
    -> m (Either SomeError SendId)
postNewsDatabaseOperation _ (Left message) = return $ Left message
postNewsDatabaseOperation draftHandle (Right (token, draftId)) =
    dhPublicNewsOnDb draftHandle token draftId

postNewsSendResult ::
       Monad m
    => Either SomeError SendId
    -> m (Either ResponseErrorMessage ResponseOkMessage)
postNewsSendResult result =
    case result of
        Left someError ->
            return $ Left $ toResponseErrorMessage "News not created." someError
        Right n -> do
            return $ Right $ Created $ LBS.fromStrict $ BC.pack $ show n

postNewsHandle ::
       Monad m
    => DraftsHandle m
    -> AnswerHandle m (Either SomeError (Maybe Token, Id)) SendId
postNewsHandle draftHandle =
    AnswerHandle
        { parseInformation = postNewsParseInformation draftHandle
        , databaseOperation = postNewsDatabaseOperation draftHandle
        , sendResult = postNewsSendResult
        }
--------------------------------------------------------------------------------------------------------------------
