{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Answers.Drafts where

import           Answer                    (AnswerHandle (..))
import           Control.Monad.Except      (MonadError (throwError), MonadIO)
import qualified Data.ByteString.Char8     as BC
import           Data.Maybe                (fromJust, isNothing)
import           FromRequest               (checkNotImageMaybe, checkNotImages,
                                            takeToken, toDraftId, toDraftInf,
                                            toDraftTags, toImage)
import           HelpFunction              (foundParametr, readByteStringToId,
                                            saveHead)
import           Network.HTTP.Types.Method (methodDelete, methodGet, methodPost,
                                            methodPut)
import           Network.Wai               (Request (rawPathInfo, requestMethod))
import           Network.Wai.Parse         (FileInfo (fileContent))
import           OperationsHandle          (DraftsHandle (dhCreateDraftOnDb, dhDeleteDraftFromDb, dhGetDraftByIdFromDb, dhGetDraftsByAuthorToken, dhParseRequestBody, dhPublicNewsOnDb, dhUpdateDraftInDb))
import           Types.Drafts              (Draft, DraftArray, DraftInf,
                                            DraftTags)
import           Types.Images              (Image)
import           Types.Other               (Id, SendId,
                                            SomeError (BadMethod, OtherError),
                                            Token)

--------------------------------------------------------------------------------------------------------------------
getDraftsParseInformation ::
       (MonadIO m, MonadError SomeError m)
    => DraftsHandle m
    -> Request
    -> m (Maybe Token)
getDraftsParseInformation _ request =
    if requestMethod request /= methodGet
        then throwError BadMethod
        else return $ takeToken request

getDraftsDatabaseOperation ::
       (MonadIO m, MonadError SomeError m)
    => DraftsHandle m
    -> Maybe Token
    -> m DraftArray
getDraftsDatabaseOperation = dhGetDraftsByAuthorToken

getDraftsHandle ::
       (MonadIO m, MonadError SomeError m)
    => DraftsHandle m
    -> AnswerHandle m (Maybe Token) DraftArray
getDraftsHandle draftHandle =
    AnswerHandle
        { parseInformation = getDraftsParseInformation draftHandle
        , databaseOperation = getDraftsDatabaseOperation draftHandle
        }

--------------------------------------------------------------------------------------------------------------------
getDraftByIdInformation ::
       (MonadIO m, MonadError SomeError m)
    => DraftsHandle m
    -> Request
    -> m (Maybe Token, Id)
getDraftByIdInformation _ request =
    if requestMethod request /= methodGet
        then throwError BadMethod
        else do
            let token = takeToken request
            let draftId = fromJust $ readByteStringToId $ last pathElems
            return (token, draftId)
  where
    path = BC.tail $ rawPathInfo request
    pathElems = BC.split '/' path

getDraftByIdDatabaseOperation ::
       (MonadIO m, MonadError SomeError m)
    => DraftsHandle m
    -> (Maybe Token, Id)
    -> m Draft
getDraftByIdDatabaseOperation draftHandle (token, draftId) =
    dhGetDraftByIdFromDb draftHandle token draftId

getDraftByIdHandle ::
       (MonadIO m, MonadError SomeError m)
    => DraftsHandle m
    -> AnswerHandle m (Maybe Token, Id) Draft
getDraftByIdHandle draftHandle =
    AnswerHandle
        { parseInformation = getDraftByIdInformation draftHandle
        , databaseOperation = getDraftByIdDatabaseOperation draftHandle
        }

--------------------------------------------------------------------------------------------------------------------
deleteDraftInformation ::
       (MonadIO m, MonadError SomeError m)
    => DraftsHandle m
    -> Request
    -> m (Maybe Token, Maybe Id)
deleteDraftInformation _ request =
    if requestMethod request /= methodDelete
        then throwError BadMethod
        else do
            let token = takeToken request
            let draftId = toDraftId request
            return (token, draftId)

deleteDraftDatabaseOperation ::
       (MonadIO m, MonadError SomeError m)
    => DraftsHandle m
    -> (Maybe Token, Maybe Id)
    -> m ()
deleteDraftDatabaseOperation draftHandle (token, draftId) =
    dhDeleteDraftFromDb draftHandle token draftId

deleteDraftHandle ::
       (MonadIO m, MonadError SomeError m)
    => DraftsHandle m
    -> AnswerHandle m (Maybe Token, Maybe Id) ()
deleteDraftHandle draftHandle =
    AnswerHandle
        { parseInformation = deleteDraftInformation draftHandle
        , databaseOperation = deleteDraftDatabaseOperation draftHandle
        }

--------------------------------------------------------------------------------------------------------------------
postNewsParseInformation ::
       (MonadIO m, MonadError SomeError m)
    => DraftsHandle m
    -> Request
    -> m (Maybe Token, Id)
postNewsParseInformation _ request =
    if requestMethod request /= methodPut
        then throwError BadMethod
        else do
            let token = takeToken request
            let draftId = fromJust $ readByteStringToId $ last pathElems
            return (token, draftId)
  where
    path = BC.tail $ rawPathInfo request
    pathElems = BC.split '/' path

postNewsDatabaseOperation ::
       (MonadIO m, MonadError SomeError m)
    => DraftsHandle m
    -> (Maybe Token, Id)
    -> m SendId
postNewsDatabaseOperation draftHandle (token, draftId) =
    dhPublicNewsOnDb draftHandle token draftId

postNewsHandle ::
       (MonadIO m, MonadError SomeError m)
    => DraftsHandle m
    -> AnswerHandle m (Maybe Token, Id) SendId
postNewsHandle draftHandle =
    AnswerHandle
        { parseInformation = postNewsParseInformation draftHandle
        , databaseOperation = postNewsDatabaseOperation draftHandle
        }

--------------------------------------------------------------------------------------------------------------------
createDraftParseInformation ::
       (MonadIO m, MonadError SomeError m)
    => DraftsHandle m
    -> Request
    -> m (DraftInf, Maybe DraftTags, Maybe Image, Maybe [Image])
createDraftParseInformation handler request =
    if requestMethod request /= methodPost
        then throwError BadMethod
        else do
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
                then throwError $ OtherError "Bad image file"
                else return (draftInf, listOfTags, mainImage, imagesList)

createDraftDatabaseOperation ::
       (MonadIO m, MonadError SomeError m)
    => DraftsHandle m
    -> (DraftInf, Maybe DraftTags, Maybe Image, Maybe [Image])
    -> m SendId
createDraftDatabaseOperation draftHandle (draftInfUpdate, listOfTags, mainImage, imagesList) =
    dhCreateDraftOnDb draftHandle draftInfUpdate listOfTags mainImage imagesList

createDraftHandle ::
       (MonadIO m, MonadError SomeError m)
    => DraftsHandle m
    -> AnswerHandle m (DraftInf, Maybe DraftTags, Maybe Image, Maybe [Image]) SendId
createDraftHandle draftHandle =
    AnswerHandle
        { parseInformation = createDraftParseInformation draftHandle
        , databaseOperation = createDraftDatabaseOperation draftHandle
        }

--------------------------------------------------------------------------------------------
updateDraftParseInformation ::
       (MonadIO m, MonadError SomeError m)
    => DraftsHandle m
    -> Request
    -> m (DraftInf, Maybe DraftTags, Maybe Image, Maybe [Image], Id)
updateDraftParseInformation handler request =
    if requestMethod request /= methodPut
        then throwError BadMethod
        else do
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
                then throwError $ OtherError "Bad image file"
                else return
                         ( draftInfUpdate
                         , listOfTags
                         , mainImage
                         , imagesList
                         , draftId)
  where
    path = BC.tail $ rawPathInfo request
    pathElems = BC.split '/' path

updateDraftDatabaseOperation ::
       (MonadIO m, MonadError SomeError m)
    => DraftsHandle m
    -> (DraftInf, Maybe DraftTags, Maybe Image, Maybe [Image], Id)
    -> m ()
updateDraftDatabaseOperation draftHandle (draftInfUpdate, listOfTags, mainImage, imagesList, draftId) =
    dhUpdateDraftInDb
        draftHandle
        draftInfUpdate
        listOfTags
        mainImage
        imagesList
        draftId

updateDraftHandle ::
       (MonadIO m, MonadError SomeError m)
    => DraftsHandle m
    -> AnswerHandle m ( DraftInf
                      , Maybe DraftTags
                      , Maybe Image
                      , Maybe [Image]
                      , Id) ()
updateDraftHandle draftHandle =
    AnswerHandle
        { parseInformation = updateDraftParseInformation draftHandle
        , databaseOperation = updateDraftDatabaseOperation draftHandle
        }
