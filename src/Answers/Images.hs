{-# LANGUAGE OverloadedStrings #-}

module Answers.Images where

import           Answer                    (AnswerHandle' (..))
import           Control.Monad.Except      (ExceptT, MonadError (throwError),
                                            MonadIO (liftIO), runExceptT)
import           Data.Aeson                (encode)
import qualified Data.ByteString.Char8     as BC
import           FromRequest               (toPage)
import           HelpFunction              (readByteStringToId)
import           Logger                    (logError)
import           Network.HTTP.Types.Method (methodGet)
import           Network.Wai               (Request (rawPathInfo, requestMethod))
import           OperationsHandle          (ImagesHandle (ihGetPhoto, ihGetPhotoList, ihLogger))
import           Responses                 (toResponseErrorMessage)
import           Types.Images              (ImageArray, ImageB)
import           Types.Other               (Id, MonadWithError, Page,
                                            ResponseErrorMessage,
                                            ResponseOkMessage (OkImage, OkJSON),
                                            SomeError (BadMethod, OtherError))

----------------------------------------------------------------------------
getImageParseInformation ::
       ImagesHandle MonadWithError IO -> Request -> MonadWithError Id
getImageParseInformation handler request = do
    if requestMethod request /= methodGet
        then do
            liftIO $ logError (ihLogger handler) "Bad request method"
            throwError BadMethod
        else do
            case readByteStringToId $ last pathElems of
                Nothing -> throwError $ OtherError "Bad image id"
                Just n  -> return n
  where
    path = BC.tail $ rawPathInfo request
    pathElems = BC.split '/' path

getImageDatabaseOperation ::
       ImagesHandle MonadWithError IO -> Id -> MonadWithError ImageB
getImageDatabaseOperation = ihGetPhoto

getImageSendResult ::
       MonadWithError ImageB
    -> IO (Either ResponseErrorMessage ResponseOkMessage)
getImageSendResult result = do
    a <- runExceptT result
    case a of
        Left someError ->
            return $ Left $ toResponseErrorMessage "Image not sended." someError
        Right image -> do
            return $ Right $ OkImage image

getImageHandle ::
       ImagesHandle MonadWithError IO
    -> AnswerHandle' (ExceptT SomeError IO) Id ImageB IO
getImageHandle imagesHandle =
    AnswerHandle'
        { parseInformation' = getImageParseInformation imagesHandle
        , databaseOperation' = getImageDatabaseOperation imagesHandle
        , sendResult' = getImageSendResult
        }

------------------------------------------------------------------------------------
imagesListParseInformation ::
       ImagesHandle MonadWithError IO -> Request -> MonadWithError (Maybe Page)
imagesListParseInformation handler request =
    if requestMethod request /= methodGet
        then do
            liftIO $ logError (ihLogger handler) "Bad request method"
            throwError BadMethod
        else do
            return $ toPage request

imagesListDatabaseOperation ::
       ImagesHandle MonadWithError IO -> Maybe Page -> MonadWithError ImageArray
imagesListDatabaseOperation = ihGetPhotoList

imagesListSendResult ::
       MonadWithError ImageArray
    -> IO (Either ResponseErrorMessage ResponseOkMessage)
imagesListSendResult result = do
    a <- runExceptT result
    case a of
        Left someError ->
            return $
            Left $ toResponseErrorMessage "List of images not sended." someError
        Right imagesArray -> do
            return $ Right $ OkJSON $ encode imagesArray

getImagesListHandle ::
       ImagesHandle MonadWithError IO
    -> AnswerHandle' (ExceptT SomeError IO) (Maybe Page) ImageArray IO
getImagesListHandle imagesHandle =
    AnswerHandle'
        { parseInformation' = imagesListParseInformation imagesHandle
        , databaseOperation' = imagesListDatabaseOperation imagesHandle
        , sendResult' = imagesListSendResult
        }
