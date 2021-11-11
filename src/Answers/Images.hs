{-# LANGUAGE OverloadedStrings #-}

module Answers.Images where

import           Answer                    (AnswerHandle (..))
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
import           Types.Other               (Id, Page, ResponseErrorMessage,
                                            ResponseOkMessage (OkImage, OkJSON),
                                            SomeError (BadMethod, OtherError))

imagesListParseInformation ::
       Monad m => ImagesHandle m -> Request -> m (Either SomeError (Maybe Page))
imagesListParseInformation handler request =
    if requestMethod request /= methodGet
        then do
            logError (ihLogger handler) "Bad request method"
            return $ Left BadMethod
        else do
            return $ Right $ toPage request

imagesListDatabaseOperation ::
       Monad m
    => ImagesHandle m
    -> Either SomeError (Maybe Page)
    -> m (Either SomeError ImageArray)
imagesListDatabaseOperation _ (Left message) = return $ Left message
imagesListDatabaseOperation imagesHandler (Right pageParam) =
    ihGetPhotoList imagesHandler pageParam

imagesListSendResult ::
       Monad m
    => Either SomeError ImageArray
    -> m (Either ResponseErrorMessage ResponseOkMessage)
imagesListSendResult result =
    case result of
        Left someError ->
            return $
            Left $ toResponseErrorMessage "List of images not sended." someError
        Right imagesArray -> do
            return $ Right $ OkJSON $ encode imagesArray

getImagesListHandle ::
       Monad m
    => ImagesHandle m
    -> AnswerHandle m (Either SomeError (Maybe Page)) ImageArray
getImagesListHandle imagesHandle =
    AnswerHandle
        { parseInformation = imagesListParseInformation imagesHandle
        , databaseOperation = imagesListDatabaseOperation imagesHandle
        , sendResult = imagesListSendResult
        }

getImageParseInformation ::
       Monad m => ImagesHandle m -> Request -> m (Either SomeError Id)
getImageParseInformation handler request = do
    if requestMethod request /= methodGet
        then do
            logError (ihLogger handler) "Bad request method"
            return $ Left BadMethod
        else do
            case readByteStringToId $ last pathElems of
                Nothing -> return $ Left $ OtherError "Bad image id"
                Just n  -> return $ Right n
  where
    path = BC.tail $ rawPathInfo request
    pathElems = BC.split '/' path

getImageDatabaseOperation ::
       Monad m
    => ImagesHandle m
    -> Either SomeError Id
    -> m (Either SomeError ImageB)
getImageDatabaseOperation _ (Left message)        = return $ Left message
getImageDatabaseOperation imagesHandler (Right n) = ihGetPhoto imagesHandler n

getImageSendResult ::
       Monad m
    => Either SomeError ImageB
    -> m (Either ResponseErrorMessage ResponseOkMessage)
getImageSendResult result =
    case result of
        Left someError ->
            return $ Left $ toResponseErrorMessage "Image not sended." someError
        Right image -> do
            return $ Right $ OkImage image

getImageHandle ::
       Monad m => ImagesHandle m -> AnswerHandle m (Either SomeError Id) ImageB
getImageHandle imagesHandle =
    AnswerHandle
        { parseInformation = getImageParseInformation imagesHandle
        , databaseOperation = getImageDatabaseOperation imagesHandle
        , sendResult = getImageSendResult
        }
