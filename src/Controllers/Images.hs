{-# LANGUAGE OverloadedStrings #-}

module Controllers.Images where

import           Control.Monad.IO.Class    (MonadIO)
import           Data.Aeson                (encode)
import qualified Data.ByteString.Char8     as BC
import           FromRequest               (toPage)
import           HelpFunction              (readByteStringToId)
import           Logger                    (logError, logInfo)
import           Network.HTTP.Types.Method (methodGet)
import           Network.Wai               (Request (rawPathInfo, requestMethod))
import           OperationsHandle          (ImagesHandle (get_photo, get_photo_list, photos_logger))
import           Responses                 (toResponseErrorMessage)
import           Types.Other               (Id,
                                            ResponseErrorMessage (BadRequest, MethodNotAllowed, NotFound),
                                            ResponseOkMessage (OkImage, OkJSON))

getImagesList ::
       MonadIO m
    => ImagesHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
getImagesList operations req = do
    if requestMethod req == methodGet
        then do
            logInfo
                (photos_logger operations)
                "Preparing data for sending images list"
            result <- get_photo_list operations pageParam
            case result of
                Left someError ->
                    return $
                    Left $
                    toResponseErrorMessage
                        "List of images not sended."
                        someError
                Right ia -> do
                    return $ Right $ OkJSON $ encode ia
        else do
            logError (photos_logger operations) "Bad method request"
            return $ Left $ MethodNotAllowed "Bad request method"
  where
    pageParam = toPage req

getImage ::
       MonadIO m
    => ImagesHandle m
    -> Id
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
getImage operations imageId req = do
    if requestMethod req == methodGet
        then do
            logInfo
                (photos_logger operations)
                "Preparing data for sending image"
            result <- get_photo operations imageId
            case result of
                Left someError ->
                    return $
                    Left $ toResponseErrorMessage "Image not sended." someError
                Right ib -> do
                    return $ Right $ OkImage ib
        else do
            logError (photos_logger operations) "Bad method request"
            return $ Left $ MethodNotAllowed "Bad request method"

imagesRouter ::
       MonadIO m
    => ImagesHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
imagesRouter operations req
    | pathElemsC == 1 = getImagesList operations req
    | pathElemsC == 2 =
        case readByteStringToId $ last pathElems of
            Nothing -> return $ Left $ BadRequest "Bad image id"
            Just n  -> getImage operations n req
    | otherwise = return $ Left $ NotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
