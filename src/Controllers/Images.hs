{-# LANGUAGE OverloadedStrings #-}

module Controllers.Images where

import           Control.Monad.IO.Class     (MonadIO)
import           Data.Aeson                 (encode)
import qualified Data.ByteString.Char8      as BC
import           Database.PostgreSQL.Simple (Binary (fromBinary))
import           FromRequest                (toPage)
import           HelpFunction               (readByteStringToId)
import           Logger                     (logError, logInfo)
import           Network.HTTP.Types.Method  (methodGet)
import           Network.Wai                (Request (rawPathInfo, requestMethod),
                                             Response)
import           OperationsHandle           (ImagesHandle (get_photo, get_photo_list, photos_logger))
import           Responses                  (badResponse, responseBadRequest,
                                             responseMethodNotAllowed,
                                             responseNotFound, responseOKImage,
                                             responseOKJSON)
import           Types.Images               (ImageB (con_type, image_b))
import           Types.Other                (Id)

sendImagesList :: MonadIO m => ImagesHandle m -> Request -> m Response
sendImagesList operations req = do
    if requestMethod req == methodGet
        then do
            logInfo
                (photos_logger operations)
                "Preparing data for sending images list"
            result <-
                get_photo_list operations (photos_logger operations) pageParam
            case result of
                Left someError ->
                    return $ badResponse "List of images not sended." someError
                Right ia -> do
                    return $ responseOKJSON $ encode ia
        else do
            logError (photos_logger operations) "Bad method request"
            return $ responseMethodNotAllowed "Bad method request"
  where
    pageParam = toPage req

sendImage :: MonadIO m => ImagesHandle m -> Id -> Request -> m Response
sendImage operations imageId req = do
    if requestMethod req == methodGet
        then do
            logInfo
                (photos_logger operations)
                "Preparing data for sending image"
            result <- get_photo operations (photos_logger operations) imageId
            case result of
                Left someError ->
                    return $ badResponse "Image not sended." someError
                Right ib -> do
                    return $
                        responseOKImage (con_type ib) (fromBinary $ image_b ib)
        else do
            logError (photos_logger operations) "Bad method request"
            return $ responseMethodNotAllowed "Bad method request"

imagesRouter :: MonadIO m => ImagesHandle m -> Request -> m Response
imagesRouter operations req
    | pathElemsC == 1 = sendImagesList operations req
    | pathElemsC == 2 =
        case readByteStringToId $ last pathElems of
            Nothing -> return $ responseBadRequest "Bad image id"
            Just n  -> sendImage operations n req
    | otherwise = return $ responseNotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
