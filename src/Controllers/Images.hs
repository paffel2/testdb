{-# LANGUAGE OverloadedStrings #-}

module Controllers.Images where

import           Data.Aeson                (encode)
import qualified Data.ByteString.Char8     as BC
import           FromRequest               (toPage)
import           HelpFunction              (readByteStringToId)
import           Logger                    (logError, logInfo)
import           Network.HTTP.Types.Method (methodGet)
import           Network.Wai               (Request (rawPathInfo, requestMethod))
import           OperationsHandle          (ImagesHandle (ihGetPhoto, ihGetPhotoList, ihLogger))
import           Responses                 (toResponseErrorMessage)
import           Types.Other               (Id,
                                            ResponseErrorMessage (BadRequest, MethodNotAllowed, NotFound),
                                            ResponseOkMessage (OkImage, OkJSON))

getImagesList ::
       Monad m
    => ImagesHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
getImagesList operations req = do
    if requestMethod req == methodGet
        then do
            logInfo
                (ihLogger operations)
                "Preparing data for sending images list"
            result <- ihGetPhotoList operations pageParam
            case result of
                Left someError ->
                    return $
                    Left $
                    toResponseErrorMessage
                        "List of images not sended."
                        someError
                Right imagesArray -> do
                    return $ Right $ OkJSON $ encode imagesArray
        else do
            logError (ihLogger operations) "Bad method request"
            return $ Left $ MethodNotAllowed "Bad request method"
  where
    pageParam = toPage req

getImage ::
       Monad m
    => ImagesHandle m
    -> Id
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
getImage operations imageId req = do
    if requestMethod req == methodGet
        then do
            logInfo (ihLogger operations) "Preparing data for sending image"
            result <- ihGetPhoto operations imageId
            case result of
                Left someError ->
                    return $
                    Left $ toResponseErrorMessage "Image not sended." someError
                Right image -> do
                    return $ Right $ OkImage image
        else do
            logError (ihLogger operations) "Bad method request"
            return $ Left $ MethodNotAllowed "Bad request method"

imagesRouter ::
       Monad m
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
