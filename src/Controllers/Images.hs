{-# LANGUAGE OverloadedStrings #-}

module Controllers.Images where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BC
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Binary(fromBinary), Connection)
import FromRequest (toPage)
import HelpFunction (readByteStringToInt)
import Logger (Handle, logError, logInfo)
import Network.HTTP.Types.Method (methodGet)
import Network.Wai (Request(rawPathInfo, requestMethod), Response)
import OperationsHandle (ImagesHandle(get_photo, get_photo_list))
import Responses
    ( responseBadRequest
    , responseMethodNotAllowed
    , responseNotFound
    , responseOKImage
    , responseOKJSON
    )
import Types (ImageB(con_type, image_b))

sendImagesList ::
       MonadIO m
    => Handle m
    -> ImagesHandle m
    -> Pool Connection
    -> Request
    -> m Response
sendImagesList hLogger operations pool req = do
    if requestMethod req == methodGet
        then do
            logInfo hLogger "Preparing data for sending images list"
            result <- get_photo_list operations hLogger pool pageParam
            case result of
                Left bs -> do
                    logError hLogger "Images list not sended"
                    return $ responseBadRequest bs
                Right ia -> do
                    logInfo hLogger "Images list sended"
                    return $ responseOKJSON $ encode ia
        else do
            logError hLogger "Bad method request"
            return $ responseMethodNotAllowed "Bad method request"
  where
    pageParam = toPage req

sendImage ::
       MonadIO m
    => Handle m
    -> ImagesHandle m
    -> Pool Connection
    -> Int
    -> Request
    -> m Response
sendImage hLogger operations pool imageId req = do
    if requestMethod req == methodGet
        then do
            logInfo hLogger "Preparing data for sending image"
            result <- get_photo operations hLogger pool imageId
            case result of
                Left bs -> do
                    logError hLogger "Image not sended"
                    return $ responseBadRequest bs
                Right ib -> do
                    logInfo hLogger "Image sended"
                    return $
                        responseOKImage (con_type ib) (fromBinary $ image_b ib)
        else do
            logError hLogger "Bad method request"
            return $ responseMethodNotAllowed "Bad method request"

imagesRouter ::
       MonadIO m
    => Handle m
    -> ImagesHandle m
    -> Pool Connection
    -> Request
    -> m Response
imagesRouter hLogger operations pool req
    | pathElemsC == 1 = sendImagesList hLogger operations pool req
    | pathElemsC == 2 =
        case readByteStringToInt $ last pathElems of
            Nothing -> return $ responseBadRequest "Bad image id"
            Just n -> sendImage hLogger operations pool n req
    | otherwise = return $ responseNotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
