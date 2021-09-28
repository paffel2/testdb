{-# LANGUAGE OverloadedStrings #-}

module Controllers.Images where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Binary(fromBinary), Connection)
import Databaseoperations.Images (getPhoto, getPhotoList)
import HelpFunction (readByteStringToInt)
import Logger (Handle, logError, logInfo)
import Network.HTTP.Types.Method (methodGet)
import Network.Wai (Request(queryString, rawPathInfo, requestMethod), Response)
import Responses
    ( responseBadRequest
    , responseMethodNotAllowed
    , responseNotFound
    , responseOKImage
    , responseOKJSON
    )
import Types (ImageB(con_type, image_b))

sendImagesList :: Handle IO -> Pool Connection -> Request -> IO Response
sendImagesList hLogger pool req = do
    if requestMethod req == methodGet
        then do
            logInfo hLogger "Preparing data for sending images list"
            result <- getPhotoList hLogger pool pageParam
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
    queryParams = queryString req
    pageParam = fromMaybe Nothing (lookup "page" queryParams)

sendImage :: Handle IO -> Pool Connection -> Int -> Request -> IO Response
sendImage hLogger pool imageId req = do
    if requestMethod req == methodGet
        then do
            logInfo hLogger "Preparing data for sending image"
            result <- getPhoto hLogger pool imageId
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

imageBlock :: Handle IO -> Pool Connection -> Request -> IO Response
imageBlock hLogger pool req
    | pathElemsC == 1 = sendImagesList hLogger pool req
    | pathElemsC == 2 =
        case readByteStringToInt $ last pathElems of
            Nothing -> return $ responseBadRequest "Bad image id"
            Just n -> sendImage hLogger pool n req
    | otherwise = return $ responseNotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
