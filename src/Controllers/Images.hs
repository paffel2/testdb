{-# LANGUAGE OverloadedStrings #-}

module Controllers.Images where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Binary(fromBinary), Connection)
import Databaseoperations.Images (getPhoto, getPhotoList)
import HelpFunction (readByteStringToInt)
import Logger (Handle)
import Network.HTTP.Types.Method (methodGet)
import Network.Wai (Request(queryString, rawPathInfo, requestMethod), Response)
import Responses (responseBadRequest, responseOKImage, responseOKJSON)
import Types (ImageB(con_type, image_b))

sendImagesList :: Handle -> Pool Connection -> Request -> IO Response
sendImagesList hLogger pool req = do
    if requestMethod req == methodGet
        then do
            result <- getPhotoList hLogger pool pageParam
            case result of
                Left bs -> return $ responseBadRequest bs
                Right ia -> return $ responseOKJSON $ encode ia
        else return $ responseBadRequest "Bad method request"
  where
    queryParams = queryString req
    pageParam = fromMaybe Nothing (lookup "page" queryParams)

sendImage :: Handle -> Pool Connection -> Int -> Request -> IO Response
sendImage hLogger pool imageId req = do
    if requestMethod req == methodGet
        then do
            result <- getPhoto hLogger pool imageId
            case result of
                Left bs -> return $ responseBadRequest bs
                Right ib ->
                    return $
                    responseOKImage (con_type ib) (fromBinary $ image_b ib)
        else return $ responseBadRequest "Bad method request"

imageBlock :: Handle -> Pool Connection -> Request -> IO Response
imageBlock hLogger pool req
    | pathElemsC == 1 = sendImagesList hLogger pool req
    | pathElemsC == 2 =
        case readByteStringToInt $ last pathElems of
            Nothing -> return $ responseBadRequest "Bad image if"
            Just n -> sendImage hLogger pool n req
    | otherwise = return $ responseBadRequest "bad request"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
