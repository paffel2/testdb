{-# LANGUAGE OverloadedStrings #-}

module Images where

import Data.Aeson
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import Data.Pool
import qualified Data.Text.Encoding as E
import Database.PostgreSQL.Simple
import Databaseoperations
import FromRequest
import HelpFunction
import Logger
import Network.HTTP.Types.Method
import Network.Wai
import Network.Wai.Parse
import Responses
import Types

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
