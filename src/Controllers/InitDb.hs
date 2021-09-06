{-# LANGUAGE OverloadedStrings #-}

module Controllers.InitDb where

import qualified Data.ByteString.Char8 as BC
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Databaseoperations.InitDb (createDb)
import Logger (Handle)
import Network.Wai (Request(rawPathInfo), Response)
import Responses (responseBadRequest, responseCreated, responseNotFound)
import Types (DatabaseAddress)

initDbBlock ::
       Handle -> Pool Connection -> DatabaseAddress -> Request -> IO Response
initDbBlock hLogger pool db_server_addres req =
    if pathElemsC == 1
        then do
            result <- createDb hLogger pool db_server_addres
            case result of
                Left bs -> return $ responseBadRequest bs
                Right bs -> return $ responseCreated bs
        else return $ responseNotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
