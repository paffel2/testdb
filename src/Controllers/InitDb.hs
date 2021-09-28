{-# LANGUAGE OverloadedStrings #-}

module Controllers.InitDb where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Char8 as BC
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Databaseoperations.InitDb (createDb)
import Logger (Handle, logError, logInfo)
import Network.Wai (Request(rawPathInfo), Response)
import OperationsHandle (InitDbHandle(create_db))
import Responses (responseBadRequest, responseCreated, responseNotFound)
import Types (DatabaseAddress)

initDb ::
       MonadIO m
    => Handle m
    -> InitDbHandle m
    -> Pool Connection
    -> DatabaseAddress
    -> Request
    -> m Response
initDb hLogger operations pool db_server_addres req =
    if pathElemsC == 1
        then do
            logInfo hLogger "Preparing data creating database"
            result <- create_db operations hLogger pool db_server_addres
            case result of
                Left bs -> do
                    logError hLogger "Database not created"
                    return $ responseBadRequest bs
                Right bs -> do
                    logInfo hLogger "Database created"
                    return $ responseCreated bs
        else do
            logError hLogger "Bad method request"
            return $ responseNotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
