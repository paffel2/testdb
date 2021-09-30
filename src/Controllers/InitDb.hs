{-# LANGUAGE OverloadedStrings #-}

module Controllers.InitDb where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Char8 as BC
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Logger (Handle, logError, logInfo)
import Network.Wai (Request(rawPathInfo), Response)
import OperationsHandle (InitDbHandle(create_db))
import Responses (responseBadRequest, responseCreated, responseNotFound)

initDb ::
       MonadIO m
    => Handle m
    -> InitDbHandle m
    -> Pool Connection
    -> Request
    -> m Response
initDb hLogger operations pool req =
    if pathElemsC == 1
        then do
            logInfo hLogger "Preparing data creating database"
            result <- create_db operations hLogger pool
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
