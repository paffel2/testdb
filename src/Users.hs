{-# LANGUAGE OverloadedStrings #-}

module Users where

import Data.Aeson (encode)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Database.PostgreSQL.Simple (Connection)
import Databaseoperations
    ( authentication
    , checkAdmin'
    , createUserInDb
    , deleteUserFromDb
    , profileOnDb
    )
import FromRequest (takeToken)
import Logger (Handle, logError)
import Network.Wai (Request(queryString), Response)
import Network.Wai.Parse
    ( FileInfo(fileContent, fileContentType, fileName)
    , lbsBackEnd
    , parseRequestBody
    )
import Responses (responseBadRequest, responseOKJSON, responseOk)
import Types (TokenLifeTime)

login :: Handle -> Pool Connection -> Request -> IO Response
login hLogger pool req = do
    (i, _) <- parseRequestBody lbsBackEnd req
    let login' = fromMaybe "" (lookup "login" i)
    let pass = fromMaybe "" (lookup "user_password" i)
    check <- authentication hLogger pool login' pass
    case check of
        Left bs -> return $ responseBadRequest bs
        Right bs -> return $ responseOk bs

registration :: Handle -> Pool Connection -> Request -> IO Response
registration hLogger pool req = do
    (i, f) <- parseRequestBody lbsBackEnd req
    let [(_, file)] = f
    let f_name = E.decodeUtf8 <$> lookup "f_name" i
    let l_name = E.decodeUtf8 <$> lookup "l_name" i
    let login' = E.decodeUtf8 <$> lookup "login" i
    let password = E.decodeUtf8 <$> lookup "password" i
    if T.length (fromMaybe "" f_name) > 50 ||
       T.length (fromMaybe "" f_name) > 50 ||
       T.length (fromMaybe "" login') > 50 ||
       T.length (fromMaybe "" password) > 50
        then do
            logError hLogger "Long parametr for registration"
            return $
                responseBadRequest "one or more parameter more then 50 symbols"
        else do
            result <-
                createUserInDb
                    hLogger
                    pool
                    login'
                    password
                    f_name
                    l_name
                    (fileName file)
                    (fileContentType file)
                    (fileContent file)
            case result of
                Left bs -> return $ responseBadRequest bs
                Right bs -> return $ responseOk bs

deleteUser ::
       Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
deleteUser hLogger pool token_lifetime req = do
    let login' = fromMaybe Nothing (lookup "login" $ queryString req)
    let token' = E.decodeUtf8 <$> takeToken req
    isAdmin <- checkAdmin' hLogger pool token_lifetime token'
    case isAdmin of
        (False, bs) -> return $ responseBadRequest bs
        (True, _) -> do
            result <- deleteUserFromDb hLogger $ fromMaybe "" login'
            case result of
                Left bs' -> return $ responseBadRequest bs'
                Right bs' -> return $ responseOk bs'

profile :: Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
profile hLogger pool token_lifetime req = do
    let token' = E.decodeUtf8 <$> takeToken req
    result <- profileOnDb hLogger pool token_lifetime token'
    case result of
        Left bs -> return $ responseBadRequest bs
        Right pro -> return $ responseOKJSON $ encode pro
