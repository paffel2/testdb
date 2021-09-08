{-# LANGUAGE OverloadedStrings #-}

module Controllers.Users where

import Data.Aeson (encode)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import qualified Data.Text.Encoding as E
import Database.PostgreSQL.Simple (Connection)
import Databaseoperations.Users
    ( authentication
    , createUserInDb
    , deleteUserFromDb
    , profileOnDb
    )
import FromRequest (takeToken)
import Logger (Handle, logError, logInfo)
import Network.HTTP.Types.Method (methodDelete, methodGet, methodPost)
import Network.Wai (Request(queryString, requestMethod), Response)
import Network.Wai.Parse
    ( FileInfo(fileContent, fileContentType, fileName)
    , lbsBackEnd
    , parseRequestBody
    )
import Responses
    ( responseBadRequest
    , responseCreated
    , responseForbidden
    , responseMethodNotAllowed
    , responseOKJSON
    , responseOk
    )
import Types (TokenLifeTime)

login :: Handle -> Pool Connection -> Request -> IO Response
login hLogger pool req =
    if requestMethod req /= methodGet
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo hLogger "Preparing data for sign in."
            (i, _) <- parseRequestBody lbsBackEnd req
            let login' = E.decodeUtf8 $ fromMaybe "" (lookup "login" i)
            let pass = E.decodeUtf8 $ fromMaybe "" (lookup "user_password" i)
            check <- authentication hLogger pool login' pass
            case check of
                Left bs -> do
                    logError hLogger "User not logged."
                    return $ responseBadRequest bs
                Right bs -> do
                    logInfo hLogger "User logged."
                    return $ responseOk bs

registration :: Handle -> Pool Connection -> Request -> IO Response
registration hLogger pool req =
    if requestMethod req /= methodPost
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo hLogger "Preparing data for registration new user."
            (i, f) <- parseRequestBody lbsBackEnd req
            let [(_, file)] = f
            let f_name = E.decodeUtf8 <$> lookup "f_name" i
            let l_name = E.decodeUtf8 <$> lookup "l_name" i
            let login' = E.decodeUtf8 <$> lookup "login" i
            let password = E.decodeUtf8 <$> lookup "password" i
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
                Left bs -> do
                    logError hLogger "User not registered."
                    return $ responseBadRequest bs
                Right bs -> do
                    logInfo hLogger "User registered."
                    return $ responseCreated bs

deleteUser ::
       Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
deleteUser hLogger pool token_lifetime req =
    if requestMethod req /= methodDelete
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo hLogger "Preparing data for deleting user."
            let login' = fromMaybe Nothing (lookup "login" $ queryString req)
            let token' = E.decodeUtf8 <$> takeToken req
            result <-
                deleteUserFromDb hLogger pool token_lifetime token' $
                fromMaybe "" login'
            case result of
                Left "Not admin" -> do
                    logError hLogger "User not deleted. Not admin."
                    return $ responseForbidden "Not admin"
                Left "Bad token" -> do
                    logError hLogger "User not deleted. Bad token."
                    return $ responseForbidden "Bad token"
                Left bs' -> do
                    logError hLogger "User not deleted."
                    return $ responseBadRequest bs'
                Right bs' -> do
                    logInfo hLogger "User deleted."
                    return $ responseOk bs'

profile :: Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
profile hLogger pool token_lifetime req =
    if requestMethod req /= methodGet
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo hLogger "Preparing data for sending user information."
            let token' = E.decodeUtf8 <$> takeToken req
            result <- profileOnDb hLogger pool token_lifetime token'
            case result of
                Left "Bad token" -> do
                    logError hLogger "Information not sended. Bad token."
                    return $ responseForbidden "Bad token"
                Left bs -> do
                    logError hLogger "Information not sended."
                    return $ responseBadRequest bs
                Right pro -> do
                    logInfo hLogger "Information sended."
                    return $ responseOKJSON $ encode pro
