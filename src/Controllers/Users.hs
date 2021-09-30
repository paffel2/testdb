{-# LANGUAGE OverloadedStrings #-}

module Controllers.Users where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (encode)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import qualified Data.Text.Encoding as E
import Database.PostgreSQL.Simple (Connection)
import FromRequest (takeToken)
import Logger (Handle, logError, logInfo)
import Network.HTTP.Types.Method (methodDelete, methodGet, methodPost)
import Network.Wai (Request(queryString, requestMethod), Response)
import Network.Wai.Parse
    ( FileInfo(fileContent, fileContentType, fileName)
    , lbsBackEnd
    , parseRequestBody
    )
import OperationsHandle
    ( UsersHandle(auth, create_user_in_db, delete_user_from_db,
            profile_on_db)
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

login ::
       MonadIO m
    => Handle m
    -> UsersHandle m
    -> Pool Connection
    -> Request
    -> m Response
login hLogger operations pool req =
    if requestMethod req /= methodGet
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo hLogger "Preparing data for sign in."
            (i, _) <- liftIO $ parseRequestBody lbsBackEnd req
            let login' = E.decodeUtf8 $ fromMaybe "" (lookup "login" i)
            let pass = E.decodeUtf8 $ fromMaybe "" (lookup "user_password" i)
            check <- auth operations hLogger pool login' pass
            case check of
                Left bs -> do
                    logError hLogger "User not logged."
                    return $ responseBadRequest bs
                Right bs -> do
                    logInfo hLogger "User logged."
                    return $ responseOk bs

registration ::
       MonadIO m
    => Handle m
    -> UsersHandle m
    -> Pool Connection
    -> Request
    -> m Response
registration hLogger operations pool req =
    if requestMethod req /= methodPost
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo hLogger "Preparing data for registration new user."
            (i, f) <- liftIO $ parseRequestBody lbsBackEnd req
            let [(_, file)] = f
            let f_name = E.decodeUtf8 <$> lookup "f_name" i
            let l_name = E.decodeUtf8 <$> lookup "l_name" i
            let login' = E.decodeUtf8 <$> lookup "login" i
            let password = E.decodeUtf8 <$> lookup "password" i
            result <-
                create_user_in_db
                    operations
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
       MonadIO m
    => Handle m
    -> UsersHandle m
    -> Pool Connection
    -> TokenLifeTime
    -> Request
    -> m Response
deleteUser hLogger operations pool token_lifetime req =
    if requestMethod req /= methodDelete
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo hLogger "Preparing data for deleting user."
            let login' = fromMaybe Nothing (lookup "login" $ queryString req)
            let token' = takeToken req
            result <-
                delete_user_from_db
                    operations
                    hLogger
                    pool
                    token_lifetime
                    token' $
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

profile ::
       MonadIO m
    => Handle m
    -> UsersHandle m
    -> Pool Connection
    -> TokenLifeTime
    -> Request
    -> m Response
profile hLogger operations pool token_lifetime req =
    if requestMethod req /= methodGet
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo hLogger "Preparing data for sending user information."
            let token' = takeToken req
            result <-
                profile_on_db operations hLogger pool token_lifetime token'
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
