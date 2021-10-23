{-# LANGUAGE OverloadedStrings #-}

module Controllers.Users where

import           Control.Monad.IO.Class    (MonadIO (..))
import           Data.Aeson                (encode)
import qualified Data.ByteString.Lazy      as LBS
import           Data.Maybe                (fromMaybe)
import qualified Data.Text.Encoding        as E
import           FromRequest               (takeToken, toCreateUser, toLogin,
                                            toPassword)
import           HelpFunction              (foundParametr)
import           Logger                    (Handle, logError, logInfo)
import           Network.HTTP.Types.Method (methodDelete, methodGet, methodPost)
import           Network.Wai               (Request (queryString, requestMethod),
                                            Response)
import           Network.Wai.Parse         (lbsBackEnd, parseRequestBody)
import           OperationsHandle          (UsersHandle (auth, create_user_in_db, delete_user_from_db, profile_on_db))
import           Responses                 (responseBadRequest, responseCreated,
                                            responseForbidden,
                                            responseMethodNotAllowed,
                                            responseOKJSON, responseOk)
import           Types.Other               (Token (..), TokenLifeTime)
import           Types.Users               (Login (Login))

login :: MonadIO m => Handle m -> UsersHandle m -> Request -> m Response
login hLogger operations req =
    if requestMethod req /= methodGet
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo hLogger "Preparing data for sign in."
            (i, _) <- liftIO $ parseRequestBody lbsBackEnd req
            let login' = toLogin i
            let pass = toPassword i
            check <- auth operations hLogger login' pass
            case check of
                Left _ -> do
                    return $ responseBadRequest "Bad authorization"
                Right tk -> do
                    return $
                        responseOk $
                        LBS.fromStrict $ E.encodeUtf8 $ from_token tk

registration :: MonadIO m => Handle m -> UsersHandle m -> Request -> m Response
registration hLogger operations req =
    if requestMethod req /= methodPost
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo hLogger "Preparing data for registration new user."
            (i, f) <- liftIO $ parseRequestBody lbsBackEnd req
            let avatar = foundParametr "avatar" f
            user_params <- toCreateUser i avatar
            result <- create_user_in_db operations hLogger user_params
            case result of
                Left _ -> do
                    return $ responseBadRequest "User not registered."
                Right tk -> do
                    return $
                        responseCreated $
                        LBS.fromStrict $ E.encodeUtf8 $ from_token tk

deleteUser ::
       MonadIO m
    => Handle m
    -> UsersHandle m
    -> TokenLifeTime
    -> Request
    -> m Response
deleteUser hLogger operations token_lifetime req =
    if requestMethod req /= methodDelete
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo hLogger "Preparing data for deleting user."
            let login' =
                    Login . E.decodeUtf8 <$>
                    fromMaybe Nothing (lookup "login" $ queryString req)
            let token' = takeToken req
            result <-
                delete_user_from_db
                    operations
                    hLogger
                    token_lifetime
                    token'
                    login'
            case result of
                Left "Not admin" -> do
                    logError hLogger "User not deleted. Not admin."
                    return $ responseForbidden "Not admin"
                Left "Bad token" -> do
                    logError hLogger "User not deleted. Bad token."
                    return $ responseForbidden "Bad token"
                Left _ -> do
                    return $ responseBadRequest "User not deleted"
                Right _ -> do
                    return $ responseOk "User deleted"

profile ::
       MonadIO m
    => Handle m
    -> UsersHandle m
    -> TokenLifeTime
    -> Request
    -> m Response
profile hLogger operations token_lifetime req =
    if requestMethod req /= methodGet
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo hLogger "Preparing data for sending user information."
            let token' = takeToken req
            result <- profile_on_db operations hLogger token_lifetime token'
            case result of
                Left "Bad token" -> do
                    return $ responseForbidden "Bad token"
                Left _ -> do
                    return $ responseBadRequest "Information not sended."
                Right pro -> do
                    return $ responseOKJSON $ encode pro
