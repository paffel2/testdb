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
import           Logger                    (logError, logInfo)
import           Network.HTTP.Types.Method (methodDelete, methodGet, methodPost)
import           Network.Wai               (Request (queryString, requestMethod))
import           Network.Wai.Parse         (lbsBackEnd, parseRequestBody)
import           OperationsHandle          (UsersHandle (auth, create_user_in_db, delete_user_from_db, profile_on_db, users_logger))
import           Responses                 (toResponseErrorMessage)
import           Types.Other               (ResponseErrorMessage (MethodNotAllowed),
                                            ResponseOkMessage (Created, OkJSON, OkMessage),
                                            Token (from_token))
import           Types.Users               (Login (Login))

login ::
       MonadIO m
    => UsersHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
login operations req =
    if requestMethod req /= methodGet
        then do
            logError (users_logger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo (users_logger operations) "Preparing data for sign in."
            (i, _) <- liftIO $ parseRequestBody lbsBackEnd req
            let login' = toLogin i
            let pass = toPassword i
            check <- auth operations login' pass
            case check of
                Left someError ->
                    return $
                    Left $ toResponseErrorMessage "Bad authorization." someError
                Right tk -> do
                    return $
                        Right $
                        OkMessage $
                        LBS.fromStrict $ E.encodeUtf8 $ from_token tk

registration ::
       MonadIO m
    => UsersHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
registration operations req =
    if requestMethod req /= methodPost
        then do
            logError (users_logger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo
                (users_logger operations)
                "Preparing data for registration new user."
            (i, f) <- liftIO $ parseRequestBody lbsBackEnd req
            let avatar = foundParametr "avatar" f
            user_params <- toCreateUser i avatar
            result <- create_user_in_db operations user_params
            case result of
                Left someError ->
                    return $
                    Left $
                    toResponseErrorMessage "User not registered." someError
                Right tk -> do
                    return $
                        Right $
                        Created $ LBS.fromStrict $ E.encodeUtf8 $ from_token tk

deleteUser ::
       MonadIO m
    => UsersHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
deleteUser operations req =
    if requestMethod req /= methodDelete
        then do
            logError (users_logger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo
                (users_logger operations)
                "Preparing data for deleting user."
            let login' =
                    Login . E.decodeUtf8 <$>
                    fromMaybe Nothing (lookup "login" $ queryString req)
            let token' = takeToken req
            result <- delete_user_from_db operations token' login'
            case result of
                Left someError ->
                    return $
                    Left $ toResponseErrorMessage "User not deleted." someError
                Right _ -> do
                    return $ Right $ OkMessage "User deleted."

profile ::
       MonadIO m
    => UsersHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
profile operations req =
    if requestMethod req /= methodGet
        then do
            logError (users_logger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo
                (users_logger operations)
                "Preparing data for sending user information."
            let token' = takeToken req
            result <- profile_on_db operations token'
            case result of
                Left someError ->
                    return $
                    Left $
                    toResponseErrorMessage
                        "Profile inforamtion not sended."
                        someError
                Right pro -> do
                    return $ Right $ OkJSON $ encode pro
