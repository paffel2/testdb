{-# LANGUAGE OverloadedStrings #-}

module Controllers.Users where

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
import           OperationsHandle          (UsersHandle (uhAuth, uhCreateUserInDb, uhDeleteUserFromDb, uhLogger, uhParseRequestBody, uhProfileOnDb))
import           Responses                 (toResponseErrorMessage)
import           Types.Other               (ResponseErrorMessage (MethodNotAllowed),
                                            ResponseOkMessage (Created, OkJSON, OkMessage),
                                            Token (getToken))
import           Types.Users               (Login (Login))

signIn ::
       Monad m
    => UsersHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
signIn operations req =
    if requestMethod req /= methodGet
        then do
            logError (uhLogger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo (uhLogger operations) "Preparing data for sign in."
            (i, _) <- uhParseRequestBody operations req
            let login = toLogin i
            let pass = toPassword i
            check <- uhAuth operations login pass
            case check of
                Left someError ->
                    return $
                    Left $ toResponseErrorMessage "Bad authorization." someError
                Right tk -> do
                    return $
                        Right $
                        OkMessage $ LBS.fromStrict $ E.encodeUtf8 $ getToken tk

registration ::
       Monad m
    => UsersHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
registration operations req =
    if requestMethod req /= methodPost
        then do
            logError (uhLogger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo
                (uhLogger operations)
                "Preparing data for registration new user."
            (i, f) <- uhParseRequestBody operations req
            let avatar = foundParametr "avatar" f
            let userParams = toCreateUser i avatar
            result <- uhCreateUserInDb operations userParams
            case result of
                Left someError ->
                    return $
                    Left $
                    toResponseErrorMessage "User not registered." someError
                Right tk -> do
                    return $
                        Right $
                        Created $ LBS.fromStrict $ E.encodeUtf8 $ getToken tk

deleteUser ::
       Monad m
    => UsersHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
deleteUser operations req =
    if requestMethod req /= methodDelete
        then do
            logError (uhLogger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo (uhLogger operations) "Preparing data for deleting user."
            let login =
                    Login . E.decodeUtf8 <$>
                    fromMaybe Nothing (lookup "login" $ queryString req)
            let token = takeToken req
            result <- uhDeleteUserFromDb operations token login
            case result of
                Left someError ->
                    return $
                    Left $ toResponseErrorMessage "User not deleted." someError
                Right _ -> do
                    return $ Right $ OkMessage "User deleted."

profile ::
       Monad m
    => UsersHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
profile operations req =
    if requestMethod req /= methodGet
        then do
            logError (uhLogger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo
                (uhLogger operations)
                "Preparing data for sending user information."
            let token = takeToken req
            result <- uhProfileOnDb operations token
            case result of
                Left someError ->
                    return $
                    Left $
                    toResponseErrorMessage
                        "Profile inforamtion not sended."
                        someError
                Right pro -> do
                    return $ Right $ OkJSON $ encode pro
