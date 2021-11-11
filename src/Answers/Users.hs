{-# LANGUAGE OverloadedStrings #-}

module Answers.Users where

import           Answer                    (AnswerHandle (..))
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
import           Types.Other               (ResponseErrorMessage,
                                            ResponseOkMessage (Created, OkJSON, OkMessage),
                                            SomeError (BadMethod),
                                            Token (getToken))
import           Types.Users               (CreateUser, Login (Login), Password,
                                            Profile)

signInParseInformation ::
       Monad m
    => UsersHandle m
    -> Request
    -> m (Either SomeError (Maybe Login, Maybe Password))
signInParseInformation handler request =
    if requestMethod request /= methodGet
        then do
            logError (uhLogger handler) "Bad request method"
            return $ Left BadMethod
        else do
            logInfo (uhLogger handler) "Preparing data for sign in."
            (i, _) <- uhParseRequestBody handler request
            let login = toLogin i
            let pass = toPassword i
            return $ Right (login, pass)

signInDatabaseOperation ::
       Monad m
    => UsersHandle m
    -> Either SomeError (Maybe Login, Maybe Password)
    -> m (Either SomeError Token)
signInDatabaseOperation _ (Left message) = return $ Left message
signInDatabaseOperation usersHandle (Right (login, pass)) =
    uhAuth usersHandle login pass

signInSendResult ::
       Monad m
    => Either SomeError Token
    -> m (Either ResponseErrorMessage ResponseOkMessage)
signInSendResult result =
    case result of
        Left someError ->
            return $
            Left $ toResponseErrorMessage "Bad authorization." someError
        Right tk -> do
            return $
                Right $ OkMessage $ LBS.fromStrict $ E.encodeUtf8 $ getToken tk

signInHandle ::
       Monad m
    => UsersHandle m
    -> AnswerHandle m (Either SomeError (Maybe Login, Maybe Password)) Token
signInHandle usersHandle =
    AnswerHandle
        { parseInformation = signInParseInformation usersHandle
        , databaseOperation = signInDatabaseOperation usersHandle
        , sendResult = signInSendResult
        }

---------------------------------------------------------------------------------------------------------------------
registrationParseInformation ::
       Monad m => UsersHandle m -> Request -> m (Either SomeError CreateUser)
registrationParseInformation handler request =
    if requestMethod request /= methodPost
        then do
            logError (uhLogger handler) "Bad request method"
            return $ Left BadMethod
        else do
            logInfo
                (uhLogger handler)
                "Preparing data for registration new user."
            (i, f) <- uhParseRequestBody handler request
            let avatar = foundParametr "avatar" f
            let userParams = toCreateUser i avatar
            return $ Right userParams

registrationDatabaseOperation ::
       Monad m
    => UsersHandle m
    -> Either SomeError CreateUser
    -> m (Either SomeError Token)
registrationDatabaseOperation _ (Left message) = return $ Left message
registrationDatabaseOperation usersHandle (Right userParams) =
    uhCreateUserInDb usersHandle userParams

registrationSendResult ::
       Monad m
    => Either SomeError Token
    -> m (Either ResponseErrorMessage ResponseOkMessage)
registrationSendResult result =
    case result of
        Left someError ->
            return $
            Left $ toResponseErrorMessage "User not registered." someError
        Right tk -> do
            return $
                Right $ Created $ LBS.fromStrict $ E.encodeUtf8 $ getToken tk

registrationHandle ::
       Monad m
    => UsersHandle m
    -> AnswerHandle m (Either SomeError CreateUser) Token
registrationHandle usersHandle =
    AnswerHandle
        { parseInformation = registrationParseInformation usersHandle
        , databaseOperation = registrationDatabaseOperation usersHandle
        , sendResult = registrationSendResult
        }

---------------------------------------------------------------------------------------------------------------------
deleteUserParseInformation ::
       Monad m
    => UsersHandle m
    -> Request
    -> m (Either SomeError (Maybe Token, Maybe Login))
deleteUserParseInformation handler request =
    if requestMethod request /= methodDelete
        then do
            logError (uhLogger handler) "Bad request method"
            return $ Left BadMethod
        else do
            logInfo (uhLogger handler) "Preparing data for deleting user."
            let login =
                    Login . E.decodeUtf8 <$>
                    fromMaybe Nothing (lookup "login" $ queryString request)
            let token = takeToken request
            return $ Right (token, login)

deleteUserDatabaseOperation ::
       Monad m
    => UsersHandle m
    -> Either SomeError (Maybe Token, Maybe Login)
    -> m (Either SomeError ())
deleteUserDatabaseOperation _ (Left message) = return $ Left message
deleteUserDatabaseOperation usersHandle (Right (token, login)) =
    uhDeleteUserFromDb usersHandle token login

deleteUserSendResult ::
       Monad m
    => Either SomeError ()
    -> m (Either ResponseErrorMessage ResponseOkMessage)
deleteUserSendResult result =
    case result of
        Left someError ->
            return $ Left $ toResponseErrorMessage "User not deleted." someError
        Right _ -> do
            return $ Right $ OkMessage "User deleted."

deleteUserHandle ::
       Monad m
    => UsersHandle m
    -> AnswerHandle m (Either SomeError (Maybe Token, Maybe Login)) ()
deleteUserHandle usersHandle =
    AnswerHandle
        { parseInformation = deleteUserParseInformation usersHandle
        , databaseOperation = deleteUserDatabaseOperation usersHandle
        , sendResult = deleteUserSendResult
        }

---------------------------------------------------------------------------------------------------------------------
profileUserParseInformation ::
       Monad m => UsersHandle m -> Request -> m (Either SomeError (Maybe Token))
profileUserParseInformation handler request =
    if requestMethod request /= methodGet
        then do
            logError (uhLogger handler) "Bad request method"
            return $ Left BadMethod
        else do
            logInfo
                (uhLogger handler)
                "Preparing data for sending user information."
            let token = takeToken request
            return $ Right token

profileUserDatabaseOperation ::
       Monad m
    => UsersHandle m
    -> Either SomeError (Maybe Token)
    -> m (Either SomeError Profile)
profileUserDatabaseOperation _ (Left message) = return $ Left message
profileUserDatabaseOperation usersHandle (Right token) =
    uhProfileOnDb usersHandle token

profileUserSendResult ::
       Monad m
    => Either SomeError Profile
    -> m (Either ResponseErrorMessage ResponseOkMessage)
profileUserSendResult result =
    case result of
        Left someError ->
            return $
            Left $
            toResponseErrorMessage "Profile inforamtion not sended." someError
        Right pro -> do
            return $ Right $ OkJSON $ encode pro

profileUserHandle ::
       Monad m
    => UsersHandle m
    -> AnswerHandle m (Either SomeError (Maybe Token)) Profile
profileUserHandle usersHandle =
    AnswerHandle
        { parseInformation = profileUserParseInformation usersHandle
        , databaseOperation = profileUserDatabaseOperation usersHandle
        , sendResult = profileUserSendResult
        }
---------------------------------------------------------------------------------------------------------------------
