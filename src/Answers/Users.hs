{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Answers.Users where

import           Answer                    (AnswerHandle' (..))
import           Control.Monad.Except      (ExceptT, MonadError (throwError),
                                            MonadIO (liftIO), runExceptT)
import           Data.Aeson                (encode)
import qualified Data.ByteString.Lazy      as LBS
import           Data.Maybe                (fromMaybe)
import qualified Data.Text.Encoding        as E
import           FromRequest               (parseRequestBodyLBS, takeToken,
                                            toCreateUser, toLogin, toPassword)
import           HelpFunction              (foundParametr)
import           Logger                    (logError, logInfo)
import           Network.HTTP.Types.Method (methodDelete, methodGet, methodPost)
import           Network.Wai               (Request (queryString, requestMethod))
import           OperationsHandle          (UsersHandle (uhAuth, uhCreateUserInDb, uhDeleteUserFromDb, uhLogger, uhProfileOnDb))
import           Responses                 (toResponseErrorMessage)
import           Types.Other               (MonadWithError,
                                            ResponseErrorMessage,
                                            ResponseOkMessage (Created, OkJSON, OkMessage),
                                            SomeError (..), Token (getToken))
import           Types.Users               (CreateUser (..), Login (Login),
                                            Password, Profile)

---------------------------------------------------------------------------------------------------------------------
registrationParseInformation ::
       UsersHandle MonadWithError IO -> Request -> MonadWithError CreateUser
registrationParseInformation handle request =
    if requestMethod request /= methodPost
        then do
            liftIO $ logError (uhLogger handle) "Bad request method"
            throwError BadMethod
        else do
            liftIO $
                logInfo
                    (uhLogger handle)
                    "Preparing data for registration new user."
            (i, f) <- liftIO $ parseRequestBodyLBS request
            let avatar = foundParametr "avatar" f
            let userParams = toCreateUser i avatar
            return userParams

registrationDatabaseOperation ::
       UsersHandle MonadWithError IO -> CreateUser -> MonadWithError Token
registrationDatabaseOperation = uhCreateUserInDb

registrationSendResult ::
       MonadWithError Token
    -> IO (Either ResponseErrorMessage ResponseOkMessage)
registrationSendResult result = do
    a <- runExceptT result
    case a of
        Left someError ->
            return $
            Left $ toResponseErrorMessage "User not registered." someError
        Right token ->
            return $
            Right $ Created $ LBS.fromStrict $ E.encodeUtf8 $ getToken token

registrationHandle ::
       UsersHandle MonadWithError IO
    -> AnswerHandle' MonadWithError CreateUser Token IO
registrationHandle usersHandle =
    AnswerHandle'
        { parseInformation' = registrationParseInformation usersHandle
        , databaseOperation' = registrationDatabaseOperation usersHandle
        , sendResult' = registrationSendResult
        }

-------------------------------------------------------------------------------------------
profileUserParseInformation ::
       UsersHandle MonadWithError IO -> Request -> MonadWithError (Maybe Token)
profileUserParseInformation handler request =
    if requestMethod request /= methodGet
        then do
            liftIO $ logError (uhLogger handler) "Bad request method"
            throwError BadMethod
        else do
            liftIO $
                logInfo
                    (uhLogger handler)
                    "Preparing data for sending user information."
            let token = takeToken request
            return token

profileUserDatabaseOperation ::
       UsersHandle MonadWithError IO -> Maybe Token -> MonadWithError Profile
profileUserDatabaseOperation = uhProfileOnDb

profileUserSendResult ::
       MonadWithError Profile
    -> IO (Either ResponseErrorMessage ResponseOkMessage)
profileUserSendResult result = do
    a <- runExceptT result
    case a of
        Left someError ->
            return $
            Left $
            toResponseErrorMessage "Profile inforamtion not sended." someError
        Right profile -> return $ Right $ OkJSON $ encode profile

profileUserHandle ::
       UsersHandle MonadWithError IO
    -> AnswerHandle' MonadWithError (Maybe Token) Profile IO
profileUserHandle usersHandle =
    AnswerHandle'
        { parseInformation' = profileUserParseInformation usersHandle
        , databaseOperation' = profileUserDatabaseOperation usersHandle
        , sendResult' = profileUserSendResult
        }

----------------------------------------------------------------------------------------------
signInParseInformation ::
       UsersHandle MonadWithError IO
    -> Request
    -> MonadWithError (Maybe Login, Maybe Password)
signInParseInformation handler request =
    if requestMethod request /= methodGet
        then do
            liftIO $ logError (uhLogger handler) "Bad request method"
            throwError BadMethod
        else do
            liftIO $ logInfo (uhLogger handler) "Preparing data for sign in."
            (i, _) <- liftIO $ parseRequestBodyLBS request
            let login = toLogin i
            let pass = toPassword i
            return (login, pass)

signInDatabaseOperation ::
       UsersHandle MonadWithError IO
    -> (Maybe Login, Maybe Password)
    -> MonadWithError Token
signInDatabaseOperation usersHandle (login, pass) =
    uhAuth usersHandle login pass

signInSendResult ::
       MonadWithError Token
    -> IO (Either ResponseErrorMessage ResponseOkMessage)
signInSendResult result = do
    a <- runExceptT result
    case a of
        Left someError ->
            return $
            Left $ toResponseErrorMessage "Bad authorization." someError
        Right tk -> do
            return $
                Right $ OkMessage $ LBS.fromStrict $ E.encodeUtf8 $ getToken tk

signInHandle ::
       UsersHandle MonadWithError IO
    -> AnswerHandle' MonadWithError (Maybe Login, Maybe Password) Token IO
signInHandle usersHandle =
    AnswerHandle'
        { parseInformation' = signInParseInformation usersHandle
        , databaseOperation' = signInDatabaseOperation usersHandle
        , sendResult' = signInSendResult
        }

------------------------------------------------------------------------------------------------------------
deleteUserParseInformation ::
       UsersHandle MonadWithError IO
    -> Request
    -> MonadWithError (Maybe Token, Maybe Login)
deleteUserParseInformation handler request =
    if requestMethod request /= methodDelete
        then do
            liftIO $ logError (uhLogger handler) "Bad request method"
            throwError BadMethod
        else do
            liftIO $
                logInfo (uhLogger handler) "Preparing data for deleting user."
            let login =
                    Login . E.decodeUtf8 <$>
                    fromMaybe Nothing (lookup "login" $ queryString request)
            let token = takeToken request
            return (token, login)

deleteUserDatabaseOperation ::
       UsersHandle MonadWithError IO
    -> (Maybe Token, Maybe Login)
    -> MonadWithError ()
deleteUserDatabaseOperation usersHandle (token, login) =
    uhDeleteUserFromDb usersHandle token login

deleteUserSendResult ::
       MonadWithError () -> IO (Either ResponseErrorMessage ResponseOkMessage)
deleteUserSendResult result = do
    a <- runExceptT result
    case a of
        Left someError ->
            return $ Left $ toResponseErrorMessage "User not deleted." someError
        Right _ -> do
            return $ Right $ OkMessage "User deleted."

deleteUserHandle ::
       UsersHandle MonadWithError IO
    -> AnswerHandle' (ExceptT SomeError IO) (Maybe Token, Maybe Login) () IO
deleteUserHandle usersHandle =
    AnswerHandle'
        { parseInformation' = deleteUserParseInformation usersHandle
        , databaseOperation' = deleteUserDatabaseOperation usersHandle
        , sendResult' = deleteUserSendResult
        }
