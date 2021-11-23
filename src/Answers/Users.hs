{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Answers.Users where

import           Answer
import           Control.Monad.Except
import           Data.Aeson                (encode)
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as LBS
import           Data.Maybe                (fromMaybe)
import qualified Data.Text.Encoding        as E
import           Databaseoperations.Users
import           FromRequest               (parseRequestBodyLBS, takeToken,
                                            toCreateUser, toLogin, toPassword)
import           HelpFunction              (foundParametr)
import           Logger
import           Network.HTTP.Types.Method (methodDelete, methodGet, methodPost)
import           Network.Wai               (Request (queryString, requestMethod))
import           OperationsHandle
import           Responses                 (toResponseErrorMessage)
import           Types.Other               (MonadWithError,
                                            ResponseErrorMessage,
                                            ResponseOkMessage (Created, OkJSON, OkMessage),
                                            SomeError (..), Token (getToken))
import           Types.Users               (CreateUser (..), Login (Login),
                                            Password, Profile)

{-signInParseInformation ::
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
-}
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
