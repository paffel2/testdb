{-# LANGUAGE OverloadedStrings #-}

module Controllers.Users where

import           Answer                (answer'')
import           Answers.Users         (deleteUserHandle, profileUserHandle,
                                        registrationHandle, signInHandle)
import           Control.Monad.Except  (ExceptT, MonadIO, runExceptT)
import           Data.Aeson            (encode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.Text.Encoding    as E
import           Logger                (LoggerHandle, logInfo)
import           Network.Wai           (Request)
import           OperationsHandle      (UsersHandle)
import           Responses             (toResponseErrorMessage,
                                        toResponseErrorMessage')
import           Types.Other           (ResponseErrorMessage,
                                        ResponseOkMessage (Created, OkJSON, OkMessage),
                                        SomeError, Token (getToken))
import           Types.Users           (Profile)

signIn ::
       MonadIO m
    => UsersHandle (ExceptT SomeError m)
    -> LoggerHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
signIn operations hLogger req =
    signInSendResult hLogger $ answer'' req (signInHandle operations)

deleteUser ::
       MonadIO m
    => UsersHandle (ExceptT SomeError m)
    -> LoggerHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
deleteUser operations hLogger req =
    deleteUserSendResult hLogger $ answer'' req (deleteUserHandle operations)

profile ::
       MonadIO m
    => UsersHandle (ExceptT SomeError m)
    -> LoggerHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
profile operations hLogger req =
    profileSendResult hLogger $ answer'' req (profileUserHandle operations)

registration ::
       MonadIO m
    => UsersHandle (ExceptT SomeError m)
    -> LoggerHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
registration operations hLogger req =
    registrationSendResult hLogger $
    answer'' req (registrationHandle operations)

signInSendResult ::
       Monad m
    => LoggerHandle m
    -> ExceptT SomeError m Token
    -> m (Either ResponseErrorMessage ResponseOkMessage)
signInSendResult hLogger result = do
    a <- runExceptT result
    case a of
        Left someError ->
            Left <$>
            toResponseErrorMessage' hLogger "Bad authorization." someError
        Right token -> do
            logInfo hLogger "User logged."
            return $
                Right $
                OkMessage $ LBS.fromStrict $ E.encodeUtf8 $ getToken token

deleteUserSendResult ::
       Monad m
    => LoggerHandle m
    -> ExceptT SomeError m ()
    -> m (Either ResponseErrorMessage ResponseOkMessage)
deleteUserSendResult hLogger result = do
    a <- runExceptT result
    case a of
        Left someError ->
            return $ Left $ toResponseErrorMessage "User not deleted." someError
        Right token -> do
            logInfo hLogger "User deleted."
            return $ Right $ OkMessage "User deleted."

profileSendResult ::
       Monad m
    => LoggerHandle m
    -> ExceptT SomeError m Profile
    -> m (Either ResponseErrorMessage ResponseOkMessage)
profileSendResult hLogger result = do
    a <- runExceptT result
    case a of
        Left someError ->
            return $
            Left $
            toResponseErrorMessage "Profile inforamtion not sended." someError
        Right profile -> do
            logInfo hLogger "User information sended."
            return $ Right $ OkJSON $ encode profile

{-registrationSendResult ::
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
            Right $ Created $ LBS.fromStrict $ E.encodeUtf8 $ getToken token-}
registrationSendResult ::
       Monad m
    => LoggerHandle m
    -> ExceptT SomeError m Token
    -> m (Either ResponseErrorMessage ResponseOkMessage)
registrationSendResult hLogger result = do
    a <- runExceptT result
    case a of
        Left someError ->
            Left <$>
            toResponseErrorMessage' hLogger "User not registered." someError
        Right token -> do
            logInfo hLogger "User registered.."
            return $
                Right $ Created $ LBS.fromStrict $ E.encodeUtf8 $ getToken token
