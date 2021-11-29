{-# LANGUAGE OverloadedStrings #-}

module Controllers.Users where

import           Answer               (answer)
import           Answers.Users        (deleteUserHandle, profileUserHandle,
                                       registrationHandle, signInHandle)
import           Control.Monad.Except (ExceptT, MonadIO)
import           Data.Aeson           (encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding   as E
import           HelpFunction         (sendResult)
import           Logger               (LoggerHandle)
import           Network.Wai          (Request)
import           OperationsHandle     (UsersHandle)
import           Types.Other          (ResponseErrorMessage,
                                       ResponseOkMessage (Created, OkJSON, OkMessage),
                                       SomeError, Token (getToken))

signIn ::
       MonadIO m
    => UsersHandle (ExceptT SomeError m)
    -> LoggerHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
signIn operations hLogger req =
    sendResult hLogger "User not logged." signInOK $
    answer req (signInHandle operations)
  where
    signInOK token = OkMessage $ LBS.fromStrict $ E.encodeUtf8 $ getToken token

deleteUser ::
       MonadIO m
    => UsersHandle (ExceptT SomeError m)
    -> LoggerHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
deleteUser operations hLogger req =
    sendResult hLogger "User not deleted." deleteOK $
    answer req (deleteUserHandle operations)
  where
    deleteOK _ = OkMessage "User deleted."

profile ::
       MonadIO m
    => UsersHandle (ExceptT SomeError m)
    -> LoggerHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
profile operations hLogger req =
    sendResult hLogger "Profile inforamtion not sended." profileOk $
    answer req (profileUserHandle operations)
  where
    profileOk someProfile = OkJSON $ encode someProfile

registration ::
       MonadIO m
    => UsersHandle (ExceptT SomeError m)
    -> LoggerHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
registration operations hLogger req =
    sendResult hLogger "User not registered." registrationOk $
    answer req (registrationHandle operations)
  where
    registrationOk token =
        Created $ LBS.fromStrict $ E.encodeUtf8 $ getToken token
