{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Answers.Users where

import           Answer                    (AnswerHandle'' (..))
import           Control.Monad.Except      (MonadError (throwError), MonadIO)
import           Data.Maybe                (fromMaybe)
import qualified Data.Text.Encoding        as E
import           FromRequest               (takeToken, toCreateUser, toLogin,
                                            toPassword)
import           HelpFunction              (foundParametr)
import           Network.HTTP.Types.Method (methodDelete, methodGet, methodPost)
import           Network.Wai               (Request (queryString, requestMethod))
import           OperationsHandle          (UsersHandle (uhAuth, uhCreateUserInDb, uhDeleteUserFromDb, uhParseRequestBody, uhProfileOnDb))
import           Types.Other               (SomeError (BadMethod), Token)
import           Types.Users               (CreateUser, Login (Login), Password,
                                            Profile)

---------------------------------------------------------------------------------------------------------------------
registrationParseInformation ::
       (MonadIO m, MonadError SomeError m)
    => UsersHandle m
    -> Request
    -> m CreateUser
registrationParseInformation handle request =
    if requestMethod request /= methodPost
        then throwError BadMethod
        else do
            (i, f) <- uhParseRequestBody handle request
            let avatar = foundParametr "avatar" f
            let userParams = toCreateUser i avatar
            return userParams

registrationDatabaseOperation ::
       (MonadIO m, MonadError SomeError m)
    => UsersHandle m
    -> CreateUser
    -> m Token
registrationDatabaseOperation = uhCreateUserInDb

registrationHandle ::
       (MonadIO m, MonadError SomeError m)
    => UsersHandle m
    -> AnswerHandle'' m CreateUser Token
registrationHandle usersHandle =
    AnswerHandle''
        { parseInformation'' = registrationParseInformation usersHandle
        , databaseOperation'' = registrationDatabaseOperation usersHandle
        }

----------------------------------------------------------------------------------------------
signInParseInformation ::
       (MonadIO m, MonadError SomeError m)
    => UsersHandle m
    -> Request
    -> m (Maybe Login, Maybe Password)
signInParseInformation handler request =
    if requestMethod request /= methodGet
        then throwError BadMethod
        else do
            (i, _) <- uhParseRequestBody handler request
            let login = toLogin i
            let pass = toPassword i
            return (login, pass)

signInDatabaseOperation ::
       (MonadIO m, MonadError SomeError m)
    => UsersHandle m
    -> (Maybe Login, Maybe Password)
    -> m Token
signInDatabaseOperation usersHandle (login, pass) =
    uhAuth usersHandle login pass

signInHandle ::
       (MonadIO m, MonadError SomeError m)
    => UsersHandle m
    -> AnswerHandle'' m (Maybe Login, Maybe Password) Token
signInHandle usersHandle =
    AnswerHandle''
        { parseInformation'' = signInParseInformation usersHandle
        , databaseOperation'' = signInDatabaseOperation usersHandle
        }

------------------------------------------------------------------------------------------------------------
deleteUserParseInformation ::
       (MonadIO m, MonadError SomeError m)
    => UsersHandle m
    -> Request
    -> m (Maybe Token, Maybe Login)
deleteUserParseInformation handler request =
    if requestMethod request /= methodDelete
        then throwError BadMethod
        else do
            let login =
                    Login . E.decodeUtf8 <$>
                    fromMaybe Nothing (lookup "login" $ queryString request)
            let token = takeToken request
            return (token, login)

deleteUserDatabaseOperation ::
       (MonadIO m, MonadError SomeError m)
    => UsersHandle m
    -> (Maybe Token, Maybe Login)
    -> m ()
deleteUserDatabaseOperation usersHandle (token, login) =
    uhDeleteUserFromDb usersHandle token login

deleteUserHandle ::
       (MonadIO m, MonadError SomeError m)
    => UsersHandle m
    -> AnswerHandle'' m (Maybe Token, Maybe Login) ()
deleteUserHandle usersHandle =
    AnswerHandle''
        { parseInformation'' = deleteUserParseInformation usersHandle
        , databaseOperation'' = deleteUserDatabaseOperation usersHandle
        }

---------------------------------------------------------------------------------
profileUserParseInformation ::
       (MonadIO m, MonadError SomeError m)
    => UsersHandle m
    -> Request
    -> m (Maybe Token)
profileUserParseInformation handler request =
    if requestMethod request /= methodGet
        then throwError BadMethod
        else return $ takeToken request

profileUserDatabaseOperation ::
       (MonadIO m, MonadError SomeError m)
    => UsersHandle m
    -> Maybe Token
    -> m Profile
profileUserDatabaseOperation = uhProfileOnDb

profileUserHandle ::
       (MonadIO m, MonadError SomeError m)
    => UsersHandle m
    -> AnswerHandle'' m (Maybe Token) Profile
profileUserHandle usersHandle =
    AnswerHandle''
        { parseInformation'' = profileUserParseInformation usersHandle
        , databaseOperation'' = profileUserDatabaseOperation usersHandle
        }
