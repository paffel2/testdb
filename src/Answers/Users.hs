{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Answers.Users where

import           Answer                    (AnswerHandle (..))
import           Control.Monad.Except      (MonadError (throwError))
import           Data.Maybe                (fromMaybe)
import qualified Data.Text.Encoding        as E
import           FromRequest               (takeToken, toCreateUser, toLogin,
                                            toPassword)
import           HelpFunction              (foundParametr)
import           Network.HTTP.Types.Method (methodDelete, methodGet, methodPost)
import           Network.Wai               (Request (queryString, requestMethod))
import           OperationsHandle          (UsersHandle (uhAuth, uhCreateUserInDb, uhDeleteUserFromDb, uhParseRequestBody, uhProfileOnDb))
import           Types.Other               (MonadIOWithError,
                                            SomeError (BadMethod), Token)
import           Types.Users               (CreateUser, Login (Login), Password,
                                            Profile)

---------------------------------------------------------------------------------------------------------------------
registrationParseInformation ::
       MonadIOWithError m => UsersHandle m -> Request -> m CreateUser
registrationParseInformation handle request =
    if requestMethod request /= methodPost
        then throwError BadMethod
        else do
            (i, f) <- uhParseRequestBody handle request
            let avatar = foundParametr "avatar" f
            let userParams = toCreateUser i avatar
            return userParams

registrationDatabaseOperation ::
       MonadIOWithError m => UsersHandle m -> CreateUser -> m Token
registrationDatabaseOperation = uhCreateUserInDb

registrationHandle ::
       MonadIOWithError m => UsersHandle m -> AnswerHandle m CreateUser Token
registrationHandle usersHandle =
    AnswerHandle
        { parseInformation = registrationParseInformation usersHandle
        , databaseOperation = registrationDatabaseOperation usersHandle
        }

----------------------------------------------------------------------------------------------
signInParseInformation ::
       MonadIOWithError m
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
       MonadIOWithError m
    => UsersHandle m
    -> (Maybe Login, Maybe Password)
    -> m Token
signInDatabaseOperation usersHandle (login, pass) =
    uhAuth usersHandle login pass

signInHandle ::
       MonadIOWithError m
    => UsersHandle m
    -> AnswerHandle m (Maybe Login, Maybe Password) Token
signInHandle usersHandle =
    AnswerHandle
        { parseInformation = signInParseInformation usersHandle
        , databaseOperation = signInDatabaseOperation usersHandle
        }

------------------------------------------------------------------------------------------------------------
deleteUserParseInformation ::
       MonadIOWithError m
    => UsersHandle m
    -> Request
    -> m (Maybe Token, Maybe Login)
deleteUserParseInformation _ request =
    if requestMethod request /= methodDelete
        then throwError BadMethod
        else do
            let login =
                    Login . E.decodeUtf8 <$>
                    fromMaybe Nothing (lookup "login" $ queryString request)
            let token = takeToken request
            return (token, login)

deleteUserDatabaseOperation ::
       MonadIOWithError m => UsersHandle m -> (Maybe Token, Maybe Login) -> m ()
deleteUserDatabaseOperation usersHandle (token, login) =
    uhDeleteUserFromDb usersHandle token login

deleteUserHandle ::
       MonadIOWithError m
    => UsersHandle m
    -> AnswerHandle m (Maybe Token, Maybe Login) ()
deleteUserHandle usersHandle =
    AnswerHandle
        { parseInformation = deleteUserParseInformation usersHandle
        , databaseOperation = deleteUserDatabaseOperation usersHandle
        }

---------------------------------------------------------------------------------
profileUserParseInformation ::
       MonadIOWithError m => UsersHandle m -> Request -> m (Maybe Token)
profileUserParseInformation _ request =
    if requestMethod request /= methodGet
        then throwError BadMethod
        else return $ takeToken request

profileUserDatabaseOperation ::
       MonadIOWithError m => UsersHandle m -> Maybe Token -> m Profile
profileUserDatabaseOperation = uhProfileOnDb

profileUserHandle ::
       MonadIOWithError m
    => UsersHandle m
    -> AnswerHandle m (Maybe Token) Profile
profileUserHandle usersHandle =
    AnswerHandle
        { parseInformation = profileUserParseInformation usersHandle
        , databaseOperation = profileUserDatabaseOperation usersHandle
        }
