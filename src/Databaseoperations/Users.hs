{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Databaseoperations.Users where

import           Control.Monad.Except          (MonadError (catchError, throwError),
                                                MonadIO (..))
import qualified Data.ByteString.Char8         as BC
import           Data.Pool                     (Pool)
import qualified Data.Text                     as T
import           Data.Time                     (UTCTime, getCurrentTime)
import           Database.PostgreSQL.Simple    (Binary (fromBinary), Connection)
import           Databaseoperations.CheckAdmin (checkAdmin)
import           HelpFunction                  (someErrorToInt)
import           PostgreSqlWithPool            (executeWithPool, queryWithPool)
import           Types.Other                   (MonadIOWithError,
                                                SomeError (BadToken, OtherError),
                                                Token (Token), TokenLifeTime)
import           Types.Users                   (CreateUser (CreateUser, cuFirstName, cuLastName, cuUserLogin, cuUserPassword),
                                                Login (getLogin), Password,
                                                Profile,
                                                TokenProfile (TokenProfile))

generateToken :: MonadIO m => Login -> m (Token, UTCTime)
generateToken login = do
    now <- liftIO getCurrentTime
    let token = T.pack $ Prelude.filter (`Prelude.notElem` filt) (show now)
    return (Token (getLogin login <> token), now)
  where
    filt = " :.-UTC" :: String

--------------------------------------------------------------------------------------------------------------------------------------
firstToken ::
       MonadIOWithError m
    => Pool Connection
    -> Maybe Login
    -> Maybe Password
    -> m Token
firstToken pool (Just login) (Just password) = do
    (token, now) <- generateToken login
    n <- executeWithPool pool q (login, password, token, now)
    if n > 0
        then return token
        else throwError $ OtherError "Bad login or password"
  where
    q =
        "WITH loging_user as (select user_id from users where login = ? and user_password = (crypt(?,user_password))) \
            \insert into tokens (user_id,token,creation_date) values ((select user_id from loging_user),?,?)"
firstToken _ _ _ = throwError $ OtherError "Bad login or password parameters"

--------------------------------------------------------------------------------------------------------------------------
authentication ::
       MonadIOWithError m
    => Pool Connection
    -> Maybe Login
    -> Maybe Password
    -> m Token
authentication _ _ Nothing = throwError $ OtherError "No login parameter."
authentication _ Nothing _ = throwError $ OtherError "No password parameter."
authentication pool (Just login) (Just password) = do
    (token, now) <- generateToken login
    n <- executeWithPool pool q (login, password, token, now)
    if n > 0
        then return token
        else throwError $ OtherError "Wrong login or password."
  where
    q =
        "WITH loging_user as (select user_id from users where login = ? and user_password = (crypt(?,user_password))) \
         \update tokens set token = ?, creation_date = ? where user_id in (select user_id from loging_user)"

-------------------------------------------------------------------------------------------------------
deleteUserFromDb ::
       MonadIOWithError m
    => Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> Maybe Login
    -> m ()
deleteUserFromDb _ _ _ Nothing = throwError $ OtherError "No login parameter"
deleteUserFromDb pool tokenLifeTime token (Just login) = do
    checkAdmin pool tokenLifeTime token
    n <- executeWithPool pool "delete from users where login = ?" [login]
    if n > 0
        then return ()
        else throwError $ OtherError "User not exist"

profileOnDb ::
       MonadIOWithError m
    => Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> m Profile
profileOnDb _ _ Nothing = throwError $ OtherError "No token parameter"
profileOnDb pool tokenLifeTime (Just token) = do
    rows <-
        queryWithPool
            pool
            "select first_name, last_name, avatar from users join tokens using (user_id) where token = ? and (now()- tokens.creation_date) < make_interval(secs => ?)"
            (TokenProfile token tokenLifeTime)
    case rows of
        [] -> do
            throwError BadToken
        (profile:_) -> do
            return profile

createUserInDb :: MonadIOWithError m => Pool Connection -> CreateUser -> m Token
createUserInDb _ CreateUser {cuFirstName = Nothing} =
    throwError $ OtherError "User not created. No first_name parameter"
createUserInDb _ CreateUser {cuLastName = Nothing} =
    throwError $ OtherError "No last_name parameter"
createUserInDb _ CreateUser {cuUserLogin = Nothing} =
    throwError $ OtherError "No login parameter"
createUserInDb _ CreateUser {cuUserPassword = Nothing} =
    throwError $ OtherError "No password parameter"
createUserInDb pool createUser@(CreateUser (Just avFileName) (Just avContent) (Just avContentType) (Just _) (Just _) (Just _) (Just _) _) = do
    if avFileName == "" ||
       avContentType == "" ||
       fromBinary avContent == "" || BC.take 5 avContentType /= "image"
        then throwError $ OtherError "Bad image file"
        else do
            n <-
                catchError (executeWithPool pool q createUser) $ \e ->
                    case someErrorToInt e of
                        22001 ->
                            throwError $
                            OtherError "One or more parameter too long"
                        23505 ->
                            throwError $
                            OtherError "User with that login already exist"
                        _ -> throwError e
            if n > 0
                then firstToken
                         pool
                         (cuUserLogin createUser)
                         (cuUserPassword createUser)
                else do
                    throwError $ OtherError "Registration failed"
  where
    q =
        "with avatar_id as (insert into images (image_name,image_b,content_type) values (?,?,?) returning image_id) \
         \insert into users (first_name, last_name, avatar, login, user_password, creation_date, admin_mark) \
         \values (?,?,(select image_id from avatar_id),?,crypt(?,gen_salt('md5')),now(),?)"
createUserInDb pool (CreateUser Nothing Nothing Nothing (Just firstName) (Just lastName) (Just login) (Just password) _) = do
    n <-
        catchError
            (executeWithPool
                 pool
                 q
                 (firstName, lastName, login, password, False)) $ \e ->
            case someErrorToInt e of
                22001 ->
                    throwError $ OtherError "One or more parameter too long"
                23505 ->
                    throwError $ OtherError "User with that login already exist"
                _ -> throwError e
    if n > 0
        then firstToken pool (Just login) (Just password)
        else throwError $ OtherError "Registration failed"
  where
    q =
        "insert into users (first_name, last_name, login, user_password, creation_date, admin_mark) \
             \values (?,?,?,crypt(?,gen_salt('md5')),now(),?)"
createUserInDb _ _ = throwError $ OtherError "Unexpected error"
-----------------------------------------------------------------------------------------------
