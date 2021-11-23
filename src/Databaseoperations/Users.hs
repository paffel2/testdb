{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Databaseoperations.Users where

import           Control.Exception             (catch, try)
import qualified Data.ByteString.Char8         as BC
import           Data.Maybe                    (fromMaybe)
import           Data.Pool                     (Pool)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import           Data.Time                     (UTCTime, getCurrentTime)
import           Database.PostgreSQL.Simple    (Binary (fromBinary), Connection,
                                                SqlError (sqlErrorMsg, sqlState))

import           Control.Monad.Except          (MonadError (throwError),
                                                MonadIO (..))
import           Databaseoperations.CheckAdmin (checkAdmin')
import           HelpFunction                  (readByteStringToInt)
import           Logger                        (LoggerHandle, logError, logInfo)
import           PostgreSqlWithPool            (executeWithPool, queryWithPool)
import           Types.Other                   (SomeError (BadToken, DatabaseError, NotAdmin, OtherError),
                                                Token (Token), TokenLifeTime)
import           Types.Users                   (CreateUser (..),
                                                Login (getLogin), Password,
                                                Profile,
                                                TokenProfile (TokenProfile))

generateToken :: Login -> IO (Token, UTCTime)
generateToken login = do
    now <- getCurrentTime
    let token = T.pack $ Prelude.filter (`Prelude.notElem` filt) (show now)
    return (Token (getLogin login <> token), now)
  where
    filt = " :.-UTC" :: String

firstToken ::
       LoggerHandle IO
    -> Pool Connection
    -> Maybe Login
    -> Maybe Password
    -> IO (Either SomeError Token)
firstToken hLogger pool (Just login) (Just password) =
    catch
        (do logInfo hLogger $
                T.concat ["Generate first token for user ", getLogin login]
            (token, now) <- generateToken login
            n <- executeWithPool pool q (login, password, token, now)
            if n > 0
                then do
                    logInfo hLogger $
                        T.concat ["User with login ", getLogin login, " logged"]
                    return $ Right token
                else do
                    logError hLogger $
                        T.concat
                            ["User with login ", getLogin login, " cant logged"]
                    return . Left . OtherError $ "Wrong login or password") $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        logError hLogger err
        return $ Left DatabaseError
  where
    q =
        "WITH loging_user as (select user_id from users where login = ? and user_password = (crypt(?,user_password))) \
            \insert into tokens (user_id,token,creation_date) values ((select user_id from loging_user),?,?)"
firstToken hLogger _ _ _ = do
    logError hLogger "Bad login or password parameters"
    return . Left . OtherError $ "Bad login or password parameters"

--------------------------------------------------------------------------------------------------------------------------------------
createUserInDb ::
       (MonadIO m, MonadError SomeError m)
    => Pool Connection
    -> LoggerHandle IO
    -> CreateUser
    -> m Token
createUserInDb _ hLogger CreateUser {cuFirstName = Nothing} = do
    liftIO $ logError hLogger "User not created. No first_name parameter"
    throwError $ OtherError "User not created. No first_name parameter"
createUserInDb _ hLogger CreateUser {cuLastName = Nothing} = do
    liftIO $ logError hLogger "User not created. No last_name parameter"
    throwError $ OtherError "No last_name parameter"
createUserInDb _ hLogger CreateUser {cuUserLogin = Nothing} = do
    liftIO $ logError hLogger "User not created. No login parameter"
    throwError $ OtherError "No login parameter"
createUserInDb _ hLogger CreateUser {cuUserPassword = Nothing} = do
    liftIO $ logError hLogger "User not created. No password parameter"
    throwError $ OtherError "No password parameter"
createUserInDb pool hLogger createUser@(CreateUser (Just avFileName) (Just avContent) (Just avContentType) (Just _) (Just _) (Just _) (Just _) _) = do
    if avFileName == "" ||
       avContentType == "" ||
       fromBinary avContent == "" || BC.take 5 avContentType /= "image"
        then do
            liftIO $ logError hLogger "User not created. Bad image file"
            throwError $ OtherError "Bad image file"
        else do
            n <- liftIO $ try $ executeWithPool pool q createUser
            case n of
                Left e -> errorHandle e
                Right 0 -> do
                    liftIO $ logError hLogger "Registration failed"
                    throwError $ OtherError "Registration failed"
                Right _ -> do
                    liftIO $ logInfo hLogger "New user registered."
                    firstToken'
                        hLogger
                        pool
                        (cuUserLogin createUser)
                        (cuUserPassword createUser)
  where
    q =
        "with avatar_id as (insert into images (image_name,image_b,content_type) values (?,?,?) returning image_id) \
         \insert into users (first_name, last_name, avatar, login, user_password, creation_date, admin_mark) \
         \values (?,?,(select image_id from avatar_id),?,crypt(?,gen_salt('md5')),now(),?)"
    errorHandle sqlError = do
        let errState = sqlState sqlError
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        let err = E.decodeUtf8 $ sqlErrorMsg sqlError
        liftIO $ logError hLogger err
        case errStateInt of
            22001 -> throwError $ OtherError "One or more parameter too long"
            23505 ->
                throwError $ OtherError "User with that login already exist"
            _ -> throwError DatabaseError
createUserInDb pool hLogger (CreateUser Nothing Nothing Nothing (Just firstName) (Just lastName) (Just login) (Just password) _) = do
    liftIO $ logInfo hLogger "Registartion without avatar"
    n <-
        liftIO $
        try $
        executeWithPool pool q (firstName, lastName, login, password, False)
    case n of
        Left e -> errorHandle e
        Right 0 -> do
            liftIO $ logError hLogger "Registration failed"
            throwError $ OtherError "Registration failed"
        Right _ -> do
            liftIO $ logInfo hLogger "New user registered."
            firstToken' hLogger pool (Just login) (Just password)
  where
    q =
        "insert into users (first_name, last_name, login, user_password, creation_date, admin_mark) \
             \values (?,?,?,crypt(?,gen_salt('md5')),now(),?)"
    errorHandle sqlError = do
        let errState = sqlState sqlError
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        let err = E.decodeUtf8 $ sqlErrorMsg sqlError
        liftIO $ logError hLogger err
        case errStateInt of
            22001 -> throwError $ OtherError "One or more parameter too long"
            23505 ->
                throwError $ OtherError "User with that login already exist"
            _ -> throwError DatabaseError
createUserInDb _ hLogger _ = do
    liftIO $ logError hLogger "Unexpected error"
    throwError $ OtherError "Unexpected error"

firstToken' ::
       (MonadIO m, MonadError SomeError m)
    => LoggerHandle IO
    -> Pool Connection
    -> Maybe Login
    -> Maybe Password
    -> m Token
firstToken' hLogger pool (Just login) (Just password) = do
    liftIO $
        logInfo hLogger $
        T.concat ["Generate first token for user ", getLogin login]
    (token, now) <- liftIO $ generateToken login
    n <- liftIO $ try (executeWithPool pool q (login, password, token, now))
    case n of
        Left e -> errorHandle e
        Right k ->
            if k > 0
                then do
                    liftIO $
                        logInfo hLogger $
                        T.concat ["User with login ", getLogin login, " logged"]
                    return token
                else do
                    liftIO $
                        logError hLogger $
                        T.concat
                            ["User with login ", getLogin login, " cant logged"]
                    throwError $ OtherError "Wrong login or password"
  where
    q =
        "WITH loging_user as (select user_id from users where login = ? and user_password = (crypt(?,user_password))) \
            \insert into tokens (user_id,token,creation_date) values ((select user_id from loging_user),?,?)"
    errorHandle sqlError = do
        let err = E.decodeUtf8 $ sqlErrorMsg sqlError
        liftIO $ logError hLogger err
        throwError DatabaseError
firstToken' hLogger _ _ _ = do
    liftIO $ logError hLogger "Bad login or password parameters"
    throwError $ OtherError "Bad login or password parameters"

profileOnDb ::
       (MonadIO m, MonadError SomeError m)
    => Pool Connection
    -> TokenLifeTime
    -> LoggerHandle IO
    -> Maybe Token
    -> m Profile
profileOnDb _ _ hLogger Nothing = do
    liftIO $
        logError hLogger "User's information not sended. No token parameter"
    throwError $ OtherError "No token parameter"
profileOnDb pool tokenLifeTime hLogger (Just token) = do
    rows <-
        liftIO $
        try
            (queryWithPool
                 pool
                 "select first_name, last_name, avatar from users join tokens using (user_id) where token = ? and (now()- tokens.creation_date) < make_interval(secs => ?)"
                 (TokenProfile token tokenLifeTime))
    case rows of
        Left e -> errorHandle e
        Right [] -> do
            liftIO $
                logError hLogger "User's information not sended. Bad token."
            throwError BadToken
        Right (profile:_) -> do
            liftIO $ logInfo hLogger "User's inforamtion sended"
            return profile
  where
    errorHandle sqlError = do
        let errState = sqlState sqlError
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        liftIO $
            logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        throwError DatabaseError

--------------------------------------------------------------------------------------------------------------------------
authentication ::
       (MonadIO m, MonadError SomeError m)
    => Pool Connection
    -> LoggerHandle IO
    -> Maybe Login
    -> Maybe Password
    -> m Token
authentication _ hLogger _ Nothing = do
    liftIO $ logError hLogger "No login parameter."
    throwError $ OtherError "No login parameter."
authentication _ hLogger Nothing _ = do
    liftIO $ logError hLogger "No password parameter."
    throwError $ OtherError "No password parameter."
authentication pool hLogger (Just login) (Just password) = do
    (token, now) <- liftIO $ generateToken login
    n <- liftIO $ try (executeWithPool pool q (login, password, token, now))
    case n of
        Left e -> errorHandle e
        Right k ->
            if k > 0
                then do
                    liftIO $ logInfo hLogger "User's inforamtion sended"
                    return token
                else do
                    liftIO $ logError hLogger "Bad authorization"
                    throwError BadToken
  where
    q =
        "WITH loging_user as (select user_id from users where login = ? and user_password = (crypt(?,user_password))) \
         \update tokens set token = ?, creation_date = ? where user_id in (select user_id from loging_user)"
    errorHandle sqlError = do
        let errState = sqlState sqlError
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        liftIO $
            logError hLogger $ "Database error " <> T.pack (show errStateInt)
        throwError DatabaseError

-------------------------------------------------------------------------------------------------------
deleteUserFromDb ::
       (MonadIO m, MonadError SomeError m)
    => Pool Connection
    -> TokenLifeTime
    -> LoggerHandle IO
    -> Maybe Token
    -> Maybe Login
    -> m ()
deleteUserFromDb _ _ hLogger _ Nothing = do
    liftIO $ logError hLogger "User not deleted. No login parameter"
    throwError $ OtherError "No login parameter"
deleteUserFromDb pool tokenLifeTime hLogger token (Just login) = do
    ch <- checkAdmin' hLogger pool tokenLifeTime token
    if ch
        then do
            n <-
                liftIO $
                try
                    (executeWithPool
                         pool
                         "delete from users where login = ?"
                         [login])
            case n of
                Left e -> errorHandle e
                Right k ->
                    if k > 0
                        then do
                            liftIO $ logInfo hLogger "User deleted"
                            return ()
                        else do
                            liftIO $ logError hLogger "User not deleted"
                            throwError $ OtherError "User not exist"
        else do
            throwError NotAdmin
  where
    errorHandle sqlError = do
        let err = E.decodeUtf8 $ sqlErrorMsg sqlError
        liftIO $ logError hLogger err
        throwError DatabaseError
