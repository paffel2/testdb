{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.Users where

import           Control.Exception             (catch)
import qualified Data.ByteString.Char8         as BC
import           Data.Maybe                    (fromMaybe)
import           Data.Pool                     (Pool)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import           Data.Time                     (UTCTime, getCurrentTime)
import           Database.PostgreSQL.Simple    (Binary (fromBinary), Connection,
                                                SqlError (sqlErrorMsg, sqlState))

import           Databaseoperations.CheckAdmin (checkAdmin)
import           HelpFunction                  (readByteStringToInt, toQuery)
import           Logger                        (LoggerHandle, logError, logInfo)
import           PostgreSqlWithPool            (executeWithPool, queryWithPool)
import           Types.Other                   (SomeError (BadToken, DatabaseError, OtherError),
                                                Token (Token), TokenLifeTime)
import           Types.Users                   (CreateUser (..),
                                                Login (getLogin), Password,
                                                Profile,
                                                TokenProfile (TokenProfile))

generateToken :: Login -> IO (Token, UTCTime)
generateToken login = do
    now <- getCurrentTime
    let token = Prelude.filter (`Prelude.notElem` filt) (show now)
    return (Token $ T.concat [getLogin login, T.pack token], now)
  where
    filt = " :.-UTC" :: String

authentication ::
       Pool Connection
    -> LoggerHandle IO
    -> Maybe Login
    -> Maybe Password
    -> IO (Either SomeError Token)
authentication _ hLogger _ Nothing = do
    logError hLogger "No login parameter."
    return . Left . OtherError $ "No login parameter."
authentication _ hLogger Nothing _ = do
    logError hLogger "No password parameter."
    return . Left . OtherError $ "No password parameter."
authentication pool hLogger (Just login) (Just password) =
    catch
        (do (token, now) <- generateToken login
            n <- executeWithPool pool q (login, password, token, now)
            if n > 0
                then do
                    logInfo hLogger "User logged"
                    return $ Right token
                else do
                    logError hLogger "Bad authorization"
                    return . Left . OtherError $ "Wrong login or password") $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ "Database error " <> T.pack (show errStateInt)
        return $ Left DatabaseError
  where
    q =
        "WITH loging_user as (select user_id from users where login = ? and user_password = (crypt(?,user_password))) \
         \update tokens set token = ?, creation_date = ? where user_id in (select user_id from loging_user)"

createUserInDb ::
       Pool Connection
    -> LoggerHandle IO
    -> CreateUser
    -> IO (Either SomeError Token)
createUserInDb _ hLogger CreateUser {cuFirstName = Nothing} = do
    logError hLogger "User not created. No first_name parameter"
    return . Left . OtherError $ "User not created. No first_name parameter"
createUserInDb _ hLogger CreateUser {cuLastName = Nothing} = do
    logError hLogger "User not created. No last_name parameter"
    return . Left . OtherError $ "No last_name parameter"
createUserInDb _ hLogger CreateUser {cuUserLogin = Nothing} = do
    logError hLogger "User not created. No login parameter"
    return . Left . OtherError $ "No login parameter"
createUserInDb _ hLogger CreateUser {cuUserPassword = Nothing} = do
    logError hLogger "User not created. No password parameter"
    return . Left . OtherError $ "No password parameter"
createUserInDb pool hLogger createUser@(CreateUser (Just avFileName) (Just avContent) (Just avContentType) (Just _) (Just _) (Just _) (Just _) _) = do
    if avFileName == "" ||
       avContentType == "" ||
       fromBinary avContent == "" || BC.take 5 avContentType /= "image"
        then do
            logError hLogger "User not created. Bad image file"
            return . Left . OtherError $ "Bad image file"
        else catch
                 (do n <- executeWithPool pool q createUser
                     if n > 0
                         then do
                             logInfo hLogger "New user registered."
                             firstToken
                                 hLogger
                                 pool
                                 (cuUserLogin createUser)
                                 (cuUserPassword createUser)
                         else do
                             logError hLogger "Registration failed"
                             return . Left . OtherError $ "Registration failed") $ \e -> do
                 let errState = sqlState e
                 let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                 let err = E.decodeUtf8 $ sqlErrorMsg e
                 logError hLogger err
                 case errStateInt of
                     22001 ->
                         return . Left . OtherError $
                         "One or more parameter too long"
                     23505 ->
                         return . Left . OtherError $
                         "User with that login already exist"
                     _ -> return $ Left DatabaseError
  where
    q =
        "with avatar_id as (insert into images (image_name,image_b,content_type) values (?,?,?) returning image_id) \
         \insert into users (first_name, last_name, avatar, login, user_password, creation_date, admin_mark) \
         \values (?,?,(select image_id from avatar_id),?,crypt(?,gen_salt('md5')),now(),?)"
createUserInDb pool hLogger (CreateUser Nothing Nothing Nothing (Just firstName) (Just lastName) (Just login) (Just password) _) =
    catch
        (do logInfo hLogger "Registartion without avatar"
            n <-
                executeWithPool
                    pool
                    q
                    (firstName, lastName, login, password, False)
            if n > 0
                then do
                    firstToken hLogger pool (Just login) (Just password)
                else do
                    logError hLogger "Registration failed"
                    return . Left . OtherError $ "Registration failed") $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        let err = E.decodeUtf8 $ sqlErrorMsg e
        logError hLogger err
        case errStateInt of
            22001 ->
                return . Left . OtherError $ "One or more parameter too long"
            23505 ->
                return . Left . OtherError $
                "User with that login already exist"
            _ -> return $ Left DatabaseError
  where
    q =
        "insert into users (first_name, last_name, login, user_password, creation_date, admin_mark) \
             \values (?,?,?,crypt(?,gen_salt('md5')),now(),?)"
createUserInDb _ hLogger _ = do
    logError hLogger "Unexpected error"
    return . Left . OtherError $ "Unexpected error"

deleteUserFromDb ::
       Pool Connection
    -> TokenLifeTime
    -> LoggerHandle IO
    -> Maybe Token
    -> Maybe Login
    -> IO (Either SomeError ())
deleteUserFromDb _ _ hLogger _ Nothing = do
    logError hLogger "User not deleted. No login parameter"
    return . Left . OtherError $ "No login parameter"
deleteUserFromDb pool tokenLifeTime hLogger token (Just login) =
    catch
        (do ch <- checkAdmin hLogger pool tokenLifeTime token
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    let q = "delete from users where login = ?"
                    n <- executeWithPool pool q [login]
                    if n > 0
                        then do
                            logInfo hLogger "User deleted"
                            return $ Right ()
                        else do
                            logError hLogger "User not deleted"
                            return . Left . OtherError $ "User not exist") $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        logError hLogger err
        return $ Left DatabaseError

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
        toQuery
            "WITH loging_user as (select user_id from users where login = ? and user_password = (crypt(?,user_password))) \
            \insert into tokens (user_id,token,creation_date) values ((select user_id from loging_user),?,?)"
firstToken hLogger _ _ _ = do
    logError hLogger "Bad login or password parameters"
    return . Left . OtherError $ "Bad login or password parameters"

profileOnDb ::
       Pool Connection
    -> TokenLifeTime
    -> LoggerHandle IO
    -> Maybe Token
    -> IO (Either SomeError Profile)
profileOnDb _ _ hLogger Nothing = do
    logError hLogger "User's information not sended. No token parameter"
    return . Left . OtherError $ "No token parameter"
profileOnDb pool tokenLifeTime hLogger (Just token) =
    catch
        (do rows <- queryWithPool pool q (TokenProfile token tokenLifeTime)
            if Prelude.null rows
                then do
                    logError hLogger "User's information not sended. Bad token."
                    return $ Left BadToken
                else do
                    logInfo hLogger "User's inforamtion sended"
                    return $ Right $ Prelude.head rows) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left DatabaseError
  where
    q =
        "select first_name, last_name, avatar from users join tokens using (user_id) where token = ? and (now()- tokens.creation_date) < make_interval(secs => ?)"
