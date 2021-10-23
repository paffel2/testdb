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
import           Types.Other                   (ErrorMessage, Token (..),
                                                TokenLifeTime)
import           Types.Users                   (CreateUser (CreateUser, creation_date, first_name, last_name, user_login, user_password),
                                                Login (from_login), Password,
                                                Profile,
                                                TokenProfile (TokenProfile))

generateToken :: Login -> IO (Token, UTCTime)
generateToken login = do
    now <- getCurrentTime
    let token' = Prelude.filter (`Prelude.notElem` filt) (show now)
    return (Token $ T.concat [from_login login, T.pack token'], now)
  where
    filt = " :.-UTC" :: String

authentication ::
       Pool Connection
    -> LoggerHandle IO
    -> Maybe Login
    -> Maybe Password
    -> IO (Either ErrorMessage Token)
authentication _ hLogger _ Nothing = do
    logError hLogger "No login parameter."
    return $ Left "No login parameter."
authentication _ hLogger Nothing _ = do
    logError hLogger "No password parameter."
    return $ Left "No password parameter."
authentication pool hLogger (Just login) (Just password) =
    catch
        (do (token', now) <- generateToken login
            n <- executeWithPool pool q (login, password, token', now)
            if n > 0
                then do
                    logInfo hLogger "User logged"
                    return $ Right token'
                else do
                    logError hLogger "Bad authorization"
                    return $ Left "Wrong login or password") $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left "Database error in authentication"
  where
    q =
        toQuery $
        BC.concat
            [ "WITH loging_user as (select user_id from users where login = ? and user_password = (crypt(?,user_password))) "
            , "update tokens set token = ?, creation_date = ? where user_id in (select user_id from loging_user)"
            ]

createUserInDb ::
       Pool Connection
    -> LoggerHandle IO
    -> CreateUser
    -> IO (Either ErrorMessage Token)
createUserInDb _ hLogger (CreateUser _ _ _ Nothing _ _ _ _ _) = do
    logError hLogger "User not created. No first_name parameter"
    return $ Left "User not created. No first_name parameter"
createUserInDb _ hLogger (CreateUser _ _ _ _ Nothing _ _ _ _) = do
    logError hLogger "User not created. No last_name parameter"
    return $ Left "No last_name parameter"
createUserInDb _ hLogger (CreateUser _ _ _ _ _ Nothing _ _ _) = do
    logError hLogger "User not created. No login parameter"
    return $ Left "No login parameter"
createUserInDb _ hLogger (CreateUser _ _ _ _ _ _ Nothing _ _) = do
    logError hLogger "User not created. No password parameter"
    return $ Left "No password parameter"
createUserInDb pool hLogger c_user@(CreateUser (Just av_file_name) (Just av_con) (Just av_con_type) (Just _) (Just _) (Just _) (Just _) _ _) = do
    if av_file_name == "" ||
       av_con_type == "" ||
       fromBinary av_con == "" || BC.take 5 av_con_type /= "image"
        then do
            logError hLogger "User not created. Bad image file"
            return $ Left "Bad image file"
        else catch
                 (do n <- executeWithPool pool q c_user
                     if n > 0
                         then do
                             logInfo hLogger "New user registered."
                             firstToken
                                 hLogger
                                 pool
                                 (user_login c_user)
                                 (user_password c_user)
                         else do
                             logError hLogger "Registration failed"
                             return $ Left "Registration failed") $ \e -> do
                 let errState = sqlState e
                 let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                 let err = E.decodeUtf8 $ sqlErrorMsg e
                 logError hLogger err
                 case errStateInt of
                     22001 -> return $ Left "One or more parameter too long"
                     23505 -> return $ Left "User with that login already exist"
                     _     -> return $ Left "Database error in registration"
  where
    q =
        toQuery $
        BC.concat
            [ "with avatar_id as (insert into images (image_name,image_b,content_type) values (?,?,?) returning image_id) "
            , "insert into users (first_name, last_name, avatar, login, user_password, creation_date, admin_mark) "
            , "values (?,?,(select image_id from avatar_id),?,crypt(?,gen_salt('md5')),?,?)"
            ]
createUserInDb pool hLogger c_user@(CreateUser Nothing Nothing Nothing (Just _) (Just _) (Just _) (Just _) _ _) =
    catch
        (do logInfo hLogger "Registartion without avatar"
            n <-
                executeWithPool
                    pool
                    q
                    ( first_name c_user
                    , last_name c_user
                    , user_login c_user
                    , user_password c_user
                    , creation_date c_user
                    , False)
            if n > 0
                then do
                    firstToken
                        hLogger
                        pool
                        (user_login c_user)
                        (user_password c_user)
                else do
                    logError hLogger "Registration failed"
                    return $ Left "Registration failed") $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        let err = E.decodeUtf8 $ sqlErrorMsg e
        logError hLogger err
        case errStateInt of
            22001 -> return $ Left "One or more parameter too long"
            23505 -> return $ Left "User with that login already exist"
            _     -> return $ Left "Database error in registration"
  where
    q =
        toQuery $
        BC.concat
            [ "insert into users (first_name, last_name, login, user_password, creation_date, admin_mark) "
            , "values (?,?,?,crypt(?,gen_salt('md5')),?,?)"
            ]
createUserInDb _ hLogger _ = do
    logError hLogger "Unexpected error"
    return $ Left "Unexpected error"

deleteUserFromDb ::
       Pool Connection
    -> LoggerHandle IO
    -> TokenLifeTime
    -> Maybe Token
    -> Maybe Login
    -> IO (Either ErrorMessage ())
deleteUserFromDb _ hLogger _ _ Nothing = do
    logError hLogger "User not deleted. No login parameter"
    return $ Left "No login parameter"
deleteUserFromDb pool hLogger token_lifetime token (Just login) =
    catch
        (do ch <- checkAdmin hLogger pool token_lifetime token
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
                            return $ Left "User not exist") $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        logError hLogger err
        return $ Left "Database error"

firstToken ::
       LoggerHandle IO
    -> Pool Connection
    -> Maybe Login
    -> Maybe Password
    -> IO (Either ErrorMessage Token)
firstToken hLogger pool (Just login') (Just password') =
    catch
        (do logInfo hLogger $
                T.concat ["Generate first token for user ", from_login login']
            (token', now) <- generateToken login'
            n <- executeWithPool pool q (login', password', token', now)
            if n > 0
                then do
                    logInfo hLogger $
                        T.concat
                            ["User with login ", from_login login', " logged"]
                    return $ Right token'
                else do
                    logError hLogger $
                        T.concat
                            [ "User with login "
                            , from_login login'
                            , " cant logged"
                            ]
                    return $ Left "Wrong login or password") $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        logError hLogger err
        return $ Left "Database error"
  where
    q =
        toQuery $
        BC.concat
            [ "WITH loging_user as (select user_id from users where login = ? and user_password = (crypt(?,user_password))) "
            , "insert into tokens (user_id,token,creation_date) values ((select user_id from loging_user),?,?)"
            ]
firstToken hLogger _ _ _ = do
    logError hLogger "Bad login password parameters"
    return $ Left "Bad login password parameters"

profileOnDb ::
       Pool Connection
    -> LoggerHandle IO
    -> TokenLifeTime
    -> Maybe Token
    -> IO (Either ErrorMessage Profile)
profileOnDb _ hLogger _ Nothing = do
    logError hLogger "User's information not sended. No token parameter"
    return $ Left "No token parameter"
profileOnDb pool hLogger token_lifetime (Just token') =
    catch
        (do rows <- queryWithPool pool q (TokenProfile token' token_lifetime)
            if Prelude.null rows
                then do
                    logError hLogger "User's information not sended. Bad token."
                    return $ Left "Bad token"
                else do
                    logInfo hLogger "User's inforamtion sended"
                    return $ Right $ Prelude.head rows) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        case errStateInt of
            23505 -> return $ Left "Category already exist"
            _     -> return $ Left "Database error"
  where
    q =
        "select first_name, last_name, avatar from users join tokens using (user_id) where token = ? and (now()- tokens.creation_date) < make_interval(secs => ?)"
