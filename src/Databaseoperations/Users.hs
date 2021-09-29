{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.Users where

import Control.Exception (catch)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Time (UTCTime, getCurrentTime)
import Database.PostgreSQL.Simple
    ( Binary(Binary)
    , Connection
    , SqlError(sqlState)
    )
import Databaseoperations.CheckAdmin (checkAdmin)
import HelpFunction (readByteStringToInt, toQuery)
import Logger (Handle, logError, logInfo)
import PostgreSqlWithPool (executeWithPool, queryWithPool)
import Types
    ( Profile
    , TokenLifeTime(token_life_time)
    , TokenProfile(TokenProfile)
    )

generateToken :: T.Text -> IO (T.Text, UTCTime)
generateToken login = do
    now <- getCurrentTime
    let token' = Prelude.filter (`Prelude.notElem` filt) (show now)
    return (T.concat [login, T.pack token'], now)
  where
    filt = " :.-UTC" :: String

authentication ::
       Handle IO
    -> Pool Connection
    -> T.Text
    -> T.Text
    -> IO (Either LBS.ByteString LBS.ByteString)
authentication hLogger pool login password =
    catch
        (do (token', now) <- generateToken login
            n <- executeWithPool pool q (login, password, token', now)
            if n > 0
                then do
                    return $ Right $ LBS.fromStrict $ E.encodeUtf8 token'
                else do
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
       Handle IO
    -> Pool Connection
    -> Maybe T.Text
    -> Maybe T.Text
    -> Maybe T.Text
    -> Maybe T.Text
    -> BC.ByteString
    -> BC.ByteString
    -> LBS.ByteString
    -> IO (Either LBS.ByteString LBS.ByteString)
createUserInDb hLogger _ Nothing _ _ _ _ _ _ = do
    logError hLogger "No login parameter"
    return $ Left "No login parameter"
createUserInDb hLogger _ _ Nothing _ _ _ _ _ = do
    logError hLogger "No password parameter"
    return $ Left "No password parameter"
createUserInDb hLogger _ _ _ Nothing _ _ _ _ = do
    logError hLogger "No first name parameter"
    return $ Left "No first name parameter"
createUserInDb hLogger _ _ _ _ Nothing _ _ _ = do
    logError hLogger "No second name parameter"
    return $ Left "No second name parameter"
createUserInDb hLogger pool (Just login) (Just password) (Just f'_name) (Just l_name) avatar_name avatar_contentType avatar_b = do
    if avatar_name == "" ||
       avatar_contentType == "" ||
       avatar_b == "" || BC.take 5 avatar_contentType /= "image"
        then do
            logError hLogger "Bad image file"
            return $ Left "Bad image file"
        else catch
                 (do now <- getCurrentTime
                     n <-
                         executeWithPool
                             pool
                             q
                             ( avatar_name
                             , Binary avatar_b
                             , avatar_contentType
                             , f'_name
                             , l_name
                             , login
                             , password
                             , now
                             , False)
                     if n > 0
                         then do
                             firstToken hLogger pool login password
                         else do
                             return $ Left "Registration failed") $ \e -> do
                 let errState = sqlState e
                 let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                 logError hLogger $
                     T.concat ["Database error ", T.pack $ show errStateInt]
                 case errStateInt of
                     22001 -> return $ Left "One or more parameter too long"
                     23505 -> return $ Left "User with that login already exist"
                     _ -> return $ Left "Database error in registration"
  where
    q =
        toQuery $
        BC.concat
            [ "with avatar_id as (insert into images (image_name,image_b,content_type) values (?,?,?) returning image_id) "
            , "insert into users (first_name, last_name, avatar, login, user_password, creation_date, admin_mark) "
            , "values (?,?,(select image_id from avatar_id),?,crypt(?,gen_salt('md5')),?,?)"
            ]

deleteUserFromDb ::
       Handle IO
    -> Pool Connection
    -> TokenLifeTime
    -> Maybe T.Text
    -> BC.ByteString
    -> IO (Either LBS.ByteString LBS.ByteString)
deleteUserFromDb hLogger pool token_lifetime token login =
    catch
        (do ch <- checkAdmin hLogger pool token_lifetime token
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    let q = "delete from users where login = ?"
                    n <- executeWithPool pool q [login]
                    if n > 0
                        then do
                            return $
                                Right $
                                LBS.concat
                                    ["User ", LBS.fromStrict login, " deleted"]
                        else return $ Left "User not exist") $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left "Database error"

firstToken ::
       Handle IO
    -> Pool Connection
    -> T.Text
    -> T.Text
    -> IO (Either LBS.ByteString LBS.ByteString)
firstToken hLogger pool login password =
    catch
        (do logInfo hLogger $ T.concat ["Generate first token for user ", login]
            (token', now) <- generateToken login
            n <- executeWithPool pool q (login, password, token', now)
            if n > 0
                then do
                    logInfo hLogger $
                        T.concat ["User with login ", login, " logged"]
                    return $ Right $ LBS.fromStrict $ E.encodeUtf8 token'
                else do
                    logError hLogger $
                        T.concat ["User with login ", login, " cant logged"]
                    return $ Left "Wrong login or password") $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left "Database error"
  where
    q =
        toQuery $
        BC.concat
            [ "WITH loging_user as (select user_id from users where login = ? and user_password = (crypt(?,user_password))) "
            , "insert into tokens (user_id,token,creation_date) values ((select user_id from loging_user),?,?)"
            ]

profileOnDb ::
       Handle IO
    -> Pool Connection
    -> TokenLifeTime
    -> Maybe T.Text
    -> IO (Either LBS.ByteString Profile)
profileOnDb hLogger _ _ Nothing = do
    logError hLogger "No token parameter"
    return $ Left "No token parameter"
profileOnDb hLogger pool token_lifetime (Just token') =
    catch
        (do rows <- queryWithPool pool q (TokenProfile token' token_lifetime)
            if Prelude.null rows
                then do
                    return $ Left "Bad token"
                else do
                    return $ Right $ Prelude.head rows) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        case errStateInt of
            23505 -> return $ Left "Category already exist"
            _ -> return $ Left "Database error"
  where
    q =
        "select first_name, last_name, avatar from users join tokens using (user_id) where token = ? and (now()- tokens.creation_date) < make_interval(secs => ?)"
