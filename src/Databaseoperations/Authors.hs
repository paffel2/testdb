{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.Authors where

import Control.Exception (catch)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe, isNothing)
import Data.Pool (Pool)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
    ( Connection
    , Only(fromOnly)
    , SqlError(sqlState)
    )
import Databaseoperations.CheckAdmin (checkAdmin)
import HelpFunction (readByteStringToInt, toQuery)
import Logger (Handle, logError, logInfo)
import PostgreSqlWithPool (executeWithPool, queryWithPool, query_WithPool)
import Types (AuthorsList(AuthorsList), TokenLifeTime)

createAuthorInDb ::
       Handle IO
    -> Pool Connection
    -> TokenLifeTime
    -> Maybe T.Text
    -> Maybe T.Text
    -> Maybe T.Text
    -> IO (Either LBS.ByteString Int)
createAuthorInDb hLogger _ _ Nothing _ _ = do
    logError hLogger "No token field"
    return $ Left "No token field"
createAuthorInDb hLogger _ _ _ Nothing _ = do
    logError hLogger "No login field"
    return $ Left "No login field"
createAuthorInDb hLogger _ _ _ _ Nothing = do
    logError hLogger "No description field"
    return $ Left "No description field"
createAuthorInDb hLogger pool token_lifetime token' (Just author_login) (Just author_description) =
    catch
        (do ch <- checkAdmin hLogger pool token_lifetime token'
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    rows <-
                        queryWithPool pool q (author_login, author_description) :: IO [Only Int]
                    if Prelude.null rows
                        then do
                            return $ Left "Author not created"
                        else do
                            return $ Right $ fromOnly $ Prelude.head rows) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        case errStateInt of
            23505 -> do
                logError hLogger "Author already exist"
                return $ Left "Author already exist"
            _ -> do
                logError hLogger $
                    T.concat ["Database error ", T.pack $ show errStateInt]
                return $ Left "Database error"
  where
    q =
        toQuery $
        BC.concat
            [ "insert into authors(user_id,description) values ((select user_id from users where login = ?),?) returning author_id"
            ]

deleteAuthorInDb ::
       Handle IO
    -> Pool Connection
    -> TokenLifeTime
    -> Maybe T.Text
    -> Maybe T.Text
    -> IO (Either LBS.ByteString LBS.ByteString)
deleteAuthorInDb hLogger _ _ Nothing _ = do
    logError hLogger "No token param"
    return $ Left "No token param"
deleteAuthorInDb hLogger _ _ _ Nothing = do
    logError hLogger "No login field"
    return $ Left "No login field"
deleteAuthorInDb hLogger pool token_lifetime token' (Just author_login) =
    catch
        (do ch <- checkAdmin hLogger pool token_lifetime token'
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    _ <- executeWithPool pool q [author_login]
                    return $ Right "Author deleted") $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        case errStateInt of
            23505 -> do
                logError hLogger "Author already exist"
                return $ Left "Author already exist"
            _ -> do
                logError hLogger $
                    T.concat ["Database error ", T.pack $ show errStateInt]
                return $ Left "Database error"
  where
    q =
        toQuery $
        BC.concat
            [ "with u_id as (select user_id from users where login = ?) "
            , "delete from authors where user_id = (select * from u_id)"
            ]

getAuthorsList ::
       Handle IO
    -> Pool Connection
    -> Maybe BC.ByteString
    -> IO (Either LBS.ByteString AuthorsList)
getAuthorsList hLogger pool page_p' =
    catch
        (do rows <- query_WithPool pool q
            return $ Right $ AuthorsList rows) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left "Database error"
  where
    q =
        toQuery $
        BC.concat
            [ "select author_id, (concat(first_name, ' ', last_name)) as author_name, description from authors join users using (user_id) order by 2"
            , page
            ]
    page =
        if isNothing page_p'
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $
                       show $
                       (fromMaybe 1 (readByteStringToInt (fromMaybe "" page_p')) -
                        1) *
                       10
                     ]

editAuthorInDb ::
       Handle IO
    -> Pool Connection
    -> TokenLifeTime
    -> Maybe T.Text
    -> Maybe Int
    -> Maybe T.Text
    -> IO (Either LBS.ByteString LBS.ByteString)
editAuthorInDb hLogger _ _ _ _ Nothing = do
    logError hLogger "No new_description field"
    return $ Left "No new_description field"
editAuthorInDb hLogger _ _ _ Nothing _ = do
    logError hLogger "No author_id field"
    return $ Left "No author_id field"
editAuthorInDb hLogger _ _ Nothing _ _ = do
    logError hLogger "No token param"
    return $ Left "No token param"
editAuthorInDb hLogger pool token_lifetime token (Just author_id) (Just new_description) =
    catch
        (do ch <- checkAdmin hLogger pool token_lifetime token
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    n <- executeWithPool pool q (new_description, author_id)
                    if n > 0
                        then return $ Right "Author edited"
                        else return $ Left "Author not edited") $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left "Database error"
  where
    q =
        toQuery $
        BC.concat ["update authors set description = ? where author_id = ?"]

checkDb :: Handle IO -> Pool Connection -> IO Bool
checkDb hLogger pool =
    catch
        (do n <- query_WithPool pool "select 1 from news" :: IO [Only Int]
            if n /= []
                then do
                    logInfo hLogger "Database exist"
                    return True
                else do
                    logError hLogger "Database exist"
                    return False) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return False
