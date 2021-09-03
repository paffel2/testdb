{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.Authors where

import Control.Exception (catch)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
    ( Connection
    , Only(fromOnly)
    , SqlError(sqlState)
    )
import Databaseoperations.CheckAdmin (checkAdmin)
import HelpFunction (readByteStringToInt, toQuery)
import Logger (Handle, logDebug, logError, logInfo)
import PostgreSqlWithPool (executeWithPool, queryWithPool)
import Types (TokenLifeTime)

createAuthorInDb ::
       Handle
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
        (do logDebug hLogger "Creating new author"
            ch <- checkAdmin hLogger pool token_lifetime token'
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    rows <-
                        queryWithPool pool q (author_login, author_description) :: IO [Only Int]
                    if Prelude.null rows
                        then do
                            logError hLogger "Author not created"
                            return $ Left "Author not created"
                        else do
                            logInfo hLogger "New author created"
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
       Handle
    -> Pool Connection
    -> TokenLifeTime
    -> Maybe T.Text
    -> Maybe T.Text
    -> IO (Either LBS.ByteString LBS.ByteString)
deleteAuthorInDb hLogger _ _ Nothing _ = do
    logError hLogger "No token field"
    return $ Left "No token field"
deleteAuthorInDb hLogger _ _ _ Nothing = do
    logError hLogger "No login field"
    return $ Left "No login field"
deleteAuthorInDb hLogger pool token_lifetime token' (Just author_login) =
    catch
        (do logDebug hLogger "Deleting author"
            ch <- checkAdmin hLogger pool token_lifetime token'
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
