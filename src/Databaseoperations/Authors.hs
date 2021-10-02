{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.Authors where

import Control.Exception (catch)
import qualified Data.ByteString.Char8 as BC
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
import Logger (Handle, logError)
import PostgreSqlWithPool (executeWithPool, queryWithPool, query_WithPool)
import Types.Authors
    ( AuthorLogin
    , AuthorsList(AuthorsList)
    , CreateAuthor(CreateAuthor)
    , EditAuthor(EditAuthor)
    )
import Types.Other
    ( ErrorMessage
    , Page(from_page)
    , SendId
    , SuccessMessage
    , Token
    , TokenLifeTime
    )

createAuthorInDb ::
       Handle IO
    -> Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> CreateAuthor
    -> IO (Either ErrorMessage SendId)
createAuthorInDb hLogger _ _ Nothing _ = do
    logError hLogger "No token field"
    return $ Left "No token field"
createAuthorInDb hLogger _ _ _ (CreateAuthor Nothing _) = do
    logError hLogger "No login field"
    return $ Left "No login field"
createAuthorInDb hLogger _ _ _ (CreateAuthor _ Nothing) = do
    logError hLogger "No description field"
    return $ Left "No description field"
createAuthorInDb hLogger pool token_lifetime token' create_author_params =
    catch
        (do ch <- checkAdmin hLogger pool token_lifetime token'
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    rows <-
                        queryWithPool pool q create_author_params :: IO [Only Int]
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
    -> Maybe Token
    -> Maybe AuthorLogin
    -> IO (Either ErrorMessage SuccessMessage)
deleteAuthorInDb hLogger _ _ Nothing _ = do
    logError hLogger "No token param"
    return $ Left "No token param"
deleteAuthorInDb hLogger _ _ _ Nothing = do
    logError hLogger "No login field"
    return $ Left "No login field"
deleteAuthorInDb hLogger pool token_lifetime token' (Just author_login') =
    catch
        (do ch <- checkAdmin hLogger pool token_lifetime token'
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    _ <- executeWithPool pool q [author_login']
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
    -> Maybe Page
    -> IO (Either ErrorMessage AuthorsList)
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
            , page'
            ]
    page' =
        if isNothing page_p'
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $
                       show $
                       (fromMaybe
                            1
                            (readByteStringToInt (maybe "" from_page page_p')) -
                        1) *
                       10
                     ]

editAuthorInDb ::
       Handle IO
    -> Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> EditAuthor
    -> IO (Either ErrorMessage SuccessMessage)
editAuthorInDb hLogger _ _ _ (EditAuthor Nothing _) = do
    logError hLogger "No new_description field"
    return $ Left "No new_description field"
editAuthorInDb hLogger _ _ _ (EditAuthor _ Nothing) = do
    logError hLogger "No author_id field"
    return $ Left "No author_id field"
editAuthorInDb hLogger _ _ Nothing _ = do
    logError hLogger "No token param"
    return $ Left "No token param"
editAuthorInDb hLogger pool token_lifetime token edit_params =
    catch
        (do ch <- checkAdmin hLogger pool token_lifetime token
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    n <- executeWithPool pool q edit_params
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
