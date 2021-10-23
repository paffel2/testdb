{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.Authors where

import           Control.Exception             (catch)
import qualified Data.ByteString.Char8         as BC
import           Data.Maybe                    (fromMaybe, isNothing)
import           Data.Pool                     (Pool)
import qualified Data.Text                     as T
import           Database.PostgreSQL.Simple    (Connection, Only (fromOnly),
                                                SqlError (sqlState))
import           Databaseoperations.CheckAdmin (checkAdmin)
import           HelpFunction                  (readByteStringToInt, toQuery)
import           Logger                        (LoggerHandle, logError, logInfo)
import           PostgreSqlWithPool            (executeWithPool, queryWithPool,
                                                query_WithPool)
import           Types.Authors                 (AuthorLogin,
                                                AuthorsList (AuthorsList),
                                                CreateAuthor (CreateAuthor),
                                                EditAuthor (EditAuthor))
import           Types.Other                   (ErrorMessage, Page (from_page),
                                                SendId, Token, TokenLifeTime)

createAuthorInDb ::
       Pool Connection
    -> LoggerHandle IO
    -> TokenLifeTime
    -> Maybe Token
    -> CreateAuthor
    -> IO (Either ErrorMessage SendId)
createAuthorInDb _ hLogger _ Nothing _ = do
    logError hLogger "Author not created.No token field"
    return $ Left "Author not created.No token field"
createAuthorInDb _ hLogger _ _ (CreateAuthor Nothing _) = do
    logError hLogger "Author not created.No login field"
    return $ Left "Author not created.No login field"
createAuthorInDb _ hLogger _ _ (CreateAuthor _ Nothing) = do
    logError hLogger "Author not created.No description field"
    return $ Left "Author not created.No description field"
createAuthorInDb pool hLogger token_lifetime token' create_author_params =
    catch
        (do ch <- checkAdmin hLogger pool token_lifetime token'
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    rows <-
                        queryWithPool pool q create_author_params :: IO [Only Int]
                    if Prelude.null rows
                        then do
                            logError
                                hLogger
                                "Something wrong. Author not created."
                            return $ Left "Author not created"
                        else do
                            logInfo hLogger "Author created."
                            return $ Right $ fromOnly $ Prelude.head rows) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        case errStateInt of
            23505 -> do
                logError hLogger "Author not created.Author already exist"
                return $ Left "Author not created.Author already exist"
            _ -> do
                logError hLogger $
                    T.concat
                        [ "Author not created. Database error "
                        , T.pack $ show errStateInt
                        ]
                return $ Left "Database error"
  where
    q =
        toQuery $
        BC.concat
            [ "insert into authors(user_id,description) values ((select user_id from users where login = ?),?) returning author_id"
            ]

deleteAuthorInDb ::
       Pool Connection
    -> LoggerHandle IO
    -> TokenLifeTime
    -> Maybe Token
    -> Maybe AuthorLogin
    -> IO (Either ErrorMessage ())
deleteAuthorInDb _ hLogger _ Nothing _ = do
    logError hLogger "Author not deleted. No token param"
    return $ Left "Author not deleted. No token param"
deleteAuthorInDb _ hLogger _ _ Nothing = do
    logError hLogger "Author not deleted. No login field"
    return $ Left "Author not deleted. No login field"
deleteAuthorInDb pool hLogger token_lifetime token' (Just author_login') =
    catch
        (do ch <- checkAdmin hLogger pool token_lifetime token'
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    _ <- executeWithPool pool q [author_login']
                    logInfo hLogger "Author deleted"
                    return $ Right ()) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        case errStateInt of
            23505 -> do
                logError hLogger "Author not exist"
                return $ Left "Author not exist"
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
       Pool Connection
    -> LoggerHandle IO
    -> Maybe Page
    -> IO (Either ErrorMessage AuthorsList)
getAuthorsList pool hLogger page_p' =
    catch
        (do rows <- query_WithPool pool q
            logInfo hLogger "List of authors sended"
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
                     , BC.pack $ show $ (maybe 1 from_page page_p' - 1) * 10
                     ]

editAuthorInDb ::
       Pool Connection
    -> LoggerHandle IO
    -> TokenLifeTime
    -> Maybe Token
    -> EditAuthor
    -> IO (Either ErrorMessage ())
editAuthorInDb _ hLogger _ _ (EditAuthor Nothing _) = do
    logError hLogger "Author not edited. No new_description field"
    return $ Left "Author not edited. No new_description field"
editAuthorInDb _ hLogger _ _ (EditAuthor _ Nothing) = do
    logError hLogger "Author not edited. No author_id field"
    return $ Left "Author not edited. No author_id field"
editAuthorInDb _ hLogger _ Nothing _ = do
    logError hLogger "Author not edited. No token param"
    return $ Left "Author not edited. No token param"
editAuthorInDb pool hLogger token_lifetime token edit_params =
    catch
        (do ch <- checkAdmin hLogger pool token_lifetime token
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    n <- executeWithPool pool q edit_params
                    if n > 0
                        then do
                            logInfo hLogger "Author edited"
                            return $ Right ()
                        else do
                            logError hLogger "Author not edited"
                            return $ Left "Author not edited") $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left "Database error"
  where
    q =
        toQuery $
        BC.concat ["update authors set description = ? where author_id = ?"]
