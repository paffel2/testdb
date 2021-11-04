{-# LANGUAGE NamedFieldPuns    #-}
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
                                                CreateAuthor (..),
                                                EditAuthor (..))
import           Types.Other                   (Page (getPage), SendId,
                                                SomeError (DatabaseError, OtherError),
                                                Token, TokenLifeTime)

createAuthorInDb ::
       Pool Connection
    -> TokenLifeTime
    -> LoggerHandle IO
    -> Maybe Token
    -> CreateAuthor
    -> IO (Either SomeError SendId)
createAuthorInDb _ _ hLogger Nothing _ = do
    logError hLogger "Author not created.No token field"
    return . Left . OtherError $ "Author not created.No token field"
createAuthorInDb _ _ hLogger _ CreateAuthor {createAuthorLogin = Nothing} = do
    logError hLogger "Author not created.No login field"
    return . Left . OtherError $ "Author not created.No login field"
createAuthorInDb _ _ hLogger _ CreateAuthor {createAuthorDescription = Nothing} = do
    logError hLogger "Author not created.No description field"
    return . Left . OtherError $ "Author not created.No description field"
createAuthorInDb pool tokenLifetime hLogger token createAuthorParams =
    catch
        (do ch <- checkAdmin hLogger pool tokenLifetime token
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    rows <-
                        queryWithPool pool q createAuthorParams :: IO [Only Int]
                    if Prelude.null rows
                        then do
                            logError
                                hLogger
                                "Something wrong. Author not created."
                            return . Left . OtherError $ "Author not created"
                        else do
                            logInfo hLogger "Author created."
                            return $ Right $ fromOnly $ Prelude.head rows) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        case errStateInt of
            23505 -> do
                logError hLogger "Author not created.Author already exist"
                return . Left . OtherError $
                    "Author not created.Author already exist"
            _ -> do
                logError hLogger $
                    "Author not created. Database error " <>
                    T.pack (show errStateInt)
                return $ Left DatabaseError
  where
    q =
        toQuery
            "insert into authors(user_id,description) values ((select user_id from users where login = ?),?) returning author_id"

deleteAuthorInDb ::
       Pool Connection
    -> TokenLifeTime
    -> LoggerHandle IO
    -> Maybe Token
    -> Maybe AuthorLogin
    -> IO (Either SomeError ())
deleteAuthorInDb _ _ hLogger Nothing _ = do
    logError hLogger "Author not deleted. No token param"
    return $ Left $ OtherError "Author not deleted. No token param"
deleteAuthorInDb _ _ hLogger _ Nothing = do
    logError hLogger "Author not deleted. No login field"
    return $ Left $ OtherError "Author not deleted. No login field"
deleteAuthorInDb pool token_lifetime hLogger token (Just authorLogin) =
    catch
        (do ch <- checkAdmin hLogger pool token_lifetime token
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    _ <- executeWithPool pool q [authorLogin]
                    logInfo hLogger "Author deleted"
                    return $ Right ()) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        case errStateInt of
            23505 -> do
                logError hLogger "Author not exist"
                return . Left . OtherError $
                    "Author not created.Author already exist"
            _ -> do
                logError hLogger $
                    "Database error " <> T.pack (show errStateInt)
                return $ Left DatabaseError
  where
    q =
        toQuery
            "with u_id as (select user_id from users where login = ?) \
             \delete from authors where user_id = (select * from u_id)"

getAuthorsList ::
       Pool Connection
    -> LoggerHandle IO
    -> Maybe Page
    -> IO (Either SomeError AuthorsList)
getAuthorsList pool hLogger page =
    catch
        (do rows <- query_WithPool pool q
            logInfo hLogger "List of authors sended"
            return $ Right $ AuthorsList rows) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ "Database error " <> T.pack (show errStateInt)
        return $ Left DatabaseError
  where
    q =
        toQuery $
        "select author_id, (concat(first_name, ' ', last_name)) as author_name, description from authors join users using (user_id) order by 2" <>
        pageBC
    pageBC =
        if isNothing page
            then " limit 10 offset 0"
            else " limit 10 offset " <>
                 BC.pack (show $ (maybe 1 getPage page - 1) * 10)

editAuthorInDb ::
       Pool Connection
    -> TokenLifeTime
    -> LoggerHandle IO
    -> Maybe Token
    -> EditAuthor
    -> IO (Either SomeError ())
editAuthorInDb _ _ hLogger _ EditAuthor {editAuthorDescription = Nothing} = do
    logError hLogger "Author not edited. No new_description field"
    return $ Left $ OtherError "Author not edited. No new_description field"
editAuthorInDb _ _ hLogger _ EditAuthor {editAuthorId = Nothing} = do
    logError hLogger "Author not edited. No author_id field"
    return $ Left $ OtherError "Author not edited. No author_id field"
editAuthorInDb _ _ hLogger Nothing _ = do
    logError hLogger "Author not edited. No token param"
    return $ Left $ OtherError "Author not edited. No token param"
editAuthorInDb pool tokenLifeTime hLogger token editParams =
    catch
        (do ch <- checkAdmin hLogger pool tokenLifeTime token
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    n <- executeWithPool pool q editParams
                    if n > 0
                        then do
                            logInfo hLogger "Author edited"
                            return $ Right ()
                        else do
                            logError hLogger "Author not edited"
                            return $ Left $ OtherError "Author not edited") $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ "Database error " <> T.pack (show errStateInt)
        return $ Left DatabaseError
  where
    q = toQuery "update authors set description = ? where author_id = ?"
