{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.Authors where

import           Control.Monad.Except          (MonadError (catchError, throwError))
import           Data.Pool                     (Pool)
import           Database.PostgreSQL.Simple    (Connection, Only (fromOnly))
import           Databaseoperations.CheckAdmin (checkAdmin)
import           HelpFunction                  (pageToBS, someErrorToInt,
                                                toQuery)
import           PostgreSqlWithPool            (executeWithPool, queryWithPool,
                                                query_WithPool)
import           Types.Authors                 (AuthorLogin,
                                                AuthorsList (AuthorsList),
                                                CreateAuthor (..),
                                                EditAuthor (..))
import           Types.Other                   (MonadIOWithError, Page, SendId,
                                                SomeError (OtherError), Token,
                                                TokenLifeTime)

getAuthorsList ::
       MonadIOWithError m => Pool Connection -> Maybe Page -> m AuthorsList
getAuthorsList pool page = do
    rows <- query_WithPool pool q
    return $ AuthorsList rows
  where
    q =
        toQuery $
        "select author_id, (concat(first_name, ' ', last_name)) as author_name, description from authors join users using (user_id) order by 2" <>
        pageToBS page

editAuthorInDb ::
       MonadIOWithError m
    => Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> EditAuthor
    -> m ()
editAuthorInDb _ _ _ EditAuthor {editAuthorDescription = Nothing} = do
    throwError $ OtherError "Author not edited. No new_description field"
editAuthorInDb _ _ _ EditAuthor {editAuthorId = Nothing} = do
    throwError $ OtherError "Author not edited. No author_id field"
editAuthorInDb _ _ Nothing _ = do
    throwError $ OtherError "Author not edited. No token param"
editAuthorInDb pool tokenLifeTime token editParams = do
    checkAdmin pool tokenLifeTime token
    n <- executeWithPool pool q editParams
    if n > 0
        then return ()
        else throwError $ OtherError "Author not edited"
  where
    q = "update authors set description = ? where author_id = ?"

deleteAuthorInDb ::
       MonadIOWithError m
    => Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> Maybe AuthorLogin
    -> m ()
deleteAuthorInDb _ _ Nothing _ =
    throwError $ OtherError "Author not deleted. No token param"
deleteAuthorInDb _ _ _ Nothing =
    throwError $ OtherError "Author not deleted. No login field"
deleteAuthorInDb pool token_lifetime token (Just authorLogin) =
    catchError
        (do checkAdmin pool token_lifetime token
            _ <- executeWithPool pool q [authorLogin]
            return ()) $ \e ->
        case someErrorToInt e of
            23505 ->
                throwError $
                OtherError "Author not created.Author already exist"
            _ -> throwError e
  where
    q =
        "with u_id as (select user_id from users where login = ?) \
             \delete from authors where user_id = (select * from u_id)"

createAuthorInDb ::
       MonadIOWithError m
    => Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> CreateAuthor
    -> m SendId
createAuthorInDb _ _ Nothing _ =
    throwError $ OtherError "Author not created.No token field"
createAuthorInDb _ _ _ CreateAuthor {createAuthorLogin = Nothing} =
    throwError $ OtherError "Author not created.No login field"
createAuthorInDb _ _ _ CreateAuthor {createAuthorDescription = Nothing} =
    throwError $ OtherError "Author not created.No description field"
createAuthorInDb pool tokenLifetime token createAuthorParams =
    catchError
        (do checkAdmin pool tokenLifetime token
            rows <- queryWithPool pool q createAuthorParams
            if Prelude.null rows
                then throwError $ OtherError "Author not created"
                else return $ fromOnly $ Prelude.head rows) $ \e ->
        case someErrorToInt e of
            23505 -> throwError $ OtherError "Author already exist."
            23502 -> throwError $ OtherError "User with that login not exist."
            _     -> throwError e
  where
    q =
        "insert into authors(user_id,description) values ((select user_id from users where login = ?),?) returning author_id"
