{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Databaseoperations.CheckAdmin where

import           Control.Monad.Except       (MonadError (throwError))
import           Data.Pool                  (Pool)
import           Database.PostgreSQL.Simple (Connection, Only (Only))
import           PostgreSqlWithPool         (queryWithPool)
import           Types.Other                (MonadIOWithError,
                                             SomeError (BadToken, NotAdmin, OtherError),
                                             Token, TokenLifeTime)

checkAdmin ::
       MonadIOWithError m
    => Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> m ()
checkAdmin _ _ Nothing = do
    throwError $ OtherError "No token parameter"
checkAdmin pool tokenLifetime (Just token) = do
    rows <-
        queryWithPool
            pool
            "select admin_mark from users join tokens using (user_id) where token = ? and ((current_timestamp - tokens.creation_date) < make_interval(secs => ?))"
            (token, tokenLifetime)
    case rows of
        []             -> throwError BadToken
        (Only True:_)  -> return ()
        (Only False:_) -> throwError NotAdmin
