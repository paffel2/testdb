{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Databaseoperations.CheckAdmin where

import           Control.Monad.Except       (MonadError (throwError), MonadIO)
import           Data.Pool                  (Pool)
import           Database.PostgreSQL.Simple (Connection, Only (Only))
import           PostgreSqlWithPool         (queryWithPoolNew)
import           Types.Other                (SomeError (BadToken, NotAdmin, OtherError),
                                             Token, TokenLifeTime)

checkAdmin ::
       (MonadIO m, MonadError SomeError m)
    => Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> m ()
checkAdmin _ _ Nothing = do
    throwError $ OtherError "No token parameter"
checkAdmin pool tokenLifetime (Just token) = do
    rows <-
        queryWithPoolNew
            pool
            "select admin_mark from users join tokens using (user_id) where token = ? and ((current_timestamp - tokens.creation_date) < make_interval(secs => ?))"
            (token, tokenLifetime)
    case rows of
        []             -> throwError BadToken
        (Only True:_)  -> return ()
        (Only False:_) -> throwError NotAdmin
