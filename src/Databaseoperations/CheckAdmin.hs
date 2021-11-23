{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Databaseoperations.CheckAdmin where

import           Control.Exception
import           Control.Monad.Except
import           Data.Maybe                 (fromMaybe)
import           Data.Pool                  (Pool)
import qualified Data.Text                  as T
import           Database.PostgreSQL.Simple (Connection, Only (fromOnly),
                                             SqlError (sqlState))
import           HelpFunction               (readByteStringToInt)
import           Logger                     (LoggerHandle, logError)
import           PostgreSqlWithPool         (queryWithPool)
import           Types.Other                (SomeError (..), Token,
                                             TokenLifeTime)

checkAdmin ::
       LoggerHandle IO
    -> Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> IO (Bool, SomeError)
checkAdmin hLogger _ _ Nothing = do
    logError hLogger "No token parameter"
    return (False, OtherError "No token parameter")
checkAdmin hLogger pool tokenLifetime (Just token) =
    catch
        (do rows <-
                queryWithPool
                    pool
                    "select admin_mark from users join tokens using (user_id) where token = ? and ((current_timestamp - tokens.creation_date) < make_interval(secs => ?))"
                    (token, tokenLifetime)
            if Prelude.null rows
                then do
                    return (False, BadToken)
                else do
                    let adminMark = fromOnly $ Prelude.head rows
                    if adminMark
                        then return (adminMark, OtherError "")
                        else return (adminMark, NotAdmin)) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ "Database error " <> T.pack (show errStateInt)
        return (False, DatabaseError)

checkAdmin' ::
       (MonadIO m, MonadError SomeError m)
    => LoggerHandle IO
    -> Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> m Bool
checkAdmin' hLogger _ _ Nothing = do
    liftIO $ logError hLogger "No token parameter"
    throwError $ OtherError "No token parameter"
checkAdmin' hLogger pool tokenLifetime (Just token) = do
    rows <-
        liftIO $
        try
            (queryWithPool
                 pool
                 "select admin_mark from users join tokens using (user_id) where token = ? and ((current_timestamp - tokens.creation_date) < make_interval(secs => ?))"
                 (token, tokenLifetime))
    case rows of
        Left (e :: SqlError) -> errorHandle e
        Right []             -> throwError BadToken
        Right (mark:_)       -> return $ fromOnly mark
  where
    errorHandle sqlError = do
        let errState = sqlState sqlError
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        liftIO $
            logError hLogger $ "Database error " <> T.pack (show errStateInt)
        throwError DatabaseError
