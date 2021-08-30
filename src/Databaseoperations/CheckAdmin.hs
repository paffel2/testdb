{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.CheckAdmin where

import Control.Exception (catch)
import qualified Data.ByteString.Lazy as LBS
import Data.Pool (Pool)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Database.PostgreSQL.Simple
    ( Connection
    , Only(fromOnly)
    , SqlError(sqlErrorMsg)
    )
import Logger (Handle, logError)
import PostgreSqlWithPool (queryWithPool)
import Types (TokenLifeTime)

checkAdmin ::
       Handle
    -> Pool Connection
    -> TokenLifeTime
    -> Maybe T.Text
    -> IO (Bool, LBS.ByteString)
checkAdmin hLogger _ _ Nothing = do
    logError hLogger "No token parameter"
    return (False, "No token parameter")
checkAdmin hLogger pool token_liferime (Just token') =
    catch
        (do rows <-
                queryWithPool
                    pool
                    "select admin_mark from users join tokens using (user_id) where token = ? and ((current_timestamp - tokens.creation_date) < make_interval(secs => ?))"
                    (token', token_liferime)
            if Prelude.null rows
                then do
                    logError hLogger "Bad token"
                    return (False, "Bad token")
                else do
                    let admin'_mark = fromOnly $ Prelude.head rows
                    if admin'_mark
                        then return (admin'_mark, "")
                        else return (admin'_mark, "Not admin")) $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        logError hLogger err
        return (False, "Database error")
