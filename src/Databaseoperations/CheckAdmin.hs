{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.CheckAdmin where

import Control.Exception (catch)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
    ( Connection
    , Only(fromOnly)
    , SqlError(sqlState)
    )
import HelpFunction (readByteStringToInt)
import Logger (Handle, logError)
import PostgreSqlWithPool (queryWithPool)
import Types.Other (ErrorMessage, Token, TokenLifeTime(token_life_time))

checkAdmin ::
       Handle IO
    -> Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> IO (Bool, ErrorMessage)
checkAdmin hLogger _ _ Nothing = do
    logError hLogger "No token parameter"
    return (False, "No token parameter")
checkAdmin hLogger pool token_lifetime (Just token') =
    catch
        (do rows <-
                queryWithPool
                    pool
                    "select admin_mark from users join tokens using (user_id) where token = ? and ((current_timestamp - tokens.creation_date) < make_interval(secs => ?))"
                    (token', token_life_time token_lifetime)
            if Prelude.null rows
                then do
                    return (False, "Bad token")
                else do
                    let admin'_mark = fromOnly $ Prelude.head rows
                    if admin'_mark
                        then return (admin'_mark, "")
                        else return (admin'_mark, "Not admin")) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return (False, "Database error")
