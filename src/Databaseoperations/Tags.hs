{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.Tags where

import Control.Exception 
import Data.ByteString as B (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe, isNothing)
import Data.Pool (Pool)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
    ( Connection, Only(fromOnly), SqlError(sqlState) )
import Databaseoperations.CheckAdmin (checkAdmin)
import HelpFunction (readByteStringToInt, toQuery)
import Logger ( logError, logInfo, Handle ) 
import PostgreSqlWithPool (executeWithPool, queryWithPool, query_WithPool)
import Types (TagsList(TagsList), TokenLifeTime)

createTagInDb ::
       Handle
    -> Pool Connection
    -> TokenLifeTime
    -> Maybe T.Text
    -> Maybe T.Text
    -> IO (Either LBS.ByteString Int)
createTagInDb hLogger _ _ _ Nothing = do
    logError hLogger "No tag_name parameter"
    return $ Left "No tag_name parameter"
createTagInDb hLogger pool token_lifetime token' (Just tag_name') =
    catch
        (do logInfo hLogger "Creating new tag"
            ch <- checkAdmin hLogger pool token_lifetime token'
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    rows <- queryWithPool pool q [tag_name'] :: IO [Only Int]
                    if Prelude.null rows
                        then do
                            logError hLogger "Tag not created"
                            return $ Left "Tag not created"
                        else do
                            logInfo hLogger "Tag created"
                            return $ Right $ fromOnly $ Prelude.head rows) $ \e -> do
        --let err = E.decodeUtf8 $ sqlErrorMsg e
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        --logError hLogger $ T.concat ["Database error " ,T.pack $ show errStateInt]
        case errStateInt of
            23505 -> do
                logError hLogger "Tag already exist"
                return $ Left "Tag already exist"
            _ -> do
                logError hLogger $ T.concat ["Database error " ,T.pack $ show errStateInt]
                return $ Left "Database error"
  where
    q =
        toQuery $
        BC.concat ["insert into tags (tag_name) values (?) returning tag_id"]

deleteTagFromDb ::
       Handle
    -> Pool Connection
    -> TokenLifeTime
    -> Maybe T.Text
    -> Maybe T.Text
    -> IO (Either LBS.ByteString LBS.ByteString)
deleteTagFromDb hLogger _ _ _ Nothing = do
    logError hLogger "No tag_name parameter"
    return $ Left "No tag_name parameter"
deleteTagFromDb hLogger pool token_lifetime token' (Just tag_name') =
    catch
        (do logInfo hLogger $ T.concat ["Deleting tag ", tag_name']
            ch <- checkAdmin hLogger pool token_lifetime token'
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    n <- executeWithPool pool q [tag_name']
                    if n > 0
                        then do
                            logInfo hLogger $
                                T.concat ["Tag ", tag_name', " deleted"]
                            return $ Right "Tag deleted"
                        else do
                            logError hLogger $
                                T.concat ["Tag ", tag_name', " not deleted"]
                            return $ Right "Tag not deleted") $ \e -> do
        --let err = E.decodeUtf8 $ sqlErrorMsg e
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ T.concat ["Database error " , T.pack $ show errStateInt]
        return $ Left "Database error"
  where
    q = toQuery $ BC.concat ["delete from tags where tag_name = ?"]

getTagsListFromDb ::
       Handle
    -> Pool Connection
    -> Maybe ByteString
    -> IO (Either LBS.ByteString TagsList)
getTagsListFromDb hLogger pool maybe_page =
    catch
        (do logInfo hLogger "Sending tags list"
            rows <- query_WithPool pool q
            --logDebug hLogger "rows accepted"
            return $ Right $ TagsList rows) $ \e -> do
                        let _ = sqlState e
                        logError hLogger "Database error"
                        return $ Left "Database error"
  where
    pg =
        if isNothing maybe_page
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $
                       show $
                       (fromMaybe
                            1
                            (readByteStringToInt (fromMaybe "" maybe_page)) -
                        1) *
                       10
                     ]
    q = toQuery $ BC.concat ["select tag_name from tags order by tag_name", pg]

{-getTagsListFromDb ::
       Handle
    -> Pool Connection
    -> Maybe ByteString
    -> IO (Either LBS.ByteString TagsList)
getTagsListFromDb hLogger pool maybe_page =
    catch
        (do logInfo hLogger "Sending tags list"
            rows <- query_WithPool pool q
            logDebug hLogger "rows accepted"
            return $ Right $ TagsList rows) $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ T.concat [err, " ", T.pack $ show errStateInt]
        return $ Left "Database error"
  where
    pg =
        if isNothing maybe_page
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $
                       show $
                       (fromMaybe
                            1
                            (readByteStringToInt (fromMaybe "" maybe_page)) -
                        1) *
                       10
                     ]
    q = toQuery $ BC.concat ["select tag_name from tags order by tag_name", pg]-}
{-getTagsListFromDb' ::
       Handle
    -> Pool Connection
    -> Maybe ByteString
    -> IO (Either LBS.ByteString TagsList)
getTagsListFromDb' hLogger pool maybe_page =
    catches
        (do logInfo hLogger "Sending tags list"
            rows <- query_WithPool pool q
            logDebug hLogger "rows accepted"
            return $ Right $ TagsList rows) $ handlerEx hLogger
                {-do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ T.concat [err, " ", T.pack $ show errStateInt]
        return $ Left "Database error"-}
  where
    pg =
        if isNothing maybe_page
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $
                       show $
                       (fromMaybe
                            1
                            (readByteStringToInt (fromMaybe "" maybe_page)) -
                        1) *
                       10
                     ]
    q = toQuery $ BC.concat ["select tag_name from tags order by tag_name", pg]


handlerSqlerror :: Handle -> SqlError -> IO (Either LBS.ByteString a)
handlerSqlerror hLogger e = do
        --print $ sqlState e
        logError hLogger "Database error"
        return $ Left "Database error"
handlerFormatError :: Handle -> FormatError -> IO (Either LBS.ByteString a)
handlerFormatError hLogger e = do
    let err =  fmtMessage e
    logError hLogger $ T.pack err
    return $ Left $ LBS.fromStrict $ BC.pack err

handlerResultError :: Handle -> ResultError -> IO (Either LBS.ByteString a)
handlerResultError hLogger e = do
    let err = errMessage e
    logError hLogger $ T.pack err
    return $ Left $ LBS.fromStrict $ BC.pack err


handlerEx :: Handle -> [Handler (Either LBS.ByteString a)]
handlerEx hLogger = [Handler (handlerSqlerror hLogger),
                     Handler (handlerFormatError hLogger),
                     Handler (handlerResultError hLogger)]-}