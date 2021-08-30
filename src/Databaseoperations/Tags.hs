{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.Tags where

import Control.Exception (catch)
import Data.ByteString as B (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe, isNothing)
import Data.Pool (Pool)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Database.PostgreSQL.Simple
    ( Connection
    , Only(fromOnly)
    , SqlError(sqlErrorMsg, sqlState)
    )
import HelpFunction (readByteStringToInt, toQuery)
import Logger (Handle, logError, logInfo)
import PostgreSqlWithPool (executeWithPool, queryWithPool, query_WithPool)
import Types (TagsList(TagsList))

createTagInDb ::
       Handle
    -> Pool Connection
    -> Maybe T.Text
    -> IO (Either LBS.ByteString Int)
createTagInDb hLogger _ Nothing = do
    logError hLogger "No tag_name parameter"
    return $ Left "No tag_name parameter"
createTagInDb hLogger pool (Just tag_name') =
    catch
        (do logInfo hLogger "Creating new tag"
            rows <- queryWithPool pool q [tag_name'] :: IO [Only Int]
            if Prelude.null rows
                then do
                    logError hLogger "Tag not created"
                    return $ Left "Tag not created"
                else do
                    logInfo hLogger "Tag created"
                    return $ Right $ fromOnly $ Prelude.head rows) $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ T.concat [err, T.pack $ show errStateInt]
        case errStateInt of
            23505 -> return $ Left "Tag already exist"
            _ -> return $ Left "Database error"
  where
    q =
        toQuery $
        BC.concat ["insert into tags (tag_name) values (?) returning tag_id"]

deleteTagFromDb ::
       Handle
    -> Pool Connection
    -> Maybe T.Text
    -> IO (Either LBS.ByteString LBS.ByteString)
deleteTagFromDb hLogger _ Nothing = do
    logError hLogger "No tag_name parameter"
    return $ Left "No tag_name parameter"
deleteTagFromDb hLogger pool (Just tag_name') =
    catch
        (do logInfo hLogger $ T.concat ["Deleting tag ", tag_name']
            n <- executeWithPool pool q [tag_name']
            if n > 0
                then do
                    logInfo hLogger $ T.concat ["Tag ", tag_name', " deleted"]
                    return $ Right "Tag deleted"
                else do
                    logError hLogger $
                        T.concat ["Tag ", tag_name', " not deleted"]
                    return $ Right "Tag not deleted") $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ T.concat [err, " ", T.pack $ show errStateInt]
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
    q = toQuery $ BC.concat ["select tag_name from tags order by tag_name", pg]
