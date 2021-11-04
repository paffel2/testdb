{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.Tags where

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
import           Types.Other                   (Page (getPage),
                                                SomeError (DatabaseError, OtherError),
                                                Token, TokenLifeTime)
import           Types.Tags                    (EditTag (..),
                                                TagName (getTagName),
                                                TagsList (TagsList))

createTagInDb ::
       Pool Connection
    -> TokenLifeTime
    -> LoggerHandle IO
    -> Maybe Token
    -> Maybe TagName
    -> IO (Either SomeError Int)
createTagInDb _ _ hLogger _ Nothing = do
    logError hLogger "Tag not created. No tag_name parameter"
    return . Left . OtherError $ "No tag_name parameter"
createTagInDb pool tokenLifeTime hLogger token (Just tagName) =
    catch
        (do ch <- checkAdmin hLogger pool tokenLifeTime token
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    rows <-
                        queryWithPool
                            pool
                            "insert into tags (tag_name) values (?) returning tag_id"
                            [tagName] :: IO [Only Int]
                    if Prelude.null rows
                        then do
                            logError hLogger "Tag not created."
                            return . Left . OtherError $ "Tag not created."
                        else do
                            return $ Right $ fromOnly $ Prelude.head rows) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        case errStateInt of
            23505 -> do
                logError hLogger "Tag already exist"
                return . Left . OtherError $ "Tag already exist"
            _ -> do
                logError hLogger $
                    "Database error " <> T.pack (show errStateInt)
                return $ Left DatabaseError

deleteTagFromDb ::
       Pool Connection
    -> TokenLifeTime
    -> LoggerHandle IO
    -> Maybe Token
    -> Maybe TagName
    -> IO (Either SomeError ())
deleteTagFromDb _ _ hLogger _ Nothing = do
    logError hLogger "Tag not deleted. No tag_name parameter"
    return . Left . OtherError $ "Tag not deleted. No tag_name parameter"
deleteTagFromDb pool tokenLifeTime hLogger token (Just tagName) =
    catch
        (do logInfo hLogger $ T.concat ["Deleting tag ", getTagName tagName]
            ch <- checkAdmin hLogger pool tokenLifeTime token
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    n <-
                        executeWithPool
                            pool
                            "delete from tags where tag_name = ?"
                            [tagName]
                    if n > 0
                        then do
                            logInfo hLogger $
                                "Tag " <> getTagName tagName <> " deleted"
                            return $ Right ()
                        else do
                            logError hLogger $
                                "Tag " <> getTagName tagName <> " not deleted"
                            return $ Right ()) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ "Database error " <> T.pack (show errStateInt)
        return $ Left DatabaseError

getTagsListFromDb ::
       Pool Connection
    -> LoggerHandle IO
    -> Maybe Page
    -> IO (Either SomeError TagsList)
getTagsListFromDb pool hLogger page =
    catch
        (do rows <- query_WithPool pool q
            logInfo hLogger "List of tags sended"
            return $ Right $ TagsList rows) $ \e -> do
        let _ = sqlState e
        logError hLogger "Database error"
        return $ Left DatabaseError
  where
    pg =
        if isNothing page
            then " limit 10 offset 0"
            else " limit 10 offset " <>
                 BC.pack (show $ (maybe 1 getPage page - 1) * 10)
    q = toQuery $ "select tag_name from tags order by tag_name" <> pg

editTagInDb ::
       Pool Connection
    -> TokenLifeTime
    -> LoggerHandle IO
    -> Maybe Token
    -> EditTag
    -> IO (Either SomeError ())
editTagInDb _ _ hLogger _ EditTag {editTagNewName = Nothing} = do
    logError hLogger "Tag not edited. No new_tag_name field"
    return . Left . OtherError $ "No new_tag_name field"
editTagInDb _ _ hLogger _ EditTag {editTagOldName = Nothing} = do
    logError hLogger "Tag not edited. No old_tag_name field"
    return . Left . OtherError $ "Tag not edited. No old_tag_name field"
editTagInDb _ _ hLogger Nothing _ = do
    logError hLogger "Tag not edited. No token param"
    return . Left . OtherError $ "No token param"
editTagInDb pool tokenLifetime hLogger token editTagParams@(EditTag (Just newTagName) (Just oldTagName)) =
    catch
        (do ch <- checkAdmin hLogger pool tokenLifetime token
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    n <-
                        executeWithPool
                            pool
                            "update tags set tag_name = ? where tag_name = ?"
                            editTagParams
                    if n > 0
                        then do
                            logInfo hLogger $
                                T.concat
                                    [ "Tag '"
                                    , getTagName oldTagName
                                    , "' renaimed to '"
                                    , getTagName newTagName
                                    , "'"
                                    ]
                            return $ Right ()
                        else do
                            logError hLogger "Tag not exist"
                            return . Left . OtherError $ "Tag not exist") $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ "Database error " <> T.pack (show errStateInt)
        return $ Left DatabaseError
