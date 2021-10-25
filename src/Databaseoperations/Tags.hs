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
import           Types.Other                   (Page (from_page),
                                                SomeError (DatabaseError, OtherError),
                                                Token, TokenLifeTime)
import           Types.Tags                    (EditTag (EditTag),
                                                TagName (from_tag_name),
                                                TagsList (TagsList))

createTagInDb ::
       Pool Connection
    -> LoggerHandle IO
    -> TokenLifeTime
    -> Maybe Token
    -> Maybe TagName
    -> IO (Either SomeError Int)
createTagInDb _ hLogger _ _ Nothing = do
    logError hLogger "Tag not created. No tag_name parameter"
    return . Left . OtherError $ "No tag_name parameter"
createTagInDb pool hLogger token_lifetime token' (Just tag_name') =
    catch
        (do ch <- checkAdmin hLogger pool token_lifetime token'
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    rows <- queryWithPool pool q [tag_name'] :: IO [Only Int]
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
                    T.concat ["Database error ", T.pack $ show errStateInt]
                return $ Left DatabaseError
  where
    q =
        toQuery $
        BC.concat ["insert into tags (tag_name) values (?) returning tag_id"]

deleteTagFromDb ::
       Pool Connection
    -> LoggerHandle IO
    -> TokenLifeTime
    -> Maybe Token
    -> Maybe TagName
    -> IO (Either SomeError ())
deleteTagFromDb _ hLogger _ _ Nothing = do
    logError hLogger "Tag not deleted. No tag_name parameter"
    return . Left . OtherError $ "Tag not deleted. No tag_name parameter"
deleteTagFromDb pool hLogger token_lifetime token' (Just tag_name') =
    catch
        (do logInfo hLogger $
                T.concat ["Deleting tag ", from_tag_name tag_name']
            ch <- checkAdmin hLogger pool token_lifetime token'
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    n <- executeWithPool pool q [tag_name']
                    if n > 0
                        then do
                            logInfo hLogger $
                                T.concat
                                    [ "Tag "
                                    , from_tag_name tag_name'
                                    , " deleted"
                                    ]
                            return $ Right ()
                        else do
                            logError hLogger $
                                T.concat
                                    [ "Tag "
                                    , from_tag_name tag_name'
                                    , " not deleted"
                                    ]
                            return $ Right ()) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left DatabaseError
  where
    q = toQuery $ BC.concat ["delete from tags where tag_name = ?"]

getTagsListFromDb ::
       Pool Connection
    -> LoggerHandle IO
    -> Maybe Page
    -> IO (Either SomeError TagsList)
getTagsListFromDb pool hLogger maybe_page =
    catch
        (do rows <- query_WithPool pool q
            logInfo hLogger "List of tags sended"
            return $ Right $ TagsList rows) $ \e -> do
        let _ = sqlState e
        logError hLogger "Database error"
        return $ Left DatabaseError
  where
    pg =
        if isNothing maybe_page
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $ show $ (maybe 1 from_page maybe_page - 1) * 10
                     ]
    q = toQuery $ BC.concat ["select tag_name from tags order by tag_name", pg]

editTagInDb ::
       Pool Connection
    -> LoggerHandle IO
    -> TokenLifeTime
    -> Maybe Token
    -> EditTag
    -> IO (Either SomeError ())
editTagInDb _ hLogger _ _ (EditTag Nothing _) = do
    logError hLogger "Tag not edited. No new_tag_name field"
    return . Left . OtherError $ "No new_tag_name field"
editTagInDb _ hLogger _ _ (EditTag _ Nothing) = do
    logError hLogger "Tag not edited. No old_tag_name field"
    return . Left . OtherError $ "Tag not edited. No old_tag_name field"
editTagInDb _ hLogger _ Nothing _ = do
    logError hLogger "Tag not edited. No token param"
    return . Left . OtherError $ "No token param"
editTagInDb pool hLogger token_lifetime token edit_tag_params@(EditTag (Just new_tag_name) (Just old_tag_name)) =
    catch
        (do ch <- checkAdmin hLogger pool token_lifetime token
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    n <- executeWithPool pool q edit_tag_params
                    if n > 0
                        then do
                            logInfo hLogger $
                                T.concat
                                    [ "Tag '"
                                    , from_tag_name old_tag_name
                                    , "' renaimed to '"
                                    , from_tag_name new_tag_name
                                    , "'"
                                    ]
                            return $ Right ()
                        else do
                            logError hLogger "Tag not exist"
                            return . Left . OtherError $ "Tag not exist") $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left DatabaseError
  where
    q = toQuery $ BC.concat ["update tags set tag_name = ? where tag_name = ?"]
