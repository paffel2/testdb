{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.Categories where

import Control.Exception (catch)
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
import PostgreSqlWithPool
    ( executeWithPool
    , queryWithPool
    , query_WithPool
    , returningWithPool
    )
import Types (ListOfCategories(ListOfCategories))

getCategoriesListFromDb ::
       Handle
    -> Pool Connection
    -> Maybe BC.ByteString
    -> IO (Either LBS.ByteString ListOfCategories)
getCategoriesListFromDb hLogger pool pageParam =
    catch
        (do logInfo hLogger "Someone try get list of categories"
            rows <- query_WithPool pool q
            return $ Right (ListOfCategories rows)) $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        logError hLogger err
        return $ Left "Database error"
  where
    pg =
        if isNothing pageParam
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $
                       show $
                       (fromMaybe
                            1
                            (readByteStringToInt (fromMaybe "" pageParam)) -
                        1) *
                       10
                     ]
    q = toQuery $ BC.concat ["select category_name from categories", pg]

createCategoryOnDb ::
       Handle
    -> Pool Connection
    -> Maybe T.Text
    -> Maybe T.Text
    -> IO (Either LBS.ByteString LBS.ByteString)
createCategoryOnDb hLogger _ Nothing _ = do
    logError hLogger "No category_name field"
    return $ Left "No category_name field"
createCategoryOnDb hLogger _ _ Nothing = do
    logError hLogger "No maternal_category_name field"
    return $ Left "No maternal_category_name field"
createCategoryOnDb hLogger pool (Just category'_name) (Just maternal_name) =
    catch
        (do logInfo hLogger "Creating new category"
            if maternal_name == ""
                then do
                    logInfo hLogger "Maternal category is null"
                    rows <-
                        returningWithPool
                            pool
                            q
                            [(category'_name, Nothing :: Maybe T.Text)] :: IO [Only Int]
                    logInfo hLogger "Category created"
                    return $
                        Right $
                        LBS.fromStrict $
                        BC.pack $ show (fromOnly $ Prelude.head rows)
                else do
                    c'_id <-
                        queryWithPool pool check_maternal [maternal_name] :: IO [Only Int]
                    if Prelude.null c'_id
                        then do
                            logError hLogger "Maternal category not exist"
                            return $ Left "Maternal category not exist"
                        else do
                            rows <-
                                returningWithPool
                                    pool
                                    q
                                    [ ( category'_name
                                      , fromOnly $ Prelude.head c'_id)
                                    ] :: IO [Only Int]
                            logInfo hLogger "Category created"
                            return $
                                Right $
                                LBS.fromStrict $
                                BC.pack $ show (fromOnly $ Prelude.head rows)) $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger err
        case errStateInt of
            23505 -> return $ Left "Category already exist"
            _ -> return $ Left "Database error"
  where
    q =
        "insert into categories (category_name, maternal_category) values (?,?) returning category_id"
    check_maternal =
        "select category_id from categories where category_name = ?"

deleteCategoryFromDb ::
       Handle
    -> Pool Connection
    -> Maybe T.Text
    -> IO (Either LBS.ByteString LBS.ByteString)
deleteCategoryFromDb hLogger _ Nothing = do
    logError hLogger "No category_name parametr"
    return $ Left "No category_name parametr"
deleteCategoryFromDb hLogger pool (Just categoryName) =
    catch
        (do n <-
                executeWithPool
                    pool
                    "delete from categories where category_name = ?"
                    [categoryName]
            if n > 0
                then do
                    logInfo hLogger $
                        T.concat ["Category ", categoryName, " deleted"]
                    return $ Right "Category deleted"
                else do
                    logError hLogger $
                        T.concat ["Category ", categoryName, " not exist"]
                    return $ Right "Category not exist") $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        logError hLogger err
        return $ Left "Database error"

editCategoryOnDb ::
       Handle
    -> Pool Connection
    -> Maybe T.Text
    -> Maybe T.Text
    -> Maybe T.Text
    -> IO (Either LBS.ByteString LBS.ByteString)
editCategoryOnDb hLogger _ Nothing _ _ = do
    logError hLogger "No old_name parametr"
    return $ Left "No old_name parametr"
editCategoryOnDb hLogger pool (Just old_name) (Just new'_name) (Just "") =
    catch
        (do logInfo hLogger $
                T.concat
                    ["Update category_name parameter on category ", old_name]
            n <-
                executeWithPool
                    pool
                    "update categories set category_name = ? where category_name = ?"
                    (new'_name, old_name)
            if n > 0
                then do
                    logInfo hLogger $
                        T.concat ["Category ", old_name, " edited"]
                    return $ Right "Category edited"
                else do
                    logError hLogger $
                        T.concat ["Category ", old_name, " not exist"]
                    return $ Left "Category not exist") $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ T.concat [err, T.pack $ show errStateInt]
        case errStateInt of
            23505 -> return $ Left "Category already exist"
            _ -> return $ Left "Database error"
editCategoryOnDb hLogger pool (Just old_name) (Just "") (Just new'_maternal) =
    catch
        (do logInfo hLogger $
                T.concat
                    [ "Update maternal_category parameter on category "
                    , old_name
                    ]
            m_id <-
                queryWithPool
                    pool
                    "select category_id from categories where category_name = ?"
                    [new'_maternal] :: IO [Only Int]
            if Prelude.null m_id
                then do
                    logError hLogger $
                        T.concat
                            ["Maternal category ", new'_maternal, " not exist"]
                    return $ Left "Maternal category not exist"
                else do
                    n <-
                        executeWithPool
                            pool
                            "update categories set maternal_category = ? where category_name = ?"
                            (fromOnly $ Prelude.head m_id, old_name)
                    if n > 0
                        then do
                            logInfo hLogger $
                                T.concat ["Category ", old_name, " edited"]
                            return $ Right "Category edited"
                        else do
                            logError hLogger $
                                T.concat ["Category ", old_name, " not exist"]
                            return $ Left "Category not exist") $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        logError hLogger err
        return $ Left "Database error"
editCategoryOnDb hLogger pool (Just old_name) (Just new'_name) (Just new'_maternal) =
    catch
        (do logInfo hLogger $
                T.concat ["Update all parameters on category ", old_name]
            m_id <-
                queryWithPool
                    pool
                    "select category_id from categories where category_name = ?"
                    [new'_maternal] :: IO [Only Int]
            if Prelude.null m_id
                then do
                    logError hLogger $
                        T.concat
                            ["Maternal category ", new'_maternal, " not exist"]
                    return $ Left "Maternal category not exist"
                else do
                    let q =
                            "update categories set category_name = ?, maternal_category = ? where category_name = ?"
                    n <-
                        executeWithPool
                            pool
                            q
                            (new'_name, fromOnly $ Prelude.head m_id, old_name)
                    if n > 0
                        then do
                            logInfo hLogger $
                                T.concat ["Category ", old_name, " edited"]
                            return $ Right "Category edited"
                        else do
                            logError hLogger $
                                T.concat ["Category ", old_name, " not exist"]
                            return $ Left "Category not exist") $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ T.concat [err, T.pack $ show errStateInt]
        case errStateInt of
            23505 -> return $ Left "Category already exist"
            _ -> return $ Left "Database error"
editCategoryOnDb hLogger _ _ _ _ = do
    logError hLogger "No update parameters"
    return $ Left "No update parameters"