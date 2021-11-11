{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.Categories where

import           Control.Exception             (catch)
import           Data.Maybe                    (fromMaybe)
import           Data.Pool                     (Pool)
import qualified Data.Text                     as T
import           Database.PostgreSQL.Simple    (Connection, Only (..),
                                                SqlError (sqlState))
import           Databaseoperations.CheckAdmin (checkAdmin)
import           HelpFunction                  (numOnlyHead, pageToBS,
                                                readByteStringToInt, toQuery)
import           Logger                        (LoggerHandle, logDebug,
                                                logError, logInfo)
import           PostgreSqlWithPool            (executeWithPool, queryWithPool,
                                                query_WithPool,
                                                returningWithPool)
import           Types.Categories              (CategoryName (..),
                                                CreateCategory (..),
                                                EditCategory (..),
                                                ListOfCategories (ListOfCategories))
import           Types.Other                   (Page, SendId,
                                                SomeError (DatabaseError, OtherError),
                                                Token, TokenLifeTime)

getCategoriesListFromDb ::
       Pool Connection
    -> LoggerHandle IO
    -> Maybe Page
    -> IO (Either SomeError ListOfCategories)
getCategoriesListFromDb pool hLogger pageParam =
    catch
        (do rows <- query_WithPool pool q
            logInfo hLogger "List of categories sended"
            return $ Right (ListOfCategories rows)) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ "Database error " <> T.pack (show errStateInt)
        return $ Left DatabaseError
  where
    q =
        toQuery $
        "select category_name from categories order by category_name" <>
        pageToBS pageParam

createCategoryOnDb ::
       Pool Connection
    -> TokenLifeTime
    -> LoggerHandle IO
    -> Maybe Token
    -> CreateCategory
    -> IO (Either SomeError SendId)
createCategoryOnDb _ _ hLogger _ CreateCategory {createCategroryName = Nothing} = do
    logError hLogger "No category_name field"
    return $ Left $ OtherError "No category_name field"
createCategoryOnDb _ _ hLogger _ CreateCategory {createCategroryMaternalCategory = Nothing} = do
    logError hLogger "No maternal_category_name field"
    return $ Left $ OtherError "No maternal_category_name field"
createCategoryOnDb pool tokenLifeTime hLogger token (CreateCategory (Just categoryName) (Just maternalName)) =
    catch
        (do ch <- checkAdmin hLogger pool tokenLifeTime token
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    if getCategoryName maternalName == ""
                        then createWithoutMaternal
                        else createWithMaternal) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ "Database error " <> T.pack (show errStateInt)
        case errStateInt of
            23505 ->
                return $
                Left $ OtherError "Category not created. Category already exist"
            _ -> return $ Left DatabaseError
  where
    q =
        "insert into categories (category_name, maternal_category) values (?,?) returning category_id"
    checkMaternal = "select category_id from categories where category_name = ?"
    createWithoutMaternal = do
        logDebug hLogger "Maternal category is null"
        rows <-
            returningWithPool pool q [(categoryName, Nothing :: Maybe T.Text)] :: IO [Only Int]
        logInfo hLogger "Category created"
        return $ Right (fromOnly $ Prelude.head rows)
    createWithMaternal = do
        catId <-
            queryWithPool pool checkMaternal [maternalName] :: IO [Only Int]
        if Prelude.null catId
            then do
                logError hLogger "Maternal category not exist"
                return . Left . OtherError $ "Maternal category not exist"
            else do
                rows <-
                    returningWithPool
                        pool
                        q
                        [(categoryName, fromOnly $ Prelude.head catId)]
                logInfo hLogger "Category created"
                return $ Right (fromOnly $ Prelude.head rows)

deleteCategoryFromDb ::
       Pool Connection
    -> TokenLifeTime
    -> LoggerHandle IO
    -> Maybe Token
    -> Maybe CategoryName
    -> IO (Either SomeError ())
deleteCategoryFromDb _ _ hLogger _ Nothing = do
    logError hLogger "Category not deleted.No category_name parametr"
    return $ Left $ OtherError "Category not deleted.No category_name parametr"
deleteCategoryFromDb pool tokenLifeTime hLogger token (Just categoryName) =
    catch
        (do ch <- checkAdmin hLogger pool tokenLifeTime token
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    n <-
                        executeWithPool
                            pool
                            "delete from categories where category_name = ?"
                            [categoryName]
                    if n > 0
                        then do
                            logInfo hLogger "Category deleted."
                            return $ Right ()
                        else do
                            logError hLogger $
                                "Category " <>
                                getCategoryName categoryName <> " not exist"
                            return . Left . OtherError $ "Category not exist") $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ "Database error " <> T.pack (show errStateInt)
        return $ Left DatabaseError

editCategoryOnDb ::
       Pool Connection
    -> TokenLifeTime
    -> LoggerHandle IO
    -> Maybe Token
    -> EditCategory
    -> IO (Either SomeError ())
editCategoryOnDb _ _ hLogger _ EditCategory {editCategoryName = Nothing} = do
    logError hLogger "Category not edited. No old_name parametr"
    return $ Left $ OtherError "Category not edited. No old_name parametr"
editCategoryOnDb _ _ hLogger _ (EditCategory (Just (CategoryName "")) (Just _) (Just _)) = do
    logError hLogger "Empty old name parameter"
    return $ Left $ OtherError "Empty old name parameter"
editCategoryOnDb pool tokenLifeTime hLogger token (EditCategory (Just oldName) (Just newName) (Just (CategoryName ""))) =
    catch
        (do ch <- checkAdmin hLogger pool tokenLifeTime token
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    logInfo hLogger $
                        "Update category_name parameter on category " <>
                        getCategoryName oldName
                    n <-
                        executeWithPool
                            pool
                            "update categories set category_name = ? where category_name = ?"
                            (newName, oldName)
                    if n > 0
                        then do
                            logInfo hLogger "Category edited."
                            return $ Right ()
                        else do
                            logError hLogger $
                                "Category " <>
                                getCategoryName oldName <> " not exist"
                            return . Left . OtherError $ "Category not exist") $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ "Database error " <> T.pack (show errStateInt)
        case errStateInt of
            23505 -> return . Left . OtherError $ "Category already exist"
            _     -> return $ Left DatabaseError
editCategoryOnDb pool tokenLifeTime hLogger token (EditCategory (Just oldName) (Just (CategoryName "")) (Just newMaternal)) =
    catch
        (do ch <- checkAdmin hLogger pool tokenLifeTime token
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    maternalId <-
                        queryWithPool
                            pool
                            "select category_id from categories where category_name = ?"
                            [newMaternal] :: IO [Only Int]
                    n <-
                        executeWithPool
                            pool
                            "update categories set maternal_category = ? where category_name = ?"
                            (fromOnly $ numOnlyHead maternalId, oldName)
                    if n > 0
                        then do
                            logInfo hLogger "Category edited."
                            return $ Right ()
                        else do
                            logError hLogger $
                                "Category " <>
                                getCategoryName oldName <> " not exist"
                            return . Left . OtherError $ "Category not exist") $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ "Database error " <> T.pack (show errStateInt)
        case errStateInt of
            23503 -> return $ Left $ OtherError "Maternal category not exist"
            _     -> return $ Left DatabaseError
editCategoryOnDb pool tokenLifeTime hLogger token (EditCategory (Just oldName) (Just newName) (Just newMaternal)) =
    catch
        (do ch <- checkAdmin hLogger pool tokenLifeTime token
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    logInfo hLogger $
                        "Update all parameters on category " <>
                        getCategoryName oldName
                    maternalId <-
                        queryWithPool
                            pool
                            "select category_id from categories where category_name = ?"
                            [newMaternal] :: IO [Only Int]
                    n <-
                        executeWithPool
                            pool
                            "update categories set category_name = ?, maternal_category = ? where category_name = ?"
                            ( newName
                            , fromOnly $ numOnlyHead maternalId
                            , oldName)
                    if n > 0
                        then do
                            logInfo hLogger "Category edited."
                            return $ Right ()
                        else do
                            logError hLogger $
                                "Category " <>
                                getCategoryName oldName <> " not exist"
                            return . Left . OtherError $ "Category not exist") $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ "Database error " <> T.pack (show errStateInt)
        case errStateInt of
            23505 -> return . Left . OtherError $ "Category already exist"
            23503 -> return . Left . OtherError $ "Maternal category not exist"
            _     -> return $ Left DatabaseError
editCategoryOnDb _ _ hLogger _ _ = do
    logError hLogger "No update parameters"
    return $ Left $ OtherError "No update parameters"
