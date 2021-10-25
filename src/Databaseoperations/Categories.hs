{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.Categories where

import           Control.Exception             (catch)
import qualified Data.ByteString.Char8         as BC
import           Data.Maybe                    (fromMaybe, isNothing)
import           Data.Pool                     (Pool)
import qualified Data.Text                     as T
import           Database.PostgreSQL.Simple    (Connection, Only (..),
                                                SqlError (sqlState))
import           Databaseoperations.CheckAdmin (checkAdmin)
import           HelpFunction                  (readByteStringToInt, toQuery)
import           Logger                        (LoggerHandle, logDebug,
                                                logError, logInfo)
import           PostgreSqlWithPool            (executeWithPool, queryWithPool,
                                                query_WithPool,
                                                returningWithPool)
import           Types.Categories              (CategoryName (..),
                                                CreateCategory (CreateCategory),
                                                EditCategory (EditCategory),
                                                ListOfCategories (ListOfCategories))
import           Types.Other                   (Page (from_page), SendId,
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
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left DatabaseError
  where
    pg =
        if isNothing pageParam
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $ show $ (maybe 1 from_page pageParam - 1) * 10
                     ]
    q =
        toQuery $
        BC.concat
            ["select category_name from categories order by category_name", pg]

createCategoryOnDb ::
       Pool Connection
    -> TokenLifeTime
    -> LoggerHandle IO
    -> Maybe Token
    -> CreateCategory
    -> IO (Either SomeError SendId)
createCategoryOnDb _ _ hLogger _ (CreateCategory Nothing _) = do
    logError hLogger "No category_name field"
    return $ Left $ OtherError "No category_name field"
createCategoryOnDb _ _ hLogger _ (CreateCategory _ Nothing) = do
    logError hLogger "No maternal_category_name field"
    return $ Left $ OtherError "No maternal_category_name field"
createCategoryOnDb pool token_lifetime hLogger token' (CreateCategory (Just category'_name) (Just maternal_name)) =
    catch
        (do ch <- checkAdmin hLogger pool token_lifetime token'
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    if from_category_name maternal_name == ""
                        then createWithoutMaternal
                        else createWithMaternal) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        case errStateInt of
            23505 ->
                return $
                Left $ OtherError "Category not created. Category already exist"
            _ -> return $ Left DatabaseError
  where
    q =
        "insert into categories (category_name, maternal_category) values (?,?) returning category_id"
    check_maternal =
        "select category_id from categories where category_name = ?"
    createWithoutMaternal = do
        logDebug hLogger "Maternal category is null"
        rows <-
            returningWithPool pool q [(category'_name, Nothing :: Maybe T.Text)] :: IO [Only Int]
        logInfo hLogger "Category created"
        return $ Right (fromOnly $ Prelude.head rows)
    createWithMaternal = do
        c'_id <-
            queryWithPool pool check_maternal [maternal_name] :: IO [Only Int]
        if Prelude.null c'_id
            then do
                logError hLogger "Maternal category not exist"
                return . Left . OtherError $ "Maternal category not exist"
            else do
                rows <-
                    returningWithPool
                        pool
                        q
                        [(category'_name, fromOnly $ Prelude.head c'_id)]
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
deleteCategoryFromDb pool token_lifetime hLogger token (Just categoryName) =
    catch
        (do ch <- checkAdmin hLogger pool token_lifetime token
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
                                T.concat
                                    [ "Category "
                                    , from_category_name categoryName
                                    , " not exist"
                                    ]
                            return . Left . OtherError $ "Category not exist") $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left DatabaseError

editCategoryOnDb ::
       Pool Connection
    -> TokenLifeTime
    -> LoggerHandle IO
    -> Maybe Token
    -> EditCategory
    -> IO (Either SomeError ())
editCategoryOnDb _ _ hLogger _ (EditCategory Nothing _ _) = do
    logError hLogger "Category not edited. No old_name parametr"
    return $ Left $ OtherError "Category not edited. No old_name parametr"
editCategoryOnDb _ _ hLogger _ (EditCategory (Just (CategoryName "")) (Just _) (Just _)) = do
    logError hLogger "Empty old name parameter"
    return $ Left $ OtherError "Empty old name parameter"
editCategoryOnDb pool token_lifetime hLogger token (EditCategory (Just old_name) (Just new'_name) (Just (CategoryName ""))) =
    catch
        (do ch <- checkAdmin hLogger pool token_lifetime token
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    logInfo hLogger $
                        T.concat
                            [ "Update category_name parameter on category "
                            , from_category_name old_name
                            ]
                    n <-
                        executeWithPool
                            pool
                            "update categories set category_name = ? where category_name = ?"
                            (new'_name, old_name)
                    if n > 0
                        then do
                            logInfo hLogger "Category edited."
                            return $ Right ()
                        else do
                            logError hLogger $
                                T.concat
                                    [ "Category "
                                    , from_category_name old_name
                                    , " not exist"
                                    ]
                            return . Left . OtherError $ "Category not exist") $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        case errStateInt of
            23505 -> return . Left . OtherError $ "Category already exist"
            _     -> return $ Left DatabaseError
editCategoryOnDb pool token_lifetime hLogger token (EditCategory (Just old_name) (Just (CategoryName "")) (Just new'_maternal)) =
    catch
        (do ch <- checkAdmin hLogger pool token_lifetime token
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    m_id <-
                        queryWithPool
                            pool
                            "select category_id from categories where category_name = ?"
                            [new'_maternal] :: IO [Only Int]
                    n <-
                        executeWithPool
                            pool
                            "update categories set maternal_category = ? where category_name = ?"
                            (fromOnly $ myHead m_id, old_name)
                    if n > 0
                        then do
                            return $ Right ()
                        else do
                            logError hLogger $
                                T.concat
                                    [ "Category "
                                    , from_category_name old_name
                                    , " not exist"
                                    ]
                            return . Left . OtherError $ "Category not exist") $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        case errStateInt of
            23503 -> return $ Left $ OtherError "Maternal category not exist"
            _     -> return $ Left DatabaseError
  where
    myHead []    = Only (-1)
    myHead (x:_) = x
editCategoryOnDb pool token_lifetime hLogger token (EditCategory (Just old_name) (Just new'_name) (Just new'_maternal)) =
    catch
        (do ch <- checkAdmin hLogger pool token_lifetime token
            case ch of
                (False, bs) -> return $ Left bs
                (True, _) -> do
                    logInfo hLogger $
                        T.concat
                            [ "Update all parameters on category "
                            , from_category_name old_name
                            ]
                    m_id <-
                        queryWithPool
                            pool
                            "select category_id from categories where category_name = ?"
                            [new'_maternal] :: IO [Only Int]
                    let q =
                            "update categories set category_name = ?, maternal_category = ? where category_name = ?"
                    n <-
                        executeWithPool
                            pool
                            q
                            (new'_name, fromOnly $ myHead m_id, old_name)
                    if n > 0
                        then do
                            return $ Right ()
                        else do
                            logError hLogger $
                                T.concat
                                    [ "Category "
                                    , from_category_name old_name
                                    , " not exist"
                                    ]
                            return . Left . OtherError $ "Category not exist") $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        case errStateInt of
            23505 -> return . Left . OtherError $ "Category already exist"
            23503 -> return . Left . OtherError $ "Maternal category not exist"
            _     -> return $ Left DatabaseError
  where
    myHead []    = Only (-1)
    myHead (x:_) = x
editCategoryOnDb _ _ hLogger _ _ = do
    logError hLogger "No update parameters"
    return $ Left $ OtherError "No update parameters"
