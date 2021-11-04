{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.Images where

import           Control.Exception          (catch)
import qualified Data.ByteString.Char8      as BC
import           Data.Maybe                 (fromMaybe, isNothing)
import           Data.Pool                  (Pool)
import qualified Data.Text                  as T
import           Database.PostgreSQL.Simple (Connection, SqlError (sqlState))
import           HelpFunction               (readByteStringToInt, toQuery)
import           Logger                     (LoggerHandle, logError, logInfo)
import           PostgreSqlWithPool         (queryWithPool, query_WithPool)
import           Types.Images               (ElemImageArray,
                                             ImageArray (ImageArray), ImageB)
import           Types.Other                (Id, Page (getPage),
                                             SomeError (DatabaseError, OtherError))

getPhotoFromDb ::
       Pool Connection -> LoggerHandle IO -> Id -> IO (Either SomeError ImageB)
getPhotoFromDb pool hLogger imageId =
    catch
        (do let q =
                    toQuery $
                    BC.concat
                        [ "select image_b, content_type from images where image_id = ?"
                        ]
            rows <- queryWithPool pool q [imageId] :: IO [ImageB]
            if Prelude.null rows
                then do
                    logError hLogger "Image not exist"
                    return . Left . OtherError $ "Image not exist"
                else do
                    logInfo hLogger "Image sended"
                    return $ Right $ Prelude.head rows) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left DatabaseError

getPhotoListFromDb ::
       Pool Connection
    -> LoggerHandle IO
    -> Maybe Page
    -> IO (Either SomeError ImageArray)
getPhotoListFromDb pool hLogger pageParam =
    catch
        (do rows <- query_WithPool pool q :: IO [ElemImageArray]
            logInfo hLogger "List of images sended"
            return $ Right (ImageArray rows)) $ \e -> do
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
                     , BC.pack $ show $ (maybe 1 getPage pageParam - 1) * 10
                     ]
    q =
        toQuery $
        "select image_id, image_name from images order by image_id" <> pg
