{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.Images where

import           Control.Exception          (try)
import           Control.Monad.Except       (MonadError (throwError),
                                             MonadIO (..))
import           Data.Maybe                 (fromMaybe)
import           Data.Pool                  (Pool)
import qualified Data.Text                  as T
import           Database.PostgreSQL.Simple (Connection, SqlError (sqlState))
import           HelpFunction               (pageToBS, readByteStringToInt,
                                             toQuery)
import           Logger                     (LoggerHandle, logError, logInfo)
import           PostgreSqlWithPool         (queryWithPool, query_WithPool)
import           Types.Images               (ImageArray (ImageArray), ImageB)
import           Types.Other                (Id, Page,
                                             SomeError (DatabaseError, OtherError))

--------------------------------------------------------------------------------
getPhotoFromDb ::
       (MonadIO m, MonadError SomeError m)
    => Pool Connection
    -> LoggerHandle IO
    -> Id
    -> m ImageB
getPhotoFromDb pool hLogger imageId = do
    rows <- liftIO $ try (queryWithPool pool q [imageId])
    case rows of
        Left e -> errorHandle e
        Right [] -> do
            liftIO $ logError hLogger "Image not exist"
            throwError $ OtherError "Image not exist"
        Right (image:_) -> do
            liftIO $ logInfo hLogger "Image sended"
            return image
  where
    q = "select image_b, content_type from images where image_id = ?"
    errorHandle sqlError = do
        let errState = sqlState sqlError
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        liftIO $logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        throwError DatabaseError

---------------------------------------------------------------------------------------------------
getPhotoListFromDb ::
       (MonadIO m, MonadError SomeError m)
    => Pool Connection
    -> LoggerHandle IO
    -> Maybe Page
    -> m ImageArray
getPhotoListFromDb pool hLogger pageParam = do
    rows <- liftIO $ try (query_WithPool pool q)
    case rows of
        Left e -> errorHandle e
        Right images -> do
            liftIO $ logInfo hLogger "List of images sended"
            return (ImageArray images)
  where
    q =
        toQuery $
        "select image_id, image_name from images order by image_id" <>
        pageToBS pageParam
    errorHandle sqlError = do
        let errState = sqlState sqlError
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        liftIO $
            logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        throwError DatabaseError
