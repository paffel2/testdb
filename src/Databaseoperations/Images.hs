{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.Images where

import           Control.Monad.Except       (MonadError (throwError))
import           Data.Pool                  (Pool)
import           Database.PostgreSQL.Simple (Connection)
import           HelpFunction               (pageToBS, toQuery)
import           PostgreSqlWithPool         (queryWithPool, query_WithPool)
import           Types.Images               (ImageArray (ImageArray), ImageB)
import           Types.Other                (Id, MonadIOWithError, Page,
                                             SomeError (OtherError))

--------------------------------------------------------------------------------
getPhotoFromDb :: MonadIOWithError m => Pool Connection -> Id -> m ImageB
getPhotoFromDb pool imageId = do
    rows <- queryWithPool pool q [imageId]
    if Prelude.null rows
        then throwError $ OtherError "Image not exist"
        else return $ head rows
  where
    q = "select image_b, content_type from images where image_id = ?"

--------------------------------------------------------------------------------
getPhotoListFromDb ::
       MonadIOWithError m => Pool Connection -> Maybe Page -> m ImageArray
getPhotoListFromDb pool pageParam = do
    rows <- query_WithPool pool q
    return (ImageArray rows)
  where
    q =
        toQuery $
        "select image_id, image_name from images order by image_id" <>
        pageToBS pageParam
