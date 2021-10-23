{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.Images where

import           Control.Exception          (catch)
import qualified Data.ByteString.Char8      as BC
import           Data.Maybe                 (fromMaybe, isNothing)
import           Data.Pool                  (Pool)
import qualified Data.Text                  as T
import           Database.PostgreSQL.Simple (Connection, SqlError (sqlState))
import           HelpFunction               (readByteStringToInt, toQuery)
import           Logger                     (Handle, logError)
import           PostgreSqlWithPool         (queryWithPool, query_WithPool)
import           Types.Images               (ElemImageArray,
                                             ImageArray (ImageArray), ImageB)
import           Types.Other                (ErrorMessage, Id, Page (from_page))

getPhoto ::
       Pool Connection -> Handle IO -> Id -> IO (Either ErrorMessage ImageB)
getPhoto pool hLogger image_id' =
    catch
        (do let q =
                    toQuery $
                    BC.concat
                        [ "select image_b, content_type from images where image_id = ?"
                        ]
            rows <- queryWithPool pool q [image_id'] :: IO [ImageB]
            if Prelude.null rows
                then return $ Left "Image not exist"
                else return $ Right $ Prelude.head rows) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left "Database error"

getPhotoList ::
       Pool Connection
    -> Handle IO
    -> Maybe Page
    -> IO (Either ErrorMessage ImageArray)
getPhotoList pool hLogger pageParam =
    catch
        (do rows <- query_WithPool pool q :: IO [ElemImageArray]
            return $ Right (ImageArray rows)) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left "Database error"
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
            ["select image_id, image_name from images order by image_id", pg]
