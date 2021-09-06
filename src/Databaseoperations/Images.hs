{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.Images where

import Control.Exception (catch)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe, isNothing)
import Data.Pool (Pool)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection, SqlError(sqlState))
import HelpFunction (readByteStringToInt, toQuery)
import Logger (Handle, logDebug, logError, logInfo)
import PostgreSqlWithPool (queryWithPool, query_WithPool)
import Types (ElemImageArray, ImageArray(ImageArray), ImageB)

getPhoto ::
       Handle -> Pool Connection -> Int -> IO (Either LBS.ByteString ImageB)
getPhoto hLogger pool image_id =
    catch
        (do logDebug hLogger "Getting images from db"
            let q =
                    toQuery $
                    BC.concat
                        [ "select image_b, content_type from images where image_id = ?"
                        ]
            rows <- queryWithPool pool q [image_id] :: IO [ImageB]
            if Prelude.null rows
                then return $ Left "Image not exist"
                else return $ Right $ Prelude.head rows) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left "Database error"

getPhotoList ::
       Handle
    -> Pool Connection
    -> Maybe BC.ByteString
    -> IO (Either LBS.ByteString ImageArray)
getPhotoList hLogger pool pageParam =
    catch
        (do logInfo hLogger "Someone try get photo list"
            rows <- query_WithPool pool q :: IO [ElemImageArray]
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
                     , BC.pack $
                       show $
                       (fromMaybe
                            1
                            (readByteStringToInt (fromMaybe "" pageParam)) -
                        1) *
                       10
                     ]
    q =
        toQuery $
        BC.concat
            ["select image_id, image_name from images order by image_id", pg]
