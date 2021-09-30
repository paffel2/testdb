{-# LANGUAGE OverloadedStrings #-}

module FromRequest where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Database.PostgreSQL.Simple.Types (Binary(Binary))
import Network.Wai (Request(queryString))
import Network.Wai.Parse (FileInfo(fileContent, fileContentType, fileName))
import Types

takeToken :: Request -> Maybe T.Text
takeToken req =
    E.decodeUtf8 <$> fromMaybe Nothing (lookup "token" $ queryString req)

toImage :: FileInfo LBS.ByteString -> Image
toImage file_info =
    Image
        (fileName file_info)
        (fileContentType file_info)
        (Binary $ fileContent file_info)

toPage :: Request -> Maybe Page
toPage req = Page <$> fromMaybe Nothing (lookup "page" $ queryString req)
