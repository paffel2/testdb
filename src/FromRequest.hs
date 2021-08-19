{-# LANGUAGE OverloadedStrings #-}

module FromRequest where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Database.PostgreSQL.Simple.Types (Binary(Binary))
import Network.Wai (Request(queryString))
import Network.Wai.Parse (FileInfo(fileContent, fileContentType, fileName))
import Types (Image(Image))

takeToken :: Request -> Maybe BC.ByteString
takeToken req = fromMaybe Nothing (lookup "token" $ queryString req)

toImage :: FileInfo LBS.ByteString -> Image
toImage file_info =
    Image
        (fileName file_info)
        (fileContentType file_info)
        (Binary $ fileContent file_info)
