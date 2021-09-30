{-# LANGUAGE OverloadedStrings #-}

module FromRequest where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Database.PostgreSQL.Simple.Types (Binary(Binary))
import HelpFunction
import Network.Wai (Request(queryString))
import Network.Wai.Parse
import Types

takeToken :: Request -> Maybe Token
takeToken req =
    Token . E.decodeUtf8 <$>
    fromMaybe Nothing (lookup "token" $ queryString req)

toImage :: FileInfo LBS.ByteString -> Image
toImage file_info =
    Image
        (fileName file_info)
        (fileContentType file_info)
        (Binary $ fileContent file_info)

toPage :: Request -> Maybe Page
toPage req = Page <$> fromMaybe Nothing (lookup "page" $ queryString req)

toEditAuthor :: [Param] -> EditAuthor
toEditAuthor params =
    EditAuthor
        { edit_author_description =
              E.decodeUtf8 <$> lookup "new_description" params
        , edit_author_id = lookup "author_id" params >>= readByteStringToInt
        }

toLogin :: [Param] -> Maybe Login
toLogin params = Login . E.decodeUtf8 <$> lookup "author_login" params

toCreateAuthor :: [Param] -> CreateAuthor
toCreateAuthor params =
    CreateAuthor
        { create_author_login = toLogin params
        , create_author_description =
              E.decodeUtf8 <$> lookup "description" params
        }
