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

toTagName :: Request -> Maybe TagName
toTagName req =
    TagName . T.toLower . E.decodeUtf8 <$>
    fromMaybe Nothing (lookup "tag_name" $ queryString req)

toEditTag :: [Param] -> EditTag
toEditTag params =
    EditTag
        { edit_tag_new_name =
              TagName . T.toLower . E.decodeUtf8 <$>
              lookup "new_tag_name" params
        , edit_tag_old_name =
              TagName . T.toLower . E.decodeUtf8 <$>
              lookup "old_tag_name" params
        }

toCategoryName :: [Param] -> Maybe CategoryName
toCategoryName params =
    CategoryName . T.toLower . E.decodeUtf8 <$> lookup "category_name" params

toCreateCategory :: [Param] -> CreateCategory
toCreateCategory params =
    CreateCategory
        { create_categrory_name =
              CategoryName . T.toLower . E.decodeUtf8 <$>
              lookup "category_name" params
        , create_categrory_maternal_category =
              CategoryName . T.toLower . E.decodeUtf8 <$>
              lookup "maternal_category_name" params
        }

toEditCategory :: [Param] -> EditCategory
toEditCategory params =
    EditCategory
        { edit_category_name =
              CategoryName . T.toLower . E.decodeUtf8 <$>
              lookup "category_name" params
        , edit_category_new_name =
              CategoryName . T.toLower . E.decodeUtf8 <$>
              lookup "new_name" params
        , edit_category_new_maternal =
              CategoryName . T.toLower . E.decodeUtf8 <$>
              lookup "new_maternal" params
        }
