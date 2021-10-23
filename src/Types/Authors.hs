{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types.Authors where

import Data.Aeson (ToJSON(toJSON), defaultOptions, genericToJSON)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import GHC.Generics (Generic)

data ElemAuthorsList =
    ElemAuthorsList
        { author_id :: Int
        , author_name' :: T.Text
        , authors_description :: Maybe T.Text
        }
    deriving (Show, Generic, ToRow, FromRow, Eq)

instance ToJSON ElemAuthorsList where
    toJSON = genericToJSON defaultOptions

newtype AuthorsList =
    AuthorsList
        { authors :: [ElemAuthorsList]
        }
    deriving (Show, Generic)

instance ToJSON AuthorsList where
    toJSON = genericToJSON defaultOptions

data EditAuthor =
    EditAuthor
        { edit_author_description :: Maybe T.Text
        , edit_author_id :: Maybe Int
        }
    deriving (Show, Generic, ToRow, Eq)

newtype AuthorLogin =
    AuthorLogin
        { author_login :: T.Text
        }
    deriving (Show, Eq)

instance ToField AuthorLogin where
    toField = toField . author_login

data CreateAuthor =
    CreateAuthor
        { create_author_login :: Maybe T.Text
        , create_author_description :: Maybe T.Text
        }
    deriving (Show, Generic, ToRow, Eq)
