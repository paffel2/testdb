{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Types.Authors where

import           Data.Aeson                         (ToJSON (toJSON),
                                                     defaultOptions,
                                                     genericToJSON)
import qualified Data.Text                          as T
import           Database.PostgreSQL.Simple         (FromRow, ToRow)
import           Database.PostgreSQL.Simple.ToField (ToField (..))
import           GHC.Generics                       (Generic)

data ElemAuthorsList =
    ElemAuthorsList
        { ealAuthorId           :: Int
        , ealAuthorName         :: T.Text
        , ealAuthorsDescription :: Maybe T.Text
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
        { editAuthorDescription :: Maybe T.Text
        , editAuthorId          :: Maybe Int
        }
    deriving (Show, Generic, ToRow, Eq)

newtype AuthorLogin =
    AuthorLogin
        { getAuthorLogin :: T.Text
        }
    deriving (Show, Eq)

instance ToField AuthorLogin where
    toField = toField . getAuthorLogin

data CreateAuthor =
    CreateAuthor
        { createAuthorLogin       :: Maybe T.Text
        , createAuthorDescription :: Maybe T.Text
        }
    deriving (Show, Generic, ToRow, Eq)
