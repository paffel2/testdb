{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}

module Types.Authors where

import           Data.Aeson                         (Options (fieldLabelModifier),
                                                     ToJSON (toJSON), camelTo2,
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
    toJSON =
        genericToJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 3}

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
    deriving ToField via T.Text

data CreateAuthor =
    CreateAuthor
        { createAuthorLogin       :: Maybe T.Text
        , createAuthorDescription :: Maybe T.Text
        }
    deriving (Show, Generic, ToRow, Eq)
