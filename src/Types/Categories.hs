{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types.Categories where

import Data.Aeson (ToJSON(toJSON), defaultOptions, genericToJSON)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import GHC.Generics (Generic)

newtype ElemOfCategoryList =
    ElemOfCategoryList
        { category_get_name :: T.Text
        }
    deriving (Show, Generic, ToRow, FromRow)

instance ToJSON ElemOfCategoryList where
    toJSON = genericToJSON defaultOptions

newtype ListOfCategories =
    ListOfCategories
        { list_of_categories :: [ElemOfCategoryList]
        }
    deriving (Show, Generic)

instance ToJSON ListOfCategories where
    toJSON = genericToJSON defaultOptions

newtype CategoryName =
    CategoryName
        { from_category_name :: T.Text
        }
    deriving (Show, Eq)

instance ToField CategoryName where
    toField = toField . from_category_name

data CreateCategory =
    CreateCategory
        { create_categrory_name :: Maybe CategoryName
        , create_categrory_maternal_category :: Maybe CategoryName
        }
    deriving (Show, Generic, ToRow, Eq)

data EditCategory =
    EditCategory
        { edit_category_name :: Maybe CategoryName
        , edit_category_new_name :: Maybe CategoryName
        , edit_category_new_maternal :: Maybe CategoryName
        }
    deriving (Show, Generic, ToRow, Eq)
