{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Types.Categories where

import           Data.Aeson                         (ToJSON (toJSON),
                                                     defaultOptions,
                                                     genericToJSON)
import qualified Data.Text                          as T
import           Database.PostgreSQL.Simple         (FromRow, ToRow)
import           Database.PostgreSQL.Simple.ToField (ToField (..))
import           GHC.Generics                       (Generic)

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
        { getCategoryName :: T.Text
        }
    deriving (Show, Eq)

instance ToField CategoryName where
    toField = toField . getCategoryName

data CreateCategory =
    CreateCategory
        { createCategroryName             :: Maybe CategoryName
        , createCategroryMaternalCategory :: Maybe CategoryName
        }
    deriving (Show, Generic, ToRow, Eq)

data EditCategory =
    EditCategory
        { editCategoryName        :: Maybe CategoryName
        , editCategoryNewName     :: Maybe CategoryName
        , editCategoryNewMaternal :: Maybe CategoryName
        }
    deriving (Show, Generic, ToRow, Eq)
