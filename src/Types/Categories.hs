{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}

module Types.Categories where

import           Data.Aeson                         (Options (fieldLabelModifier),
                                                     ToJSON (toJSON), camelTo2,
                                                     defaultOptions,
                                                     genericToJSON)
import qualified Data.Text                          as T
import           Database.PostgreSQL.Simple         (FromRow, ToRow)
import           Database.PostgreSQL.Simple.ToField (ToField (..))
import           GHC.Generics                       (Generic)

newtype ElemOfCategoryList =
    ElemOfCategoryList
        { eloclCategoryName :: T.Text
        }
    deriving (Show, Generic, ToRow, FromRow)

instance ToJSON ElemOfCategoryList where
    toJSON =
        genericToJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 5}

newtype ListOfCategories =
    ListOfCategories
        { getListOfCategories :: [ElemOfCategoryList]
        }
    deriving (Show, Generic)

instance ToJSON ListOfCategories where
    toJSON =
        genericToJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 3}

newtype CategoryName =
    CategoryName
        { getCategoryName :: T.Text
        }
    deriving (Show, Eq)
    deriving ToField via T.Text

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
