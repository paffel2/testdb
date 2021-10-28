{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types.Tags where

import Data.Aeson (ToJSON(toJSON), defaultOptions, genericToJSON)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import GHC.Generics (Generic)

newtype Tag =
    Tag
        { tag_name :: T.Text
        }
    deriving (Show, Generic, ToRow, FromRow)

instance ToJSON Tag where
    toJSON = genericToJSON defaultOptions

newtype TagsList =
    TagsList
        { tags :: [Tag]
        }
    deriving (Show, Generic)

instance ToJSON TagsList where
    toJSON = genericToJSON defaultOptions

newtype TagName =
    TagName
        { from_tag_name :: T.Text
        }
    deriving (Show, Eq)

instance ToField TagName where
    toField = toField . from_tag_name

data EditTag =
    EditTag
        { edit_tag_new_name :: Maybe TagName
        , edit_tag_old_name :: Maybe TagName
        }
    deriving (Show, Generic, ToRow, Eq)
