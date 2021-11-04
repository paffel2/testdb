{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Types.Tags where

import           Data.Aeson                         (ToJSON (toJSON),
                                                     defaultOptions,
                                                     genericToJSON)
import qualified Data.Text                          as T
import           Database.PostgreSQL.Simple         (FromRow, ToRow)
import           Database.PostgreSQL.Simple.ToField (ToField (..))
import           GHC.Generics                       (Generic)

newtype Tag =
    Tag
        { getTag :: T.Text
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
        { getTagName :: T.Text
        }
    deriving (Show, Eq)

instance ToField TagName where
    toField = toField . getTagName

data EditTag =
    EditTag
        { editTagNewName :: Maybe TagName
        , editTagOldName :: Maybe TagName
        }
    deriving (Show, Generic, ToRow, Eq)
