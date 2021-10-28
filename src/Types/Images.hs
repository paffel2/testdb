{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Images where

import Data.Aeson
    ( KeyValue((.=))
    , ToJSON(toJSON)
    , defaultOptions
    , genericToJSON
    , object
    )
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Binary, FromRow, ToRow)
import GHC.Generics (Generic)

data Image =
    Image
        { image_file_name :: BC.ByteString
        , image_content_type :: BC.ByteString
        , image_content :: Binary LBS.ByteString
        }
    deriving (Show, Generic, ToRow, FromRow, Eq)

data ImageB =
    ImageB
        { image_b :: Binary LBS.ByteString
        , con_type :: BI.ByteString
        }
    deriving (Show, Generic, ToRow, FromRow, Eq)

data ElemImageArray =
    ElemImageArray
        { image_id :: Int
        , image_name :: T.Text
        }
    deriving (Show, Generic, ToRow, FromRow, Eq)

instance ToJSON ElemImageArray where
    toJSON (ElemImageArray imid imn) =
        object ["image_id" .= imid, "image_name" .= imn]

newtype ImageArray =
    ImageArray
        { images :: [ElemImageArray]
        }
    deriving (Show, Generic)

instance ToJSON ImageArray where
    toJSON = genericToJSON defaultOptions
