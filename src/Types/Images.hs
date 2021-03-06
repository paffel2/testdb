{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Images where

import           Data.Aeson                 (Options (fieldLabelModifier),
                                             ToJSON (toJSON), camelTo2,
                                             defaultOptions, genericToJSON)
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Internal   as BI
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Text                  as T
import           Database.PostgreSQL.Simple (Binary, FromRow, ToRow)
import           GHC.Generics               (Generic)

data Image =
    Image
        { imageFileName    :: BC.ByteString
        , imageContentType :: BC.ByteString
        , imageContent     :: Binary LBS.ByteString
        }
    deriving (Show, Generic, ToRow, FromRow, Eq)

data ImageB =
    ImageB
        { imageB  :: Binary LBS.ByteString
        , conType :: BI.ByteString
        }
    deriving (Show, Generic, ToRow, FromRow, Eq)

data ElemImageArray =
    ElemImageArray
        { imageId   :: Int
        , imageName :: T.Text
        }
    deriving (Show, Generic, ToRow, FromRow, Eq)

instance ToJSON ElemImageArray where
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = camelTo2 '_'}

newtype ImageArray =
    ImageArray
        { images :: [ElemImageArray]
        }
    deriving (Show, Generic)

instance ToJSON ImageArray where
    toJSON = genericToJSON defaultOptions
