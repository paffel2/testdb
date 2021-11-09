{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Drafts where

import           Data.Aeson                         (KeyValue ((.=)),
                                                     ToJSON (toJSON),
                                                     defaultOptions,
                                                     genericToJSON, object)
import qualified Data.ByteString.Char8              as BC
import qualified Data.Text                          as T
import           Data.Time                          (UTCTime)
import           Database.PostgreSQL.Simple         (FromRow, ToRow)
import           Database.PostgreSQL.Simple.ToField (ToField (..))
import           Database.PostgreSQL.Simple.Types   (PGArray (fromPGArray))
import           GHC.Generics                       (Generic)
import           Types.Other                        (Token)

data ElemOfDraftArray =
    DraftGet
        { draftGetShortTitle    :: T.Text
        , draftGetDateOfChanges :: UTCTime
        , draftGetCategoryId    :: Maybe Int
        , draftGetText          :: Maybe T.Text
        , draftGetMainImageId   :: Maybe Int
        , draftGetImages        :: Maybe (PGArray Int)
        }
    deriving (Show, Generic, FromRow)

instance ToJSON ElemOfDraftArray where
    toJSON (DraftGet dgst dgdoc dgci dgt dgmii dgi) =
        object
            [ "draft_short_title" .= dgst
            , "date_of_changes" .= dgdoc
            , "draft_category_id" .= dgci
            , "draft_text" .= dgt
            , "draft_main_image_id'" .= dgmii
            , "draft_images" .= (fromPGArray <$> dgi)
            ]

newtype DraftArray =
    DraftArray
        { drafts :: [ElemOfDraftArray]
        }
    deriving (Show, Generic)

instance ToJSON DraftArray where
    toJSON = genericToJSON defaultOptions

data Draft =
    Draft
        { draftShortTitle  :: T.Text
        , dateOfChanges    :: UTCTime
        , draftCategoryId  :: Maybe Int
        , draftText        :: Maybe T.Text
        , draftMainImageId :: Maybe Int
        , draftImages      :: Maybe (PGArray Int)
        , draftTags        :: Maybe (PGArray T.Text)
        }
    deriving (Show, Generic, FromRow)

instance ToJSON Draft where
    toJSON (Draft dst doc dci dt dmii di dts) =
        object
            [ "draft_short_title" .= dst
            , "date_of_changes" .= doc
            , "draft_category_id" .= dci
            , "draft_text" .= dt
            , "draft_main_image_id" .= dmii
            , "draft_images" .= (fromPGArray <$> di)
            , "draft_tags" .= (fromPGArray <$> dts)
            ]

data DraftInf =
    DraftInf
        { draftInfToken    :: Maybe Token
        , draftInfCategory :: Maybe T.Text
        , draftInfTitle    :: Maybe T.Text
        , draftInfText     :: Maybe T.Text
        }
    deriving (Generic, ToRow, Show, Eq)

newtype DraftTags =
    DraftTags
        { getDraftTags :: BC.ByteString
        }
    deriving (Show, Eq)
    deriving ToField via BC.ByteString
