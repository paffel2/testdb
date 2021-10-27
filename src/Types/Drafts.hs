{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
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
        { draft_get_short_title     :: T.Text
        , draft_get_date_of_changes :: UTCTime
        , draft_get_category_id     :: Maybe Int
        , draft_get_text            :: Maybe T.Text
        , draft_get_main_image_id   :: Maybe Int
        , draft_get_images          :: Maybe (PGArray Int)
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
        { draft_short_title   :: T.Text
        , date_of_changes     :: UTCTime
        , draft_category_id   :: Maybe Int
        , draft_text          :: Maybe T.Text
        , draft_main_image_id :: Maybe Int
        , draft_images        :: Maybe (PGArray Int)
        , draft_tags          :: Maybe (PGArray T.Text)
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
        { draft_inf_token    :: Maybe Token
        , draft_inf_category :: Maybe T.Text
        , draft_inf_title    :: Maybe T.Text
        , draft_inf_text     :: Maybe T.Text
        }
    deriving (Generic, ToRow, Show, Eq)

newtype DraftTags =
    DraftTags
        { from_draft_tags :: BC.ByteString
        }
    deriving (Show, Eq)

instance ToField DraftTags where
    toField = toField . from_draft_tags
