{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

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
import Data.Time (Day, UTCTime)
import Database.PostgreSQL.Simple (Binary, FromRow, ToRow)
import Database.PostgreSQL.Simple.Types (PGArray(fromPGArray))
import GHC.Generics (Generic)

data Comment =
    Comment
        { comment_token :: T.Text
        , comment_token_lifetime :: Int
        , comment_text :: T.Text
        , comment_news_id :: Int
        , comment_time :: UTCTime
        }
    deriving (Show, Generic, ToRow, FromRow)

data ElemOfNewsArray =
    ElemOfNewsArray
        { news_id' :: Int
        , short_title' :: T.Text
        , date_creation' :: Day
        , author_name :: T.Text
        , category_name' :: T.Text
        , news_text' :: T.Text
        }
    deriving (Show, Generic, ToRow, FromRow)

instance ToJSON ElemOfNewsArray

newtype NewsArray =
    NewsArray
        { news :: [ElemOfNewsArray]
        }
    deriving (Show, Generic)

instance ToJSON NewsArray where
    toJSON = genericToJSON defaultOptions

data ElemOfCommentArray =
    ElemOfCommentArray
        { comment_author_name :: T.Text
        , comment_text' :: T.Text
        , comment_time' :: UTCTime
        , comment_id' :: Int
        }
    deriving (Show, Generic, ToRow, FromRow)

newtype CommentArray =
    CommentArray
        { comments :: [ElemOfCommentArray]
        }
    deriving (Show, Generic)

instance ToJSON ElemOfCommentArray

instance ToJSON CommentArray where
    toJSON = genericToJSON defaultOptions

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

data ElemOfDraftArray =
    DraftGet
        { draft_get_short_title :: T.Text
        , draft_get_date_of_changes :: UTCTime
        , draft_get_category_id :: Maybe Int
        , draft_get_text :: Maybe T.Text
        , draft_get_main_image_id :: Maybe Int
        , draft_get_images :: Maybe (PGArray Int)
        }
    deriving (Show, Generic, FromRow)

instance ToJSON ElemOfDraftArray where
    toJSON (DraftGet dgst dgdoc dgci dgt dgmii dgi) =
        object
            [ "draft_short_title'" .= dgst
            , "date_of_changes'" .= dgdoc
            , "draft_category_id'" .= dgci
            , "draft_text'" .= dgt
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

data Image =
    Image
        { image_file_name :: BC.ByteString
        , image_content_type :: BC.ByteString
        , image_content :: Binary LBS.ByteString
        }
    deriving (Show, Generic, ToRow, FromRow, Eq)

data Profile =
    Profile
        { profile_first_name :: Maybe T.Text
        , profile_last_name :: Maybe T.Text
        , profile_avatar :: Maybe Int
        }
    deriving (Show, Generic, ToRow, FromRow)

instance ToJSON Profile where
    toJSON (Profile pfn pln pa) =
        object
            [ "profile_first_name" .= pfn
            , "profile_last_name" .= pln
            , "profile_avatar" .= pa
            ]

data TokenProfile =
    TokenProfile
        { profile_token :: T.Text
        , profile_token_lifetime :: Int
        }
    deriving (Show, Generic, ToRow, FromRow)

type DatabaseAddress = BC.ByteString

type TokenLifeTime = Int

data DeleteComment =
    DeleteComment
        { delc_token :: T.Text
        , delc_token_lifetime :: TokenLifeTime
        , delc_comment_id :: Int
        }
    deriving (Show, Generic, ToRow, FromRow)

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

data Draft =
    Draft
        { draft_short_title'' :: T.Text
        , date_of_changes'' :: UTCTime
        , draft_category_id'' :: Maybe Int
        , draft_text'' :: Maybe T.Text
        , draft_main_image_id'' :: Maybe Int
        , draft_images' :: Maybe (PGArray Int)
        , draft_tags' :: Maybe (PGArray T.Text)
        }
    deriving (Show, Generic, FromRow)

instance ToJSON Draft where
    toJSON (Draft dst doc dci dt dmii di dts) =
        object
            [ "draft_short_title''" .= dst
            , "date_of_changes''" .= doc
            , "draft_category_id''" .= dci
            , "draft_text''" .= dt
            , "draft_main_image_id''" .= dmii
            , "draft_images'" .= (fromPGArray <$> di)
            , "draft_tags'" .= (fromPGArray <$> dts)
            ]

data GetNews =
    GetNews
        { gn_news_id'' :: Int
        , gn_short_title'' :: T.Text
        , gn_date_creation'' :: Day
        , gn_author_name' :: T.Text
        , gn_category_name''' :: T.Text
        , gn_news_text'' :: T.Text
        , gn_news_main_image :: Maybe Int
        , gn_news_other_images :: Maybe (PGArray Int)
        , gn_news_tags :: Maybe (PGArray T.Text)
        }
    deriving (Show, Generic, ToRow, FromRow)

instance ToJSON GetNews where
    toJSON (GetNews gnni gnst gndc gnan gncn gnnt gnnmi gnnoi gnnts) =
        object
            [ "news_id''" .= gnni
            , "short_title''" .= gnst
            , "date_creation''" .= gndc
            , "author_name'" .= gnan
            , "category_name'''" .= gncn
            , "news_text''" .= gnnt
            , "news_main_image" .= gnnmi
            , "news_other_images" .= (fromPGArray <$> gnnoi)
            , "news_tags" .= (fromPGArray <$> gnnts)
            ]

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
