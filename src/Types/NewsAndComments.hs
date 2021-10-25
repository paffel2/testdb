{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.NewsAndComments where

import           Data.Aeson                         (KeyValue ((.=)),
                                                     ToJSON (toJSON),
                                                     defaultOptions,
                                                     genericToJSON, object)
import qualified Data.ByteString.Char8              as BC
import qualified Data.Text                          as T
import           Data.Time                          (Day, UTCTime)
import           Database.PostgreSQL.Simple         (FromRow, ToRow)
import           Database.PostgreSQL.Simple.ToField (ToField (..))
import           Database.PostgreSQL.Simple.Types   (PGArray (fromPGArray))
import           GHC.Generics                       (Generic)
import           Types.Other                        (Id, Token, TokenLifeTime)

data Comment =
    Comment
        { comment_token          :: Maybe Token
        , comment_token_lifetime :: TokenLifeTime
        , comment_text           :: Maybe CommentText
        , comment_news_id        :: Maybe Id
        }
    deriving (Generic, ToRow)

data ElemOfNewsArray =
    ElemOfNewsArray
        { news_id'       :: Int
        , short_title'   :: T.Text
        , date_creation' :: Day
        , author_name    :: T.Text
        , category_name' :: T.Text
        , news_text'     :: T.Text
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
        , comment_text'       :: T.Text
        , comment_time'       :: UTCTime
        , comment_id'         :: Int
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

data GetNews =
    GetNews
        { gn_news_id''         :: Int
        , gn_short_title''     :: T.Text
        , gn_date_creation''   :: Day
        , gn_author_name'      :: T.Text
        , gn_category_name'''  :: T.Text
        , gn_news_text''       :: T.Text
        , gn_news_main_image   :: Maybe Int
        , gn_news_other_images :: Maybe (PGArray Int)
        , gn_news_tags         :: Maybe (PGArray T.Text)
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

newtype CommentText =
    CommentText
        { from_comment_text :: T.Text
        }
    deriving (Show, Eq)

instance ToField CommentText where
    toField = toField . from_comment_text

newtype Sort =
    Sort
        { from_sort :: BC.ByteString
        }
    deriving (Show, Eq)

newtype TagInFilterParam =
    TagInFilterParam
        { from_tag_in_fp :: [Int]
        }
    deriving (Show, Eq)

newtype CategoryFilterParam =
    CategoryFilterParam
        { from_category_fp :: Id
        }
    deriving (Show, Eq)

newtype TagFilterParam =
    TagFilterParam
        { from_tag_fp :: Id
        }
    deriving (Show, Eq)

instance ToField TagFilterParam where
    toField = toField . from_tag_fp

newtype TagAllFilterParam =
    TagAllFilterParam
        { from_tag_all_fp :: [Int]
        }
    deriving (Show, Eq)

newtype TitleFilterParam =
    TitleFilterParam
        { from_title_fp :: T.Text
        }
    deriving (Show, Eq)

newtype ContentFilterParam =
    ContentFilterParam
        { from_content_fp :: T.Text
        }
    deriving (Show, Eq)

newtype DateFilterParam =
    DateFilterParam
        { from_date_fp :: Day
        }
    deriving (Show, Eq)

instance ToField DateFilterParam where
    toField = toField . from_date_fp

newtype BeforeDateFilterParam =
    BeforeDateFilterParam
        { from_before_date_fp :: Day
        }
    deriving (Show, Eq)

instance ToField BeforeDateFilterParam where
    toField = toField . from_before_date_fp

newtype AfterDateFilterParam =
    AfterDateFilterParam
        { from_after_date_fp :: Day
        }
    deriving (Show, Eq)

instance ToField AfterDateFilterParam where
    toField = toField . from_after_date_fp

newtype AuthorFilterParam =
    AuthorFilterParam
        { from_author_fp :: T.Text
        }
    deriving (Show, Eq)

data CommentWithoutTokenLifeTime =
    CommentWithoutTokenLifeTime
        { comment_token'   :: Maybe Token
        , comment_text''   :: Maybe CommentText
        , comment_news_id' :: Maybe Id
        }
    deriving (Generic, ToRow)

toComment :: TokenLifeTime -> CommentWithoutTokenLifeTime -> Comment
toComment tokenLifeTime comm =
    Comment
        { comment_token = comment_token' comm
        , comment_token_lifetime = tokenLifeTime
        , comment_text = comment_text'' comm
        , comment_news_id = comment_news_id' comm
        }
