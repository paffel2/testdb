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
import Data.Time (Day, NominalDiffTime, UTCTime)
import Database.PostgreSQL.Simple (Binary, FromRow, ToRow)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Database.PostgreSQL.Simple.Types (PGArray(fromPGArray))
import GHC.Generics (Generic)

data Comment =
    Comment
        { comment_token :: Maybe Token
        , comment_token_lifetime :: TokenLifeTime
        , comment_text :: Maybe CommentText
        , comment_news_id :: Maybe Id
        --, comment_time :: UTCTime
        }
    deriving (Generic, ToRow)

{-data Comment =
    Comment
        { comment_token :: T.Text
        , comment_token_lifetime :: Int
        , comment_text :: T.Text
        , comment_news_id :: Maybe Id
        , comment_time :: UTCTime
        }
    deriving (Show, Generic, ToRow)-}

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
        { profile_token :: Token
        , profile_token_lifetime :: TokenLifeTime
        }
    deriving (Show, Generic, ToRow)

type DatabaseAddress = BC.ByteString

type TokenLifeTimee = Int

{-data DeleteComment =
    DeleteComment
        { delc_token :: T.Text
        , delc_token_lifetime :: TokenLifeTimee
        , delc_comment_id :: Int
        }
    deriving (Show, Generic, ToRow, FromRow)-}
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

data ElemAuthorsList =
    ElemAuthorsList
        { author_id :: Int
        , author_name' :: T.Text
        , authors_description :: Maybe T.Text
        }
    deriving (Show, Generic, ToRow, FromRow, Eq)

instance ToJSON ElemAuthorsList where
    toJSON = genericToJSON defaultOptions

newtype AuthorsList =
    AuthorsList
        { authors :: [ElemAuthorsList]
        }
    deriving (Show, Generic)

instance ToJSON AuthorsList where
    toJSON = genericToJSON defaultOptions

newtype TokenLifeTime =
    TokenLifeTime
        { token_life_time :: Int
        }

instance Show TokenLifeTime where
    show (TokenLifeTime tk) = show tk

instance ToField TokenLifeTime where
    toField = toField . token_life_time

data PoolParams =
    PoolParams
        { num_stripes :: Int
        , idle_time :: NominalDiffTime
        , max_resources :: Int
        }
    deriving (Show)

newtype Page =
    Page
        { from_page :: BC.ByteString
        }

newtype Token =
    Token
        { from_token :: T.Text
        }
    deriving (Show)

instance ToField Token where
    toField = toField . from_token

data EditAuthor =
    EditAuthor
        { edit_author_description :: Maybe T.Text
        , edit_author_id :: Maybe Int
        }
    deriving (Show, Generic, ToRow)

newtype AuthorLogin =
    AuthorLogin
        { author_login :: T.Text
        }
    deriving (Show)

instance ToField AuthorLogin where
    toField = toField . author_login

data CreateAuthor =
    CreateAuthor
        { create_author_login :: Maybe Login
        , create_author_description :: Maybe T.Text
        }
    deriving (Show, Generic, ToRow)

newtype TagName =
    TagName
        { from_tag_name :: T.Text
        }
    deriving (Show)

instance ToField TagName where
    toField = toField . from_tag_name

data EditTag =
    EditTag
        { edit_tag_new_name :: Maybe TagName
        , edit_tag_old_name :: Maybe TagName
        }
    deriving (Show, Generic, ToRow)

newtype CategoryName =
    CategoryName
        { from_category_name :: T.Text
        }
    deriving (Show, Eq)

instance ToField CategoryName where
    toField = toField . from_category_name

data CreateCategory =
    CreateCategory
        { create_categrory_name :: Maybe CategoryName
        , create_categrory_maternal_category :: Maybe CategoryName
        }
    deriving (Show, Generic, ToRow)

data EditCategory =
    EditCategory
        { edit_category_name :: Maybe CategoryName
        , edit_category_new_name :: Maybe CategoryName
        , edit_category_new_maternal :: Maybe CategoryName
        }
    deriving (Show, Generic, ToRow)

newtype Password =
    Password
        { from_password :: T.Text
        }
    deriving (Show)

instance ToField Password where
    toField = toField . from_password

newtype Login =
    Login
        { from_login :: T.Text
        }
    deriving (Show)

instance ToField Login where
    toField = toField . from_login

data CreateUser =
    CreateUser
        { avatar_file_name :: Maybe BC.ByteString
        , avatar_content :: Maybe (Binary LBS.ByteString)
        , avatar_content_type :: Maybe BC.ByteString
        , first_name :: Maybe T.Text
        , last_name :: Maybe T.Text
        , user_login :: Maybe Login
        , user_password :: Maybe Password
        , creation_date :: UTCTime
        , admin_mark :: Bool
        }
    deriving (Show, Generic, ToRow)

newtype CommentText =
    CommentText
        { from_comment_text :: T.Text
        }

instance ToField CommentText where
    toField = toField . from_comment_text

newtype Id =
    Id
        { from_id :: Int
        }
    deriving (Show)

instance ToField Id where
    toField = toField . from_id

newtype Sort =
    Sort
        { from_sort :: BC.ByteString
        }
    deriving (Show)

newtype TagInFilterParam =
    TagInFilterParam
        { from_tag_in_fp :: [Int]
        }

newtype CategoryFilterParam =
    CategoryFilterParam
        { from_category_fp :: Id
        }

newtype TagFilterParam =
    TagFilterParam
        { from_tag_fp :: Id
        }

instance ToField TagFilterParam where
    toField = toField . from_tag_fp

newtype TagAllFilterParam =
    TagAllFilterParam
        { from_tag_all_fp :: [Int]
        }

newtype TitleFilterParam =
    TitleFilterParam
        { from_title_fp :: T.Text
        }

newtype ContentFilterParam =
    ContentFilterParam
        { from_content_fp :: T.Text
        }

newtype DateFilterParam =
    DateFilterParam
        { from_date_fp :: Day
        }

instance ToField DateFilterParam where
    toField = toField . from_date_fp

newtype BeforeDateFilterParam =
    BeforeDateFilterParam
        { from_before_date_fp :: Day
        }

instance ToField BeforeDateFilterParam where
    toField = toField . from_before_date_fp

newtype AfterDateFilterParam =
    AfterDateFilterParam
        { from_after_date_fp :: Day
        }

instance ToField AfterDateFilterParam where
    toField = toField . from_after_date_fp

newtype AuthorFilterParam =
    AuthorFilterParam
        { from_author_fp :: T.Text
        }

type ParamName = BC.ByteString
{-myLookup "tag_in" queryParams <|> myLookup "category" queryParams <|>
        myLookup "tag" queryParams <|>
        myLookup "tag_all" queryParams <|>
        myLookup "author" queryParams <|>
        myLookup "title" queryParams <|>
        myLookup "content" queryParams <|>
        myLookup "date" queryParams <|>
        myLookup "after_date" queryParams <|>
        myLookup "before_date" queryParams-}
