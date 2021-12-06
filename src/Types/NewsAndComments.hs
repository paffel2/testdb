{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.NewsAndComments where

import           Data.Aeson                         (KeyValue ((.=)),
                                                     Options (fieldLabelModifier),
                                                     ToJSON (toJSON), camelTo2,
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
        { eofnaNewsId       :: Int
        , eofnaShortTitle   :: T.Text
        , eofnaDateCreation :: Day
        , eofnaAuthorName   :: T.Text
        , eofnaCategoryName :: T.Text
        , eofnaNewsText     :: T.Text
        }
    deriving (Show, Generic, ToRow, FromRow)

instance ToJSON ElemOfNewsArray where
    toJSON =
        genericToJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 5}

newtype NewsArray =
    NewsArray
        { news :: [ElemOfNewsArray]
        }
    deriving (Show, Generic)

instance ToJSON NewsArray where
    toJSON = genericToJSON defaultOptions

data ElemOfCommentArray =
    ElemOfCommentArray
        { eofcaCommentAuthorName :: T.Text
        , eofcaCommentText       :: T.Text
        , eofcaCommentTime       :: UTCTime
        , eofcaCommentId         :: Int
        }
    deriving (Show, Generic, ToRow, FromRow)

newtype CommentArray =
    CommentArray
        { comments :: [ElemOfCommentArray]
        }
    deriving (Show, Generic)

instance ToJSON ElemOfCommentArray where
    toJSON =
        genericToJSON
            defaultOptions {fieldLabelModifier = camelTo2 '_' . drop 5}

instance ToJSON CommentArray where
    toJSON = genericToJSON defaultOptions

data GetNews =
    GetNews
        { gnNewsId          :: Int
        , gnShortTitle      :: T.Text
        , gnDateCreation    :: Day
        , gnAuthorName      :: T.Text
        , gnCategoryName    :: T.Text
        , gnNewsText        :: T.Text
        , gnNewsMainImage   :: Maybe Int
        , gnNewsOtherImages :: Maybe (PGArray Int)
        , gnNewsTags        :: Maybe (PGArray T.Text)
        }
    deriving (Show, Generic, ToRow, FromRow)

instance ToJSON GetNews where
    toJSON (GetNews gnni gnst gndc gnan gncn gnnt gnnmi gnnoi gnnts) =
        object
            [ "news_id" .= gnni
            , "short_title" .= gnst
            , "date_creation" .= gndc
            , "author_name" .= gnan
            , "category_name" .= gncn
            , "news_text" .= gnnt
            , "news_main_image" .= gnnmi
            , "news_other_images" .= (fromPGArray <$> gnnoi)
            , "news_tags" .= (fromPGArray <$> gnnts)
            ]

newtype CommentText =
    CommentText
        { getCommentText :: T.Text
        }
    deriving (Show, Eq)
    deriving ToField via T.Text

newtype Sort =
    Sort
        { getSort :: BC.ByteString
        }
    deriving (Show, Eq)

newtype TagInFilterParam =
    TagInFilterParam
        { getTagInFp :: [Int]
        }
    deriving (Show, Eq)

newtype CategoryFilterParam =
    CategoryFilterParam
        { getCategoryFp :: Id
        }
    deriving (Show, Eq)

newtype TagFilterParam =
    TagFilterParam
        { getTagFp :: Id
        }
    deriving (Show, Eq)
    deriving ToField via Id

newtype TagAllFilterParam =
    TagAllFilterParam
        { getTagAllFp :: [Int]
        }
    deriving (Show, Eq)

newtype TitleFilterParam =
    TitleFilterParam
        { getTitleFp :: T.Text
        }
    deriving (Show, Eq)

newtype ContentFilterParam =
    ContentFilterParam
        { getContentFp :: T.Text
        }
    deriving (Show, Eq)

newtype DateFilterParam =
    DateFilterParam
        { getDateFp :: Day
        }
    deriving (Show, Eq)
    deriving ToField via Day

newtype BeforeDateFilterParam =
    BeforeDateFilterParam
        { getBeforeDateFp :: Day
        }
    deriving (Show, Eq)
    deriving ToField via Day

newtype AfterDateFilterParam =
    AfterDateFilterParam
        { getAfterDateFp :: Day
        }
    deriving (Show, Eq)
    deriving ToField via Day

newtype AuthorFilterParam =
    AuthorFilterParam
        { getAuthorFp :: T.Text
        }
    deriving (Show, Eq)

data CommentWithoutTokenLifeTime =
    CommentWithoutTokenLifeTime
        { commentWTLToken  :: Maybe Token
        , commentWTLText   :: Maybe CommentText
        , commentWTLNewsId :: Maybe Id
        }
    deriving (Generic, ToRow)

toComment :: TokenLifeTime -> CommentWithoutTokenLifeTime -> Comment
toComment tokenLifeTime comm =
    Comment
        { comment_token = commentWTLToken comm
        , comment_token_lifetime = tokenLifeTime
        , comment_text = commentWTLText comm
        , comment_news_id = commentWTLNewsId comm
        }

data FilterParams
    = TagIn (Maybe TagInFilterParam)
    | CategoryFilter (Maybe CategoryFilterParam)
    | TagFilter (Maybe TagFilterParam)
    | TagAll (Maybe TagAllFilterParam)
    | TitleFilter (Maybe TitleFilterParam)
    | ContentFilter (Maybe ContentFilterParam)
    | DateFilter (Maybe DateFilterParam)
    | BeforeDate (Maybe BeforeDateFilterParam)
    | AfterDate (Maybe AfterDateFilterParam)
    | AuthorFilter (Maybe AuthorFilterParam)
    | NoFilter
    | BadFilter
