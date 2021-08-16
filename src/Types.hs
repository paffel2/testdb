{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
module Types where
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
--import Control.Monad
--import Control.Applicative
--import Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
--import qualified Data.Text.IO as TIO
import GHC.Generics
import Data.Time
import Data.Aeson


data Comment = Comment { comment_token :: T.Text ,
                         comment_token_lifetime :: Int,
                         comment_text :: T.Text,
                         comment_news_id :: Int,
                         comment_time :: UTCTime
                         } deriving (Show,Generic, ToRow, FromRow)


data GetNews' = GetNews' {  news_id' :: Int ,
                            short_title' :: T.Text ,
                            date_creation' :: Day,
                            author_name :: T.Text,
                            category_name' :: T.Text,
                            news_text' :: T.Text
                       } deriving (Show, Generic, ToRow, FromRow)


instance ToJSON GetNews' where


newtype NewsArray' = NewsArray' { news' :: [GetNews']} deriving (Show, Generic)

instance ToJSON NewsArray' where
    toJSON = genericToJSON defaultOptions

data Comment' = Comment' { comment_author_name :: T.Text,
                              comment_text' :: T.Text,
                              comment_time' :: UTCTime,
                              comment_id' :: Int
                            } deriving (Show, Generic, ToRow, FromRow)

newtype CommentArray = CommentArray { comments :: [Comment']} deriving (Show, Generic)
instance ToJSON Comment' where

instance ToJSON CommentArray where
    toJSON = genericToJSON defaultOptions


data CheckToken = CheckToken { ct_user_id :: Int,
                               ct_creation_date :: UTCTime
                               } deriving (Show, Generic, ToRow, FromRow)


newtype Category' = Category' { category_name'' :: T.Text } deriving (Show, Generic, ToRow, FromRow)
instance ToJSON Category' where
    toJSON = genericToJSON defaultOptions

newtype ListOfCategories = ListOfCategories { categories' :: [Category']} deriving (Show, Generic)
instance ToJSON ListOfCategories where
    toJSON = genericToJSON defaultOptions


data Draft' = Draft' { draft_short_title' :: T.Text,
                     date_of_changes' :: UTCTime,
                     draft_category_id' :: Maybe Int,
                     draft_text' :: Maybe T.Text,
                     draft_main_image_id' :: Maybe Int,
                     draft_images :: Maybe (PGArray Int)
                     } deriving (Show,Generic,FromRow)
instance ToJSON Draft'  where
    toJSON (Draft' dst doc dci dt dmii di) = 
        object ["draft_short_title'" .= dst,
                "date_of_changes'" .= doc,
                "draft_category_id'" .= dci,
                "draft_text'" .= dt,
                "draft_main_image_id'" .= dmii,
                "draft_images" .= (fromPGArray <$> di)]

newtype DraftArray = DraftArray {drafts :: [Draft']} deriving (Show, Generic)
instance ToJSON DraftArray where
    toJSON = genericToJSON defaultOptions


data Image''' = Image''' { f_name'' :: BC.ByteString,
                     content_type'' :: BC.ByteString,
                     content'' :: Binary LBS.ByteString 
                   } deriving (Show, Generic, ToRow, FromRow)



data Profile = Profile { profile_first_name :: Maybe T.Text,
                         profile_last_name :: Maybe T.Text,
                         profile_avatar :: Maybe Int
                         } deriving (Show, Generic, ToRow, FromRow)

instance ToJSON Profile where
    toJSON (Profile pfn pln pa) = 
        object ["profile_first_name" .= pfn,
                "profile_last_name" .= pln,
                "profile_avatar" .= pa]

data TokenProfile = TokenProfile { token :: T.Text,
                    token_lifetime' :: Int } deriving (Show, Generic, ToRow, FromRow)

type DatabaseAddress = BC.ByteString

type TokenLifeTime = Int



data DeleteComment = DeleteComment {delc_token :: T.Text,
                                    delc_token_lifetime :: TokenLifeTime,
                                    delc_comment_id :: Int 
                                    } deriving (Show, Generic, ToRow, FromRow)

newtype Tag = Tag { tag_name :: T.Text} deriving (Show, Generic, ToRow, FromRow)
instance ToJSON Tag where
    toJSON = genericToJSON defaultOptions

newtype TagsList = TagsList { tags :: [Tag]} deriving (Show, Generic)
instance ToJSON TagsList where
    toJSON = genericToJSON defaultOptions

