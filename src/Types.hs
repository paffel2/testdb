{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
module Types where
import Database.PostgreSQL.Simple
import Control.Monad
import Control.Applicative
import Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics
import Data.Time
import Data.Aeson
import Database.PostgreSQL.Simple.Time

data User = User { first_name :: Maybe String,
                   last_name :: Maybe String,
                   avatar :: Maybe B.ByteString,
                   user_login :: String,
                   user_password :: String,
                   creation_date :: UTCTime,
                   admin_mark :: Bool } deriving (Show, Generic, ToRow, FromRow)
data User' = User' { first_name' :: Maybe T.Text,
                   last_name' :: Maybe T.Text,
                   avatar' :: Maybe Int,
                   login' :: T.Text,
                   user_password' :: T.Text,
                   creation_date' :: UTCTime,
                   admin_mark' :: Bool } deriving (Show, Generic, ToRow, FromRow)


data Author = Author { author_user_id :: Int,
                       description :: Maybe T.Text
                       } deriving (Show, Generic, ToRow, FromRow)


data News = News { short_title :: String,
                   date_creation :: UTCTime,
                   author_id :: Int,
                   category_id :: Int,
                   news_text :: T.Text,
                   main_image :: Binary B.ByteString 
                 } deriving (Show, Generic, ToRow, FromRow)

data Category = Category { category_name :: T.Text,
                           maternal_category :: Maybe Int 
                         } deriving (Show, Generic, ToRow, FromRow)
data GetNews = GetNews { title :: T.Text ,
                         news_id :: Int 
                       } deriving (Show, Generic, ToRow, FromRow)
instance ToJSON GetNews where
    toJSON = genericToJSON defaultOptions

newtype NewsArray = NewsArray {news :: [GetNews]} deriving (Show, Generic)

instance ToJSON NewsArray where
    toJSON = genericToJSON defaultOptions

data FindNewsByTitle = FindNewsByTitle { f_title :: T.Text,
                                         page :: Int  } deriving (Show, Generic, ToRow, FromRow)

data Image = Image { f_name :: String,
                     content_type :: String,
                     content :: Binary LBS.ByteString 
                   } deriving (Show, Generic, ToRow, FromRow)


newtype Myid = Myid { my_id :: Int } deriving (Generic, ToRow, FromRow)
instance Show Myid where
    show (Myid x) = show x
myidtointlst :: [Myid] -> [Int]
myidtointlst ((Myid x):xs) = x : myidtointlst xs
myidtointlst [] = []
newtype Login = Login { login'' :: T.Text } deriving (Generic, ToRow, FromRow)

newtype Tag = Tag { tag_name :: T.Text } deriving (Show,Generic, ToRow, FromRow)

--newtype TagList = TagList { tag_list :: [Tag] } deriving (Show,Generic, ToRow, FromRow)


data Draft = Draft { author_id' :: Int,
                     draft_short_title :: T.Text,
                     date_of_changes :: UTCTime,
                     draft_category_id :: Int,
                     draft_text :: T.Text,
                     main_image_id :: Int 
                     } deriving (Show,Generic, ToRow, FromRow)

data DrIdTgId = DrIdTgId { draft_id' :: Int,
                           tag_id' :: Int
                           } deriving (Show,Generic, ToRow, FromRow)



data DridImId = DridImId { draft_id'' :: Int,
                           image_id'' :: Int 
                           } deriving (Show,Generic, ToRow, FromRow)

data Comment = Comment { comment_user_id :: Int,
                         comment_text :: T.Text,
                         comment_news_id :: Int,
                         comment_time :: UTCTime
                         } deriving (Show,Generic, ToRow, FromRow)




data FindNewsByTitle' = FindNewsByTitle' { f_title' :: BC.ByteString ,
                                         page' :: Int  } deriving (Show, Generic, ToRow, FromRow)



data GetNews' = GetNews' {  news_id' :: Int ,
                            short_title' :: T.Text ,
                            date_creation' :: Day,
                            --author_id'' :: Int,
                            author_name :: T.Text,
                            --author_last_name :: T.Text,
                            category_name' :: T.Text,
                            news_text' :: T.Text
                       } deriving (Show, Generic, ToRow, FromRow)


instance ToJSON GetNews' where
    --toJSON = genericToJSON defaultOptions
    {-toJSON (GetNews' ni st dc ai ci nt) = object [
                                                    "news_id'" .= ni,
                                                    "short_title'" .= st,
                                                    "date_creation'" .= dc,
                                                    "author_id''" .= ai,
                                                    "category_id'" .= ci,
                                                    "news_text'" .= nt
                                                    ]-}

newtype NewsArray' = NewsArray' { news' :: [GetNews']} deriving (Show, Generic)

instance ToJSON NewsArray' where
    toJSON = genericToJSON defaultOptions

data FindNewsByCategory = FindNewsByCategory { c_id :: Int,
                                         c_page :: Int  } deriving (Show, Generic, ToRow, FromRow)


{-data FindNewsByTagIn = FindNewsByTagIn { t_ids :: [Int],
                                         t_page :: Int  } deriving (Show, Generic, ToRow, FromRow)-}


data Comment' = Comment' { comment_author_name :: T.Text,
                              comment_text' :: T.Text,
                              comment_time' :: UTCTime,
                              comment_id' :: Int
                            } deriving (Show, Generic, ToRow, FromRow)

newtype CommentArray = CommentArray { comments :: [Comment']} deriving (Show, Generic)
instance ToJSON Comment' where

instance ToJSON CommentArray where
    toJSON = genericToJSON defaultOptions

                          
daytst = 
    do 
        now <- getCurrentTime 
        print $ utctDay now


--newtype Myid' = Myid' {}

--type Myid' = Maybe Int

data CheckToken = CheckToken { ct_user_id :: Int,
                               ct_creation_date :: UTCTime
                               } deriving (Show, Generic, ToRow, FromRow)


data Image' = Image' { f_name' :: String,
                     content' :: Binary LBS.ByteString,
                     content_type' :: String
                   } deriving (Show, Generic, ToRow, FromRow)


data User'' = User'' { first_name'' :: Maybe T.Text,
                   last_name'' :: Maybe T.Text,
                   avatar'' :: Maybe Int,
                   login''' :: T.Text,
                   user_password'' :: T.Text,
                   creation_date'' :: UTCTime,
                   admin_mark'' :: Bool,
                   get_user_id :: T.Text
                   } deriving (Show, Generic, ToRow, FromRow)


{-data Rt = Rt {     --u :: String,
                   f :: Maybe T.Text,
                   l :: Maybe T.Text,
                   a :: Maybe Int,
                   l' :: T.Text,
                   u :: T.Text,
                   d :: UTCTime,
                   m :: Bool,
                   ll :: T.Text} deriving (Show, Generic, ToRow, FromRow)

data T = T { p' :: T.Text,
            u_l :: T.Text 
            } deriving (Show, Generic, ToRow, FromRow)-}


newtype Category' = Category' { category_name'' :: T.Text } deriving (Show, Generic, ToRow, FromRow)
instance ToJSON Category' where
    toJSON = genericToJSON defaultOptions

newtype ListOfCategories = ListOfCategories { categories' :: [Category']} deriving (Show, Generic)
instance ToJSON ListOfCategories where
    toJSON = genericToJSON defaultOptions


data EditCategoryName = EditCategoryName { 
                                   new_name ::  T.Text,
                                   edit_category_name ::T.Text } deriving (Show, Generic, ToRow, FromRow)

data EditCategoryMaternal = EditCategoryMaternal { 
                                   new_maternal ::  Int,
                                   category_name''' ::T.Text } deriving (Show, Generic, ToRow, FromRow)

data EditCategory = EditCategory { 
                                   new_name' ::  T.Text,
                                   new_maternal' :: Int,
                                   edit_category_name' ::T.Text } deriving (Show, Generic, ToRow, FromRow)