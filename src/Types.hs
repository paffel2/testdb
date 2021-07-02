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

data User = User { first_name :: Maybe String,
                   last_name :: Maybe String,
                   avatar :: Maybe B.ByteString,
                   login :: String,
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