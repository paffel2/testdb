{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}
module Types where
import Database.PostgreSQL.Simple
import Control.Monad
import Control.Applicative
import Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
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


data Author = Author { author_user_id :: Int,
                       description :: Maybe String
                       } deriving (Show, Generic, ToRow, FromRow)


data News = News { short_title :: String,
                   date_creation :: UTCTime,
                   author_id :: Int,
                   category_id :: Int,
                   news_text :: T.Text,
                   main_image :: Binary B.ByteString 
                 } deriving (Show, Generic, ToRow, FromRow)

data Category = Category { category_name :: String,
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




 



