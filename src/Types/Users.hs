{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Users where

import Data.Aeson (KeyValue((.=)), ToJSON(toJSON), object)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.PostgreSQL.Simple (Binary, FromRow, ToRow)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import GHC.Generics (Generic)
import Types.Other (Token, TokenLifeTime)

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
