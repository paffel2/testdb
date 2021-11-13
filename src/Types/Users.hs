{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Users where

import           Data.Aeson                         (KeyValue ((.=)),
                                                     ToJSON (toJSON), object)
import qualified Data.ByteString.Char8              as BC
import qualified Data.ByteString.Lazy               as LBS
import qualified Data.Text                          as T
import           Database.PostgreSQL.Simple         (Binary, FromRow, ToRow)
import           Database.PostgreSQL.Simple.ToField (ToField (..))
import           GHC.Generics                       (Generic)
import           Types.Other                        (Token, TokenLifeTime)

data Profile =
    Profile
        { profileFirstName :: Maybe T.Text
        , profileLastName  :: Maybe T.Text
        , profileAvatar    :: Maybe Int
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
        { profileToken         :: Token
        , profileTokenLifeTime :: TokenLifeTime
        }
    deriving (Show, Generic, ToRow)

newtype Password =
    Password
        { getPassword :: T.Text
        }
    deriving (Show, Eq)
    deriving ToField via T.Text

newtype Login =
    Login
        { getLogin :: T.Text
        }
    deriving (Show, Eq) 
    deriving ToField via T.Text

data CreateUser =
    CreateUser
        { cuAvatarFileName    :: Maybe BC.ByteString
        , cuAvatarContent     :: Maybe (Binary LBS.ByteString)
        , cuAvatarContentType :: Maybe BC.ByteString
        , cuFirstName         :: Maybe T.Text
        , cuLastName          :: Maybe T.Text
        , cuUserLogin         :: Maybe Login
        , cuUserPassword      :: Maybe Password
        , cuAdminMark         :: Bool
        }
    deriving (Show, Generic, ToRow, Eq)

data AdminData =
    AdminData
        { adminLogin     :: Maybe Login
        , adminPassword  :: Maybe Password
        , adminFirstName :: Maybe T.Text
        , adminLastName  :: Maybe T.Text
        , adminMark      :: Bool
        }
    deriving (Show, Generic, ToRow)
