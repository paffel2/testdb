module Types.Other where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import Database.PostgreSQL.Simple.ToField (ToField(..))

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

newtype Id =
    Id
        { from_id :: Int
        }
    deriving (Show)

instance ToField Id where
    toField = toField . from_id

type ErrorMessage = LBS.ByteString

type SuccessMessage = LBS.ByteString

type SendId = Int
