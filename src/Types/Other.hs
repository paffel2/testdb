module Types.Other where

import qualified Data.Text                          as T
import           Database.PostgreSQL.Simple.ToField (ToField (..))

newtype TokenLifeTime =
    TokenLifeTime
        { token_life_time :: Int
        }

instance Show TokenLifeTime where
    show (TokenLifeTime tk) = show tk

instance ToField TokenLifeTime where
    toField = toField . token_life_time

newtype Page =
    Page
        { from_page :: Int
        }
    deriving (Show, Eq)

newtype Token =
    Token
        { from_token :: T.Text
        }
    deriving (Show, Eq)

instance ToField Token where
    toField = toField . from_token

newtype Id =
    Id
        { from_id :: Int
        }
    deriving (Show, Eq)

instance ToField Id where
    toField = toField . from_id

data SomeError
    = BadToken
    | NotAdmin
    | DatabaseError
    | OtherError String

type SendId = Int
