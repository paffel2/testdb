{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia   #-}

module Types.Other where

import qualified Data.ByteString.Lazy               as LBS
import qualified Data.Text                          as T
import           Database.PostgreSQL.Simple.ToField (ToField (..))
import           Types.Images                       (ImageB)

newtype TokenLifeTime =
    TokenLifeTime
        { getTokenLifeTime :: Int
        } 
        deriving Show via Int
        deriving ToField via Int

{-instance Show TokenLifeTime where
    show (TokenLifeTime tk) = show tk

instance ToField TokenLifeTime where
    toField = toField . getTokenLifeTime -}

newtype Page =
    Page
        { getPage :: Int
        }
    deriving (Show, Eq)

newtype Token =
    Token
        { getToken :: T.Text
        }
    deriving (Show, Eq)
    deriving ToField via T.Text

{-instance ToField Token where
    toField = toField . getToken -}

newtype Id =
    Id
        { getId :: Int
        }
    deriving (Show, Eq)
    deriving ToField via Int

{-instance ToField Id where
    toField = toField . getId -}

data SomeError
    = BadToken
    | NotAdmin
    | DatabaseError
    | OtherError String

data ResponseErrorMessage
    = Forbidden LBS.ByteString
    | MethodNotAllowed LBS.ByteString
    | InternalServerError LBS.ByteString
    | BadRequest LBS.ByteString
    | NotFound LBS.ByteString
    deriving (Show, Eq)

data ResponseOkMessage
    = Created LBS.ByteString
    | OkJSON LBS.ByteString
    | OkMessage LBS.ByteString
    | OkImage ImageB
    deriving (Show, Eq)

type SendId = Int
