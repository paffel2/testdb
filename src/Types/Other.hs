{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia   #-}

module Types.Other where

import qualified Data.ByteString.Lazy               as LBS
import qualified Data.Text                          as T
import           Database.PostgreSQL.Simple.ToField (ToField (..))
import           Types.Images                       (ImageB)
import Control.Monad.Except ( ExceptT )

newtype TokenLifeTime =
    TokenLifeTime
        { getTokenLifeTime :: Int
        } 
        deriving Show via Int
        deriving ToField via Int

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

newtype Id =
    Id
        { getId :: Int
        }
    deriving (Show, Eq)
    deriving ToField via Int

data SomeError
    = BadToken
    | NotAdmin
    | DatabaseError
    | BadMethod
    | OtherError String
    | DatabaseErrorNew Int

someErrorToInt :: SomeError -> Int
someErrorToInt (DatabaseErrorNew n) = n
someErrorToInt _ = 0

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


type MonadWithError = ExceptT SomeError IO



{-data SomeErrorNew
    = BadTokenNew
    | NotAdminNew
    | DatabaseErrorNew Int
    | BadMethodNew
    | OtherErrorNew String -}