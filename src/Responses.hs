{-# LANGUAGE OverloadedStrings #-}
module Responses where
import Network.Wai
--import Network.Wai.Handler.Warp
--import Network.Wai.Parse
import Network.HTTP.Types
--import GHC.Generics
--import Data.Aeson
--import Testdatabase
--import Control.Exception
--import qualified Data.Text.Lazy.Encoding as EL
--import qualified Data.Text.Lazy.IO as TLIO
--import qualified Data.Text.Encoding as E
--import qualified Data.Text as T
--import qualified Data.Text.IO as TIO
--import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
--import qualified Data.ByteString.Char8 as BC
--import Types
--import Data.Maybe
--import Text.Read
--import Control.Applicative

responseOk, responseNotFound, responseBadRequest :: LBS.ByteString -> Response

responseOk = responsePlainText status200
responseNotFound = responsePlainText notFound404
responseBadRequest = responsePlainText badRequest400

responsePlainText :: Status -> LBS.ByteString -> Response
responsePlainText = (`responseLBS` [(hContentType, "text/plain")])