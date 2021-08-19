{-# LANGUAGE OverloadedStrings #-}
module Responses where
import Network.Wai ( Response, responseLBS )
import Network.HTTP.Types
    ( Status, hContentType, badRequest400, notFound404, status200 )
import qualified Data.ByteString.Lazy as LBS

responseOk, responseNotFound, responseBadRequest :: LBS.ByteString -> Response

responseOk = responsePlainText status200
responseNotFound = responsePlainText notFound404
responseBadRequest = responsePlainText badRequest400

responsePlainText :: Status -> LBS.ByteString -> Response
responsePlainText = (`responseLBS` [(hContentType, "text/plain")])