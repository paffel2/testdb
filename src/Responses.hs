{-# LANGUAGE OverloadedStrings #-}

module Responses where

import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Types
    ( Status
    , badRequest400
    , hContentType
    , notFound404
    , status200
    )
import Network.Wai (Response, responseLBS)

responseOk, responseNotFound, responseBadRequest :: LBS.ByteString -> Response
responseOk = responsePlainText status200

responseNotFound = responsePlainText notFound404

responseBadRequest = responsePlainText badRequest400

responsePlainText :: Status -> LBS.ByteString -> Response
responsePlainText = (`responseLBS` [(hContentType, "text/plain")])
