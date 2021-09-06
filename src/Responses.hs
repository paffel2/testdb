{-# LANGUAGE OverloadedStrings #-}

module Responses where

import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Types
    ( Status
    , badRequest400
    , forbidden403
    , hContentType
    , methodNotAllowed405
    , notFound404
    , status200
    , status201
    )
import Network.Wai (Response, responseLBS)

responseOk, responseNotFound, responseBadRequest, responseCreated, responseForbidden, responseMethodNotAllowed ::
       LBS.ByteString -> Response
responseOk = responsePlainText status200

responseNotFound = responsePlainText notFound404

responseBadRequest = responsePlainText badRequest400

responseCreated = responsePlainText status201

responseForbidden = responsePlainText forbidden403

responseMethodNotAllowed = responsePlainText methodNotAllowed405

responsePlainText :: Status -> LBS.ByteString -> Response
responsePlainText = (`responseLBS` [(hContentType, "text/plain")])

responseJSON :: Status -> LBS.ByteString -> Response
responseJSON = (`responseLBS` [(hContentType, "application/json")])

responseOKJSON :: LBS.ByteString -> Response
responseOKJSON = responseJSON status200

responseImage :: BI.ByteString -> Status -> LBS.ByteString -> Response
responseImage contype = (`responseLBS` [(hContentType, contype)])

responseOKImage :: BI.ByteString -> LBS.ByteString -> Response
responseOKImage contype = responseImage contype status200
--responseCreated :: LBS.ByteString -> Response
