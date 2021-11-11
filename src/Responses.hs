{-# LANGUAGE OverloadedStrings #-}

module Responses where

import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Internal   as BI
import qualified Data.ByteString.Lazy       as LBS
import           Database.PostgreSQL.Simple (Binary (fromBinary))
import           Network.HTTP.Types         (Status, badRequest400,
                                             forbidden403, hContentType,
                                             internalServerError500,
                                             methodNotAllowed405, notFound404,
                                             status200, status201)
import           Network.Wai                (Response, responseLBS)
import           Types.Images               (ImageB (conType, imageB))
import           Types.Other                (ResponseErrorMessage (..),
                                             ResponseOkMessage (..),
                                             SomeError (..))

responseOk, responseNotFound, responseBadRequest, responseCreated, responseForbidden, responseMethodNotAllowed ::
       LBS.ByteString -> Response
responseOk = responsePlainText status200

responseNotFound = responsePlainText notFound404

responseBadRequest = responsePlainText badRequest400

responseCreated = responsePlainText status201

responseForbidden = responsePlainText forbidden403

responseMethodNotAllowed = responsePlainText methodNotAllowed405

responseInternalServerError :: LBS.ByteString -> Response
responseInternalServerError = responsePlainText internalServerError500

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

badResponse :: LBS.ByteString -> SomeError -> Response
badResponse prefix BadToken =
    responseForbidden $ LBS.concat [prefix, " Bad Token."]
badResponse prefix NotAdmin =
    responseForbidden $ LBS.concat [prefix, " Not admin."]
badResponse prefix DatabaseError =
    responseInternalServerError $ LBS.concat [prefix, " Database Error."]
badResponse prefix (OtherError message) =
    responseBadRequest $ LBS.concat [prefix, lbsMessage]
  where
    lbsMessage = LBS.fromStrict $ BC.pack message

toResponse :: Either ResponseErrorMessage ResponseOkMessage -> Response
toResponse (Left (Forbidden message)) = responseForbidden message
toResponse (Left (MethodNotAllowed message)) = responseMethodNotAllowed message
toResponse (Left (InternalServerError message)) =
    responseInternalServerError message
toResponse (Left (BadRequest message)) = responseBadRequest message
toResponse (Left (NotFound message)) = responseNotFound message
toResponse (Right (Created message)) = responseCreated message
toResponse (Right (OkJSON someJson)) = responseOKJSON someJson
toResponse (Right (OkMessage message)) = responseOk message
toResponse (Right (OkImage image)) =
    responseOKImage (conType image) (fromBinary $ imageB image)

toResponseErrorMessage :: LBS.ByteString -> SomeError -> ResponseErrorMessage
toResponseErrorMessage prefix BadToken =
    Forbidden $ LBS.concat [prefix, " Bad Token."]
toResponseErrorMessage prefix NotAdmin =
    Forbidden $ LBS.concat [prefix, " Not Admin."]
toResponseErrorMessage prefix DatabaseError =
    InternalServerError $ LBS.concat [prefix, " Database Error."]
toResponseErrorMessage prefix (OtherError message) =
    BadRequest $ LBS.concat [prefix, " ", lbsMessage]
  where
    lbsMessage = LBS.fromStrict $ BC.pack message
