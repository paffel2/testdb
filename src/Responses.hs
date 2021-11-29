{-# LANGUAGE OverloadedStrings #-}

module Responses where

import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Internal   as BI
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as E
import           Database.PostgreSQL.Simple (Binary (fromBinary))
import           Logger
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

{-badResponse :: LBS.ByteString -> SomeError -> Response
badResponse prefix BadToken =
    responseForbidden $ LBS.concat [prefix, " Bad Token."]
badResponse prefix NotAdmin =
    responseForbidden $ LBS.concat [prefix, " Not admin."]
badResponse prefix DatabaseError =
    responseInternalServerError $ LBS.concat [prefix, " Database Error."]
badResponse prefix BadMethod =
    responseMethodNotAllowed $ LBS.concat [prefix, " Bad method request."]
badResponse prefix (OtherError message) =
    responseBadRequest $ LBS.concat [prefix, lbsMessage]
  where
    lbsMessage = LBS.fromStrict $ BC.pack message -}
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

toResponseErrorMessage ::
       Monad m
    => LoggerHandle m
    -> T.Text
    -> SomeError
    -> m ResponseErrorMessage
toResponseErrorMessage hLogger prefix BadToken = do
    logError hLogger $ prefix <> " Bad Token."
    return $
        Forbidden $
        LBS.concat [LBS.fromStrict $ E.encodeUtf8 prefix, " Bad Token."]
toResponseErrorMessage hLogger prefix NotAdmin = do
    logError hLogger $ prefix <> " Not Admin."
    return $
        Forbidden $
        LBS.concat [LBS.fromStrict $ E.encodeUtf8 prefix, " Not Admin."]
toResponseErrorMessage hLogger prefix BadMethod = do
    logError hLogger $ prefix <> " Bad method request."
    return $
        MethodNotAllowed $
        LBS.concat
            [LBS.fromStrict $ E.encodeUtf8 prefix, " Bad method request."]
toResponseErrorMessage hLogger prefix (OtherError message) = do
    logError hLogger $ prefix <> T.pack message
    return $
        BadRequest $
        LBS.concat
            [ LBS.fromStrict $ E.encodeUtf8 prefix
            , " "
            , LBS.fromStrict $ BC.pack message
            ]
toResponseErrorMessage hLogger prefix (DatabaseError code) = do
    logError hLogger $ prefix <> T.pack (show code)
    return $
        InternalServerError $
        LBS.concat
            [ LBS.fromStrict $ E.encodeUtf8 prefix
            , " Database Error "
            , LBS.fromStrict $ BC.pack (show code)
            , "."
            ]
