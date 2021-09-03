{-# LANGUAGE OverloadedStrings #-}

module Controllers.Authors where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Pool (Pool)
import qualified Data.Text.Encoding as E
import Database.PostgreSQL.Simple (Connection)
import Databaseoperations.Authors (createAuthorInDb, deleteAuthorInDb)
import FromRequest (takeToken)
import Logger (Handle, logError)
import Network.HTTP.Types.Method (methodDelete, methodPost)
import Network.Wai (Request(requestMethod), Response)
import Network.Wai.Parse (lbsBackEnd, parseRequestBody)
import Responses (responseBadRequest, responseOk)
import Types (TokenLifeTime)

newAuthor ::
       Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
newAuthor hLogger pool token_lifetime req =
    if requestMethod req /= methodPost
        then do
            logError hLogger "Bad request method"
            return $ responseBadRequest "Bad request method"
        else do
            let token' = E.decodeUtf8 <$> takeToken req
            (i, _) <- parseRequestBody lbsBackEnd req
            let author_login = E.decodeUtf8 <$> lookup "author_login" i
            let description = E.decodeUtf8 <$> lookup "description" i
            result <-
                createAuthorInDb
                    hLogger
                    pool
                    token_lifetime
                    token'
                    author_login
                    description
            case result of
                Left bs -> return $ responseBadRequest bs
                Right n ->
                    return $ responseOk $ LBS.fromStrict $ BC.pack $ show n

deleteAuthor ::
       Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
deleteAuthor hLogger pool token_lifetime req =
    if requestMethod req /= methodDelete
        then do
            logError hLogger "Bad request method"
            return $ responseBadRequest "Bad request method"
        else do
            let token' = E.decodeUtf8 <$> takeToken req
            (i, _) <- parseRequestBody lbsBackEnd req
            let author_login = E.decodeUtf8 <$> lookup "author_login" i
            result <-
                deleteAuthorInDb hLogger pool token_lifetime token' author_login
            case result of
                Left bs -> return $ responseBadRequest bs
                Right bs -> return $ responseOk bs
