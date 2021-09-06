{-# LANGUAGE OverloadedStrings #-}

module Controllers.Tags where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Database.PostgreSQL.Simple (Connection)
import Databaseoperations.Tags
    ( createTagInDb
    , deleteTagFromDb
    , editTagInDb
    , getTagsListFromDb
    )
import FromRequest (takeToken)
import Logger (Handle, logError)
import Network.HTTP.Types.Method
    ( methodDelete
    , methodGet
    , methodPost
    , methodPut
    )
import Network.Wai (Request(queryString, rawPathInfo, requestMethod), Response)
import Network.Wai.Parse (lbsBackEnd, parseRequestBody)
import Responses
    ( responseBadRequest
    , responseCreated
    , responseForbidden
    , responseMethodNotAllowed
    , responseNotFound
    , responseOKJSON
    , responseOk
    )
import Types (TokenLifeTime)

sendTagsList :: Handle -> Pool Connection -> Request -> IO Response
sendTagsList hLogger pool req =
    if requestMethod req /= methodGet
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            tags_list <- getTagsListFromDb hLogger pool page
            case tags_list of
                Left bs -> return $ responseBadRequest bs
                Right tl -> return $ responseOKJSON $ encode tl
  where
    page = fromMaybe Nothing (lookup "page" $ queryString req)

newTag :: Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
newTag hLogger pool token_lifetime req =
    if requestMethod req /= methodPost
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            let token' = E.decodeUtf8 <$> takeToken req
            let tag_name_param =
                    T.toLower . E.decodeUtf8 <$>
                    fromMaybe Nothing (lookup "tag_name" $ queryString req)
            result <-
                createTagInDb hLogger pool token_lifetime token' tag_name_param
            case result of
                Left "Not admin" -> return $ responseForbidden "Not admin"
                Left "Bad token" -> return $ responseForbidden "Bad token"
                Left bs -> return $ responseBadRequest bs
                Right n ->
                    return $ responseCreated $ LBS.fromStrict $ BC.pack $ show n

deleteTag ::
       Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
deleteTag hLogger pool token_lifetime req =
    if requestMethod req /= methodDelete
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            let token' = E.decodeUtf8 <$> takeToken req
            let tag_name_param =
                    T.toLower . E.decodeUtf8 <$>
                    fromMaybe Nothing (lookup "tag_name" $ queryString req)
            result <-
                deleteTagFromDb
                    hLogger
                    pool
                    token_lifetime
                    token'
                    tag_name_param
            case result of
                Left "Not admin" -> return $ responseForbidden "Not admin"
                Left "Bad token" -> return $ responseForbidden "Bad token"
                Left bs -> return $ responseBadRequest bs
                Right bs -> return $ responseOk bs

editTag :: Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
editTag hLogger pool token_lifetime req =
    if requestMethod req /= methodPut
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            let token' = E.decodeUtf8 <$> takeToken req
            (i, _) <- parseRequestBody lbsBackEnd req
            let old_tag_name = E.decodeUtf8 <$> lookup "old_tag_name" i
            let new_tag_name = E.decodeUtf8 <$> lookup "new_tag_name" i
            result <-
                editTagInDb
                    hLogger
                    pool
                    token_lifetime
                    token'
                    old_tag_name
                    new_tag_name
            case result of
                Left "Not admin" -> return $ responseForbidden "Not admin"
                Left "Bad token" -> return $ responseForbidden "Bad token"
                Left bs -> return $ responseBadRequest bs
                Right bs -> return $ responseOk bs

tagsBlock ::
       Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
tagsBlock hLogger pool token_lifetime req
    | pathElemsC == 1 = sendTagsList hLogger pool req
    | pathElemsC == 2 =
        case last pathElems of
            "create_tag" -> newTag hLogger pool token_lifetime req
            "delete_tag" -> deleteTag hLogger pool token_lifetime req
            "edit_tag" -> editTag hLogger pool token_lifetime req
            _ -> return $ responseNotFound "Not Found"
    | otherwise = return $ responseNotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
