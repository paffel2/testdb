{-# LANGUAGE OverloadedStrings #-}

module Controllers.NewsAndComments where

import Control.Applicative (Alternative((<|>)))
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import qualified Data.Text.Encoding as E
import Database.PostgreSQL.Simple (Connection)
import Databaseoperations.NewsAndComments
    ( addCommentToDb
    , deleteCommentFromDb
    , getCommentsByNewsIdFromDb
    , getNewsByIdFromDb
    , getNewsFilterByAfterDateFromDb
    , getNewsFilterByAuthorNameFromDb
    , getNewsFilterByBeforeDateFromDb
    , getNewsFilterByCategoryIdFromDb
    , getNewsFilterByContentFromDb
    , getNewsFilterByDateFromDb
    , getNewsFilterByTagAllFromDb
    , getNewsFilterByTagIdFromDb
    , getNewsFilterByTagInFromDb
    , getNewsFilterByTitleFromDb
    , getNewsFromDb
    )
import FromRequest (takeToken)
import HelpFunction (myLookup)
import Logger (Handle, logError)
import Network.HTTP.Types.Method (methodDelete, methodGet, methodPost)
import Network.Wai (Request(queryString, rawPathInfo, requestMethod), Response)
import Network.Wai.Parse
    ( lbsBackEnd
    , noLimitParseRequestBodyOptions
    , parseRequestBodyEx
    )
import Responses
    ( responseBadRequest
    , responseCreated
    , responseForbidden
    , responseMethodNotAllowed
    , responseNotFound
    , responseOKJSON
    , responseOk
    )
import Text.Read (readMaybe)
import Types (TokenLifeTime)

deleteCommentById ::
       Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
deleteCommentById hLogger pool token_lifetime req =
    if requestMethod req /= methodDelete
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad request method"
        else do
            let token' = E.decodeUtf8 <$> takeToken req
            let comment_id =
                    fromMaybe Nothing (lookup "comment_id" $ queryString req)
            let c_id' = read . BC.unpack <$> comment_id :: Maybe Int
            result <-
                deleteCommentFromDb hLogger pool token_lifetime token' c_id'
            case result of
                Left "Not admin" -> return $ responseForbidden "Not admin"
                Left "Bad token" -> return $ responseForbidden "Bad token"
                Left bs -> return $ responseBadRequest bs
                Right bs -> return $ responseOk bs

addCommentByNewsId ::
       Handle
    -> Pool Connection
    -> TokenLifeTime
    -> Request
    -> Maybe Int
    -> IO Response
addCommentByNewsId hLogger pool token_lifetime req news'_id =
    if requestMethod req /= methodPost
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad request method"
        else do
            (i, _) <-
                parseRequestBodyEx noLimitParseRequestBodyOptions lbsBackEnd req
            let token' = takeToken req
            let comment = lookup "comment_text" i
            result <-
                addCommentToDb
                    hLogger
                    pool
                    token_lifetime
                    (E.decodeUtf8 $ fromMaybe "" token')
                    news'_id
                    (E.decodeUtf8 <$> comment)
            case result of
                Left "Bad token" -> return $ responseForbidden "Bad token"
                Left bs -> return $ responseBadRequest bs
                Right bs -> return $ responseCreated bs

sendCommentsByNewsId ::
       Handle -> Pool Connection -> Request -> Maybe Int -> IO Response
sendCommentsByNewsId hLogger pool req news'_id =
    if requestMethod req /= methodGet
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad request method"
        else do
            let pageParam = fromMaybe Nothing (lookup "page" $ queryString req)
            result <- getCommentsByNewsIdFromDb hLogger pool news'_id pageParam
            case result of
                Left bs -> return $ responseBadRequest bs
                Right ca -> return $ responseOKJSON $ encode ca

sendNewsById :: Handle -> Pool Connection -> Request -> Maybe Int -> IO Response
sendNewsById hLogger pool req newsId =
    if requestMethod req /= methodGet
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad request method"
        else do
            result <- getNewsByIdFromDb hLogger pool newsId
            case result of
                Left bs -> return $ responseBadRequest bs
                Right gn -> return $ responseOKJSON $ encode gn

sendNews :: Handle -> Pool Connection -> Request -> IO Response
sendNews hLogger pool req =
    if requestMethod req == methodGet
        then do
            result <-
                case filterParamName of
                    Just "tag_in" ->
                        getNewsFilterByTagInFromDb
                            hLogger
                            pool
                            filterParam
                            pageParam
                    Just "category" ->
                        getNewsFilterByCategoryIdFromDb
                            hLogger
                            pool
                            filterParam
                            pageParam
                            sortParam'
                    Just "title" ->
                        getNewsFilterByTitleFromDb
                            hLogger
                            pool
                            filterParam
                            pageParam
                            sortParam'
                    Just "author" ->
                        getNewsFilterByAuthorNameFromDb
                            hLogger
                            pool
                            filterParam
                            pageParam
                            sortParam'
                    Just "date" ->
                        getNewsFilterByDateFromDb
                            hLogger
                            pool
                            filterParam
                            pageParam
                            sortParam'
                    Just "tag_all" ->
                        getNewsFilterByTagAllFromDb
                            hLogger
                            pool
                            filterParam
                            pageParam
                            sortParam'
                    Just "content" ->
                        getNewsFilterByContentFromDb
                            hLogger
                            pool
                            filterParam
                            pageParam
                            sortParam'
                    Just "after_date" ->
                        getNewsFilterByAfterDateFromDb
                            hLogger
                            pool
                            filterParam
                            pageParam
                            sortParam'
                    Just "before_date" ->
                        getNewsFilterByBeforeDateFromDb
                            hLogger
                            pool
                            filterParam
                            pageParam
                            sortParam'
                    Just "tag" ->
                        getNewsFilterByTagIdFromDb
                            hLogger
                            pool
                            filterParam
                            pageParam
                            sortParam'
                    Just _ -> do
                        logError hLogger "Bad request"
                        return $ Left "Bad request"
                    Nothing -> getNewsFromDb hLogger pool sortParam' pageParam
            case result of
                Left bs -> return $ responseBadRequest bs
                Right na -> return $ responseOKJSON $ encode na
        else do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad request method"
  where
    queryParams = queryString req
    (_, fstParam) = head queryParams
    pageParam = fromMaybe Nothing (lookup "page" queryParams)
    sortParam = fromMaybe Nothing (lookup "sort" queryParams)
    filterParamName =
        myLookup "tag_in" queryParams <|> myLookup "category" queryParams <|>
        myLookup "tag" queryParams <|>
        myLookup "tag_all" queryParams <|>
        myLookup "author" queryParams <|>
        myLookup "title" queryParams <|>
        myLookup "content" queryParams <|>
        myLookup "date" queryParams <|>
        myLookup "after_date" queryParams <|>
        myLookup "before_date" queryParams
    filterParam =
        fromMaybe Nothing (lookup (fromMaybe "" filterParamName) queryParams)
    sortParam' =
        case sortParam of
            Nothing -> ""
            Just "author_name" -> "author_name"
            Just "date_creation" -> "date_creation"
            Just "category_name" -> "category_name"
            Just _ -> ""

newsMethodBlock ::
       Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
newsMethodBlock hLogger pool token_lifetime req
    | pathElemC == 1 = do sendNews hLogger pool req
    | pathElemC == 2 = do
        let newsId = readMaybe $ BC.unpack $ last pathElems :: Maybe Int
        sendNewsById hLogger pool req newsId
    | pathElemC == 3 = do
        let newsId = readMaybe $ BC.unpack $ head $ tail pathElems :: Maybe Int
        if last pathElems == "comments"
            then sendCommentsByNewsId hLogger pool req newsId
            else do
                logError hLogger "Bad url"
                return $ responseNotFound "Not Found"
    | pathElemC == 4 =
        case last pathElems of
            "add_comment" -> do
                let news'_id =
                        readMaybe $ BC.unpack $ head $ tail pathElems :: Maybe Int
                addCommentByNewsId hLogger pool token_lifetime req news'_id
            "delete_comment" -> do
                let news'_id =
                        readMaybe $ BC.unpack $ head $ tail pathElems :: Maybe Int
                case news'_id of
                    Nothing -> do
                        logError hLogger "bad comment id"
                        return $ responseBadRequest "bad comment id"
                    Just _ -> deleteCommentById hLogger pool token_lifetime req
            _ -> do
                logError hLogger "Bad url"
                return $ responseNotFound "Not Found"
    | otherwise = do
        logError hLogger "Bad url"
        return $ responseNotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemC = length pathElems
