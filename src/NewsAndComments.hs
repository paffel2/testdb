{-# LANGUAGE OverloadedStrings #-}

module NewsAndComments where

import Control.Applicative (Alternative((<|>)))
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import qualified Data.Text.Encoding as E
import Database.PostgreSQL.Simple (Connection)
import Databaseoperations
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
import Network.Wai
    ( Request(queryString, rawQueryString, requestMethod)
    , Response
    )
import Network.Wai.Parse
    ( lbsBackEnd
    , noLimitParseRequestBodyOptions
    , parseRequestBodyEx
    )
import Responses (responseBadRequest, responseOk)
import Text.Read (readMaybe)
import Types (CommentArray, GetNews, NewsArray, TokenLifeTime)

newsMethodBlock ::
       Handle
    -> Pool Connection
    -> TokenLifeTime
    -> [BC.ByteString]
    -> Request
    -> IO Response
newsMethodBlock hLogger pool token_lifetime pathElems req
    | pathElemC == 1 = do
        result <- sendNews hLogger pool req
        case result of
            Left bs -> return $ responseBadRequest bs
            Right na -> return $ responseOk $ encode na
    | pathElemC == 2 = do
        let newsId = readMaybe $ BC.unpack $ last pathElems :: Maybe Int
        result <- sendNewsById hLogger pool req newsId
        case result of
            Left bs -> return $ responseBadRequest bs
            Right na -> return $ responseOk $ encode na
    | pathElemC == 3 = do
        let newsId = readMaybe $ BC.unpack $ head $ tail pathElems :: Maybe Int
        if last pathElems == "comments"
            then do
                result <- sendCommentsByNewsId hLogger pool req newsId
                case result of
                    Left bs -> return $ responseBadRequest bs
                    Right ca -> return $ responseOk $ encode ca
            else do
                logError hLogger "Bad url"
                return $ responseBadRequest "Bad url"
    | pathElemC == 4 =
        case last pathElems of
            "add_comment" -> do
                let news'_id =
                        readMaybe $ BC.unpack $ head $ tail pathElems :: Maybe Int
                result <-
                    addCommentByNewsId hLogger pool token_lifetime req news'_id
                case result of
                    Left bs -> return $ responseBadRequest bs
                    Right bs -> return $ responseOk bs
            "delete_comment" -> do
                let news'_id =
                        readMaybe $ BC.unpack $ head $ tail pathElems :: Maybe Int
                case news'_id of
                    Nothing -> do
                        logError hLogger "bad comment id"
                        return $ responseBadRequest "bad comment id"
                    Just _ -> do
                        result <-
                            deleteCommentById hLogger pool token_lifetime req
                        case result of
                            Left bs -> return $ responseBadRequest bs
                            Right bs -> return $ responseOk bs
            _ -> do
                logError hLogger "Bad url"
                return $ responseBadRequest "bad request"
    | otherwise = do
        logError hLogger "Bad url"
        return $ responseBadRequest "Bad url"
  where
    pathElemC = length pathElems

sendNews ::
       Handle
    -> Pool Connection
    -> Request
    -> IO (Either LBS.ByteString NewsArray)
sendNews hLogger pool req = do
    if requestMethod req == methodGet
        then do
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
                        fstParam
                        pageParam
                        sortParam'
                Just _ -> do
                    logError hLogger "Bad request"
                    return $ Left "Bad request"
                Nothing -> getNewsFromDb hLogger pool sortParam' pageParam
        else return $ Left "Bad method request"
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

sendNewsById ::
       Handle
    -> Pool Connection
    -> Request
    -> Maybe Int
    -> IO (Either LBS.ByteString GetNews)
sendNewsById hLogger pool req newsId =
    if requestMethod req == methodGet
        then do
            let queryParams = rawQueryString req
            if queryParams == ""
                then getNewsByIdFromDb hLogger pool newsId
                else return $ Left "unexpected params"
        else return $ Left "Bad request method"

sendCommentsByNewsId ::
       Handle
    -> Pool Connection
    -> Request
    -> Maybe Int
    -> IO (Either LBS.ByteString CommentArray)
sendCommentsByNewsId hLogger pool req news'_id =
    if requestMethod req == methodGet
        then do
            let pageParam = fromMaybe Nothing (lookup "page" $ queryString req)
            getCommentsByNewsIdFromDb hLogger pool news'_id pageParam
        else return $ Left "Bad request method"

addCommentByNewsId ::
       Handle
    -> Pool Connection
    -> TokenLifeTime
    -> Request
    -> Maybe Int
    -> IO (Either LBS.ByteString LBS.ByteString)
addCommentByNewsId hLogger pool token_lifetime req news'_id =
    if requestMethod req == methodPost
        then do
            (i, _) <-
                parseRequestBodyEx noLimitParseRequestBodyOptions lbsBackEnd req
            let token' = takeToken req
            let comment = lookup "comment_text" i
            addCommentToDb
                hLogger
                pool
                token_lifetime
                (E.decodeUtf8 $ fromMaybe "" token')
                news'_id
                (E.decodeUtf8 <$> comment)
        else return $ Left "Bad request method"

deleteCommentById ::
       Handle
    -> Pool Connection
    -> TokenLifeTime
    -> Request
    -> IO (Either LBS.ByteString LBS.ByteString)
deleteCommentById hLogger pool token_lifetime req =
    if requestMethod req == methodDelete
        then do
            let token' = E.decodeUtf8 <$> takeToken req
            let comment_id =
                    fromMaybe Nothing (lookup "comment_id" $ queryString req)
            let c_id' = read . BC.unpack <$> comment_id :: Maybe Int
            deleteCommentFromDb hLogger pool token_lifetime token' c_id'
        else return $ Left "Bad request method"
