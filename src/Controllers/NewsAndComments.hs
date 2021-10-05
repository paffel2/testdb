{-# LANGUAGE OverloadedStrings #-}

module Controllers.NewsAndComments where

import Control.Applicative (Alternative((<|>)))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import qualified Data.Text.Encoding as E
import Database.PostgreSQL.Simple (Connection)
import FromRequest (takeToken)
import HelpFunction (myLookup)
import Logger (Handle, logError, logInfo)
import Network.HTTP.Types.Method (methodDelete, methodGet, methodPost)
import Network.Wai (Request(queryString, rawPathInfo, requestMethod), Response)
import Network.Wai.Parse
    ( lbsBackEnd
    , noLimitParseRequestBodyOptions
    , parseRequestBodyEx
    )
import OperationsHandle
    ( NewsAndCommentsHandle(add_comment_to_db, delete_comment_from_db,
                      get_comments_by_news_id_from_db, get_news_by_id_from_db,
                      get_news_filter_by_after_date_from_db,
                      get_news_filter_by_author_name_from_db,
                      get_news_filter_by_before_date_from_db,
                      get_news_filter_by_category_id_from_db,
                      get_news_filter_by_content_from_db,
                      get_news_filter_by_date_from_db,
                      get_news_filter_by_tag_all_from_db,
                      get_news_filter_by_tag_id_from_db,
                      get_news_filter_by_tag_in_from_db,
                      get_news_filter_by_title_from_db, get_news_from_db)
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
       MonadIO m
    => Handle m
    -> NewsAndCommentsHandle m
    -> Pool Connection
    -> TokenLifeTime
    -> Request
    -> m Response
deleteCommentById hLogger operations pool token_lifetime req =
    if requestMethod req /= methodDelete
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad request method"
        else do
            logInfo hLogger "Preparing data for deleting commentary"
            let token' = E.decodeUtf8 <$> takeToken req
            let comment_id =
                    fromMaybe Nothing (lookup "comment_id" $ queryString req)
            let c_id' = read . BC.unpack <$> comment_id :: Maybe Int
            result <-
                delete_comment_from_db
                    operations
                    hLogger
                    pool
                    token_lifetime
                    token'
                    c_id'
            case result of
                Left "Not admin" -> do
                    logError hLogger "Commentary not deleted. Not admin."
                    return $ responseForbidden "Not admin"
                Left "Bad token" -> do
                    logError hLogger "Commentary not deleted. Bad token."
                    return $ responseForbidden "Bad token"
                Left bs -> do
                    logError hLogger "Commentary not deleted."
                    return $ responseBadRequest bs
                Right bs -> do
                    logInfo hLogger "Commentary deleted."
                    return $ responseOk bs

addCommentByNewsId ::
       MonadIO m
    => Handle m
    -> NewsAndCommentsHandle m
    -> Pool Connection
    -> TokenLifeTime
    -> Request
    -> Maybe Int
    -> m Response
addCommentByNewsId hLogger operations pool token_lifetime req news'_id =
    if requestMethod req /= methodPost
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad request method"
        else do
            logInfo hLogger "Preparing data for adding commentary"
            (i, _) <-
                liftIO $
                parseRequestBodyEx noLimitParseRequestBodyOptions lbsBackEnd req
            let token' = takeToken req
            let comment = lookup "comment_text" i
            result <-
                add_comment_to_db
                    operations
                    hLogger
                    pool
                    token_lifetime
                    (E.decodeUtf8 $ fromMaybe "" token')
                    news'_id
                    (E.decodeUtf8 <$> comment)
            case result of
                Left "News not exist" -> do
                    logError hLogger "Commentary not added. News not exist."
                    return $ responseBadRequest "News not exist"
                Left "Bad token" -> do
                    logError hLogger "Commentary not added. Bad token."
                    return $ responseForbidden "Bad token"
                Left bs -> do
                    logError hLogger "Commentary not added."
                    return $ responseBadRequest bs
                Right bs -> do
                    logInfo hLogger "Commentary added."
                    return $ responseCreated bs

sendCommentsByNewsId ::
       MonadIO m
    => Handle m
    -> NewsAndCommentsHandle m
    -> Pool Connection
    -> Request
    -> Maybe Int
    -> m Response
sendCommentsByNewsId hLogger operations pool req news'_id =
    if requestMethod req /= methodGet
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad request method"
        else do
            logInfo hLogger "Preparing data for sending commentary list"
            let pageParam = fromMaybe Nothing (lookup "page" $ queryString req)
            result <-
                get_comments_by_news_id_from_db
                    operations
                    hLogger
                    pool
                    news'_id
                    pageParam
            case result of
                Left bs -> do
                    logError hLogger "Commentaries not sended."
                    return $ responseBadRequest bs
                Right ca -> do
                    logInfo hLogger "Commentaries sended."
                    return $ responseOKJSON $ encode ca

sendNewsById ::
       MonadIO m
    => Handle m
    -> NewsAndCommentsHandle m
    -> Pool Connection
    -> Request
    -> Maybe Int
    -> m Response
sendNewsById hLogger operations pool req newsId =
    if requestMethod req /= methodGet
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad request method"
        else do
            logInfo hLogger "Preparing data for sending news"
            result <- get_news_by_id_from_db operations hLogger pool newsId
            case result of
                Left "News not exist" -> do
                    logError hLogger "News not sended. News not exist."
                    return $ responseBadRequest "News not exist"
                Left bs -> do
                    logError hLogger "News not sended."
                    return $ responseBadRequest bs
                Right gn -> return $ responseOKJSON $ encode gn

sendNews ::
       MonadIO m
    => Handle m
    -> NewsAndCommentsHandle m
    -> Pool Connection
    -> Request
    -> m Response
sendNews hLogger operations pool req =
    if requestMethod req == methodGet
        then do
            result <-
                case filterParamName of
                    Just "tag_in" ->
                        get_news_filter_by_tag_in_from_db
                            operations
                            hLogger
                            pool
                            filterParam
                            pageParam
                    Just "category" ->
                        get_news_filter_by_category_id_from_db
                            operations
                            hLogger
                            pool
                            filterParam
                            pageParam
                            sortParam'
                    Just "title" ->
                        get_news_filter_by_title_from_db
                            operations
                            hLogger
                            pool
                            filterParam
                            pageParam
                            sortParam'
                    Just "author" ->
                        get_news_filter_by_author_name_from_db
                            operations
                            hLogger
                            pool
                            filterParam
                            pageParam
                            sortParam'
                    Just "date" ->
                        get_news_filter_by_date_from_db
                            operations
                            hLogger
                            pool
                            filterParam
                            pageParam
                            sortParam'
                    Just "tag_all" ->
                        get_news_filter_by_tag_all_from_db
                            operations
                            hLogger
                            pool
                            filterParam
                            pageParam
                            sortParam'
                    Just "content" ->
                        get_news_filter_by_content_from_db
                            operations
                            hLogger
                            pool
                            filterParam
                            pageParam
                            sortParam'
                    Just "after_date" ->
                        get_news_filter_by_after_date_from_db
                            operations
                            hLogger
                            pool
                            filterParam
                            pageParam
                            sortParam'
                    Just "before_date" ->
                        get_news_filter_by_before_date_from_db
                            operations
                            hLogger
                            pool
                            filterParam
                            pageParam
                            sortParam'
                    Just "tag" ->
                        get_news_filter_by_tag_id_from_db
                            operations
                            hLogger
                            pool
                            filterParam
                            pageParam
                            sortParam'
                    Just _ -> do
                        logError hLogger "Bad request"
                        return $ Left "Bad request"
                    Nothing ->
                        get_news_from_db
                            operations
                            hLogger
                            pool
                            sortParam'
                            pageParam
            case result of
                Left bs -> return $ responseBadRequest bs
                Right na -> return $ responseOKJSON $ encode na
        else do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad request method"
  where
    queryParams = queryString req
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

newsAndCommentsRouter ::
       MonadIO m
    => Handle m
    -> NewsAndCommentsHandle m
    -> Pool Connection
    -> TokenLifeTime
    -> Request
    -> m Response
newsAndCommentsRouter hLogger operations pool token_lifetime req
    | pathElemC == 1 = do sendNews hLogger operations pool req
    | pathElemC == 2 = do
        let newsId = readMaybe $ BC.unpack $ last pathElems :: Maybe Int
        sendNewsById hLogger operations pool req newsId
    | pathElemC == 3 = do
        let newsId = readMaybe $ BC.unpack $ head $ tail pathElems :: Maybe Int
        if last pathElems == "comments"
            then sendCommentsByNewsId hLogger operations pool req newsId
            else do
                logError hLogger "Bad url"
                return $ responseNotFound "Not Found"
    | pathElemC == 4 =
        case last pathElems of
            "add_comment" -> do
                let news'_id =
                        readMaybe $ BC.unpack $ head $ tail pathElems :: Maybe Int
                addCommentByNewsId
                    hLogger
                    operations
                    pool
                    token_lifetime
                    req
                    news'_id
            "delete_comment" -> do
                let news'_id =
                        readMaybe $ BC.unpack $ head $ tail pathElems :: Maybe Int
                case news'_id of
                    Nothing -> do
                        logError hLogger "bad comment id"
                        return $ responseBadRequest "bad comment id"
                    Just _ ->
                        deleteCommentById
                            hLogger
                            operations
                            pool
                            token_lifetime
                            req
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
