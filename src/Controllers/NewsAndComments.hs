{-# LANGUAGE OverloadedStrings #-}

module Controllers.NewsAndComments where

import           Control.Applicative       (Alternative ((<|>)))
import           Control.Monad.IO.Class    (MonadIO (..))
import           Data.Aeson                (encode)
import qualified Data.ByteString.Char8     as BC
import           FromRequest               (FilterParam (toFilterParam),
                                            takeToken, toCommentId,
                                            toCommentText, toPage, toSort)
import           HelpFunction              (myLookup, readByteStringToId)
import           Logger                    (logError, logInfo)
import           Network.HTTP.Types.Method (methodDelete, methodGet, methodPost)
import           Network.Wai               (Request (queryString, rawPathInfo, requestMethod),
                                            Response)
import           Network.Wai.Parse         (lbsBackEnd,
                                            noLimitParseRequestBodyOptions,
                                            parseRequestBodyEx)
import           OperationsHandle          (NewsAndCommentsHandle (add_comment_to_db, delete_comment_from_db, get_comments_by_news_id_from_db, get_news_by_id_from_db, get_news_filter_by_after_date_from_db, get_news_filter_by_author_name_from_db, get_news_filter_by_before_date_from_db, get_news_filter_by_category_id_from_db, get_news_filter_by_content_from_db, get_news_filter_by_date_from_db, get_news_filter_by_tag_all_from_db, get_news_filter_by_tag_id_from_db, get_news_filter_by_tag_in_from_db, get_news_filter_by_title_from_db, get_news_from_db, news_logger))
import           Responses                 (badResponse, responseBadRequest,
                                            responseCreated,
                                            responseMethodNotAllowed,
                                            responseNotFound, responseOKJSON,
                                            responseOk)
import           Text.Read                 (readMaybe)
import           Types.NewsAndComments     (CommentWithoutTokenLifeTime (CommentWithoutTokenLifeTime, comment_news_id', comment_text'', comment_token'))
import           Types.Other               (Id, SomeError (OtherError))

deleteCommentById ::
       MonadIO m => NewsAndCommentsHandle m -> Request -> m Response
deleteCommentById operations req =
    if requestMethod req /= methodDelete
        then do
            logError (news_logger operations) "Bad request method"
            return $ responseMethodNotAllowed "Bad request method"
        else do
            logInfo
                (news_logger operations)
                "Preparing data for deleting commentary"
            let token' = takeToken req
            let comment_id = toCommentId req
            result <-
                delete_comment_from_db
                    operations
                    (news_logger operations)
                    token'
                    comment_id
            case result of
                Left someError ->
                    return $ badResponse "Commentary not deleted." someError
                Right _ -> do
                    return $ responseOk "Commentary deleted."

{-addCommentByNewsId ::
       MonadIO m
    => LoggerHandle m
    -> NewsAndCommentsHandle m
    -> TokenLifeTime
    -> Request
    -> Maybe Id
    -> m Response
addCommentByNewsId hLogger operations token_lifetime req news'_id =
    if requestMethod req /= methodPost
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad request method"
        else do
            logInfo hLogger "Preparing data for adding commentary"
            (i, _) <-
                liftIO $
                parseRequestBodyEx noLimitParseRequestBodyOptions lbsBackEnd req
            let comment =
                    Comment
                        { comment_token = takeToken req
                        , comment_token_lifetime = token_lifetime
                        , comment_text = toCommentText i
                        , comment_news_id = news'_id
                        }
            result <- add_comment_to_db operations hLogger comment
            case result of
                Left someError ->
                    return $ badResponse "Commentary not added." someError
                Right _ -> do
                    return $ responseCreated "Commentary added" -}
addCommentByNewsId ::
       MonadIO m => NewsAndCommentsHandle m -> Request -> Maybe Id -> m Response
addCommentByNewsId operations req news'_id =
    if requestMethod req /= methodPost
        then do
            logError (news_logger operations) "Bad request method"
            return $ responseMethodNotAllowed "Bad request method"
        else do
            logInfo
                (news_logger operations)
                "Preparing data for adding commentary"
            (i, _) <-
                liftIO $
                parseRequestBodyEx noLimitParseRequestBodyOptions lbsBackEnd req
            let comment =
                    CommentWithoutTokenLifeTime
                        { comment_token' = takeToken req
                        , comment_text'' = toCommentText i
                        , comment_news_id' = news'_id
                        }
            result <-
                add_comment_to_db operations (news_logger operations) comment
            case result of
                Left someError ->
                    return $ badResponse "Commentary not added." someError
                Right _ -> do
                    return $ responseCreated "Commentary added"

sendCommentsByNewsId ::
       MonadIO m => NewsAndCommentsHandle m -> Request -> Maybe Id -> m Response
sendCommentsByNewsId operations req news'_id =
    if requestMethod req /= methodGet
        then do
            logError (news_logger operations) "Bad request method"
            return $ responseMethodNotAllowed "Bad request method"
        else do
            logInfo
                (news_logger operations)
                "Preparing data for sending commentary list"
            let pageParam = toPage req
            result <-
                get_comments_by_news_id_from_db
                    operations
                    (news_logger operations)
                    news'_id
                    pageParam
            case result of
                Left someError ->
                    return $ badResponse "Commentaries not sended." someError
                Right ca -> do
                    return $ responseOKJSON $ encode ca

sendNewsById ::
       MonadIO m => NewsAndCommentsHandle m -> Request -> Maybe Id -> m Response
sendNewsById operations req newsId =
    if requestMethod req /= methodGet
        then do
            logError (news_logger operations) "Bad request method"
            return $ responseMethodNotAllowed "Bad request method"
        else do
            logInfo (news_logger operations) "Preparing data for sending news"
            result <-
                get_news_by_id_from_db
                    operations
                    (news_logger operations)
                    newsId
            case result of
                Left someError ->
                    return $ badResponse "News not sended." someError
                Right gn -> return $ responseOKJSON $ encode gn

sendNews :: MonadIO m => NewsAndCommentsHandle m -> Request -> m Response
sendNews operations req =
    if requestMethod req == methodGet
        then do
            result <-
                case filterParamName of
                    Just "tag_in" ->
                        get_news_filter_by_tag_in_from_db
                            operations
                            (news_logger operations)
                            (toFilterParam req)
                            pageParam
                    Just "category" ->
                        get_news_filter_by_category_id_from_db
                            operations
                            (news_logger operations)
                            (toFilterParam req)
                            pageParam
                            sortParam
                    Just "title" ->
                        get_news_filter_by_title_from_db
                            operations
                            (news_logger operations)
                            (toFilterParam req)
                            pageParam
                            sortParam
                    Just "author" ->
                        get_news_filter_by_author_name_from_db
                            operations
                            (news_logger operations)
                            (toFilterParam req)
                            pageParam
                            sortParam
                    Just "date" ->
                        get_news_filter_by_date_from_db
                            operations
                            (news_logger operations)
                            (toFilterParam req)
                            pageParam
                            sortParam
                    Just "tag_all" ->
                        get_news_filter_by_tag_all_from_db
                            operations
                            (news_logger operations)
                            (toFilterParam req)
                            pageParam
                            sortParam
                    Just "content" ->
                        get_news_filter_by_content_from_db
                            operations
                            (news_logger operations)
                            (toFilterParam req)
                            pageParam
                            sortParam
                    Just "after_date" ->
                        get_news_filter_by_after_date_from_db
                            operations
                            (news_logger operations)
                            (toFilterParam req)
                            pageParam
                            sortParam
                    Just "before_date" ->
                        get_news_filter_by_before_date_from_db
                            operations
                            (news_logger operations)
                            (toFilterParam req)
                            pageParam
                            sortParam
                    Just "tag" ->
                        get_news_filter_by_tag_id_from_db
                            operations
                            (news_logger operations)
                            (toFilterParam req)
                            pageParam
                            sortParam
                    Just _ -> do
                        logError (news_logger operations) "Bad request"
                        return . Left . OtherError $ "Bad request"
                    Nothing ->
                        get_news_from_db
                            operations
                            (news_logger operations)
                            sortParam
                            pageParam
            case result of
                Left someError ->
                    return $ badResponse "News not sended." someError
                Right na -> return $ responseOKJSON $ encode na
        else do
            logError (news_logger operations) "Bad request method"
            return $ responseMethodNotAllowed "Bad request method"
  where
    queryParams = queryString req
    pageParam = toPage req
    sortParam = toSort req
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

newsAndCommentsRouter ::
       MonadIO m => NewsAndCommentsHandle m -> Request -> m Response
newsAndCommentsRouter operations req
    | pathElemC == 1 = do sendNews operations req
    | pathElemC == 2 = do
        let newsId = readByteStringToId . last $ pathElems
        sendNewsById operations req newsId
    | pathElemC == 3 = do
        let newsId = readByteStringToId . head . tail $ pathElems
        if last pathElems == "comments"
            then sendCommentsByNewsId operations req newsId
            else do
                logError (news_logger operations) "Bad url"
                return $ responseNotFound "Not Found"
    | pathElemC == 4 =
        case last pathElems of
            "add_comment" -> do
                let news'_id = readByteStringToId . head . tail $ pathElems
                addCommentByNewsId operations req news'_id
            "delete_comment" -> do
                let news'_id =
                        readMaybe $ BC.unpack $ head $ tail pathElems :: Maybe Int
                case news'_id of
                    Nothing -> do
                        logError (news_logger operations) "Bad news id"
                        return $ responseBadRequest "Bad news id"
                    Just _ -> deleteCommentById operations req
            _ -> do
                logError (news_logger operations) "Bad url"
                return $ responseNotFound "Not Found"
    | otherwise = do
        logError (news_logger operations) "Bad url"
        return $ responseNotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemC = length pathElems
