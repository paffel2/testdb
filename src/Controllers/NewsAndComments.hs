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
import           Network.Wai               (Request (queryString, rawPathInfo, requestMethod))
import           Network.Wai.Parse         (lbsBackEnd,
                                            noLimitParseRequestBodyOptions,
                                            parseRequestBodyEx)
import           OperationsHandle          (NewsAndCommentsHandle (add_comment_to_db, delete_comment_from_db, get_comments_by_news_id_from_db, get_news_by_id_from_db, get_news_filter_by_after_date_from_db, get_news_filter_by_author_name_from_db, get_news_filter_by_before_date_from_db, get_news_filter_by_category_id_from_db, get_news_filter_by_content_from_db, get_news_filter_by_date_from_db, get_news_filter_by_tag_all_from_db, get_news_filter_by_tag_id_from_db, get_news_filter_by_tag_in_from_db, get_news_filter_by_title_from_db, get_news_from_db, news_logger))
import           Responses                 (toResponseErrorMessage)
import           Text.Read                 (readMaybe)
import           Types.NewsAndComments     (CommentWithoutTokenLifeTime (CommentWithoutTokenLifeTime, comment_news_id', comment_text'', comment_token'))
import           Types.Other               (Id,
                                            ResponseErrorMessage (BadRequest, MethodNotAllowed, NotFound),
                                            ResponseOkMessage (Created, OkJSON, OkMessage),
                                            SomeError (OtherError))

deleteCommentById ::
       MonadIO m
    => NewsAndCommentsHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
deleteCommentById operations req =
    if requestMethod req /= methodDelete
        then do
            logError (news_logger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo
                (news_logger operations)
                "Preparing data for deleting commentary"
            let token' = takeToken req
            let comment_id = toCommentId req
            result <- delete_comment_from_db operations token' comment_id
            case result of
                Left someError ->
                    return $
                    Left $
                    toResponseErrorMessage "Commentary not deleted." someError
                Right _ -> do
                    return $ Right $ OkMessage "Commentary deleted."

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
postCommentByNewsId ::
       MonadIO m
    => NewsAndCommentsHandle m
    -> Request
    -> Maybe Id
    -> m (Either ResponseErrorMessage ResponseOkMessage)
postCommentByNewsId operations req news'_id =
    if requestMethod req /= methodPost
        then do
            logError (news_logger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
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
            result <- add_comment_to_db operations comment
            case result of
                Left someError ->
                    return $
                    Left $
                    toResponseErrorMessage "Commentary not added." someError
                Right _ -> do
                    return $ Right $ Created "Commentary added"

sendCommentsByNewsId ::
       MonadIO m
    => NewsAndCommentsHandle m
    -> Request
    -> Maybe Id
    -> m (Either ResponseErrorMessage ResponseOkMessage)
sendCommentsByNewsId operations req news'_id =
    if requestMethod req /= methodGet
        then do
            logError (news_logger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo
                (news_logger operations)
                "Preparing data for sending commentary list"
            let pageParam = toPage req
            result <-
                get_comments_by_news_id_from_db operations news'_id pageParam
            case result of
                Left someError ->
                    return $
                    Left $
                    toResponseErrorMessage "Commentaries not sended." someError
                Right ca -> do
                    return $ Right $ OkJSON $ encode ca

getNewsById ::
       MonadIO m
    => NewsAndCommentsHandle m
    -> Request
    -> Maybe Id
    -> m (Either ResponseErrorMessage ResponseOkMessage)
getNewsById operations req newsId =
    if requestMethod req /= methodGet
        then do
            logError (news_logger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo (news_logger operations) "Preparing data for sending news"
            result <- get_news_by_id_from_db operations newsId
            case result of
                Left someError ->
                    return $
                    Left $ toResponseErrorMessage "News not sended." someError
                Right gn -> return $ Right $ OkJSON $ encode gn

getNews ::
       MonadIO m
    => NewsAndCommentsHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
getNews operations req =
    if requestMethod req == methodGet
        then do
            result <-
                case filterParamName of
                    Just "tag_in" ->
                        get_news_filter_by_tag_in_from_db
                            operations
                            (toFilterParam req)
                            pageParam
                    Just "category" ->
                        get_news_filter_by_category_id_from_db
                            operations
                            (toFilterParam req)
                            pageParam
                            sortParam
                    Just "title" ->
                        get_news_filter_by_title_from_db
                            operations
                            (toFilterParam req)
                            pageParam
                            sortParam
                    Just "author" ->
                        get_news_filter_by_author_name_from_db
                            operations
                            (toFilterParam req)
                            pageParam
                            sortParam
                    Just "date" ->
                        get_news_filter_by_date_from_db
                            operations
                            (toFilterParam req)
                            pageParam
                            sortParam
                    Just "tag_all" ->
                        get_news_filter_by_tag_all_from_db
                            operations
                            (toFilterParam req)
                            pageParam
                            sortParam
                    Just "content" ->
                        get_news_filter_by_content_from_db
                            operations
                            (toFilterParam req)
                            pageParam
                            sortParam
                    Just "after_date" ->
                        get_news_filter_by_after_date_from_db
                            operations
                            (toFilterParam req)
                            pageParam
                            sortParam
                    Just "before_date" ->
                        get_news_filter_by_before_date_from_db
                            operations
                            (toFilterParam req)
                            pageParam
                            sortParam
                    Just "tag" ->
                        get_news_filter_by_tag_id_from_db
                            operations
                            (toFilterParam req)
                            pageParam
                            sortParam
                    Just _ -> do
                        logError (news_logger operations) "Bad request"
                        return . Left . OtherError $ "Bad request"
                    Nothing -> get_news_from_db operations sortParam pageParam
            case result of
                Left someError ->
                    return $
                    Left $ toResponseErrorMessage "News not sended." someError
                Right na -> return $ Right $ OkJSON $ encode na
        else do
            logError (news_logger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
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
       MonadIO m
    => NewsAndCommentsHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
newsAndCommentsRouter operations req
    | pathElemC == 1 = do getNews operations req
    | pathElemC == 2 = do
        let newsId = readByteStringToId . last $ pathElems
        getNewsById operations req newsId
    | pathElemC == 3 = do
        let newsId = readByteStringToId . head . tail $ pathElems
        if last pathElems == "comments"
            then sendCommentsByNewsId operations req newsId
            else do
                logError (news_logger operations) "Bad url"
                return $ Left $ NotFound "Not Found"
    | pathElemC == 4 =
        case last pathElems of
            "add_comment" -> do
                let news'_id = readByteStringToId . head . tail $ pathElems
                postCommentByNewsId operations req news'_id
            "delete_comment" -> do
                let news'_id =
                        readMaybe $ BC.unpack $ head $ tail pathElems :: Maybe Int
                case news'_id of
                    Nothing -> do
                        logError (news_logger operations) "Bad news id"
                        return $ Left $ BadRequest "Bad news id"
                    Just _ -> deleteCommentById operations req
            _ -> do
                logError (news_logger operations) "Bad url"
                return $ Left $ NotFound "Not Found"
    | otherwise = do
        logError (news_logger operations) "Bad url"
        return $ Left $ NotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemC = length pathElems
