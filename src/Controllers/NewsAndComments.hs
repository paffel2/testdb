{-# LANGUAGE OverloadedStrings #-}

module Controllers.NewsAndComments where

import           Control.Applicative       (Alternative ((<|>)))
import           Data.Aeson                (encode)
import qualified Data.ByteString.Char8     as BC
import           FromRequest               (FilterParam (toFilterParam),
                                            takeToken, toCommentId,
                                            toCommentText, toPage, toSort)
import           HelpFunction              (myLookup, readByteStringToId)
import           Logger                    (logError, logInfo)
import           Network.HTTP.Types.Method (methodDelete, methodGet, methodPost)
import           Network.Wai               (Request (queryString, rawPathInfo, requestMethod))
import           OperationsHandle          (NewsAndCommentsHandle (nchAddCommentToDb, nchDeleteCommentFromDb, nchGetCommentsByNewsIdFromDb, nchGetNewsByIdFromDb, nchGetNewsFilterByAfterDateFromDb, nchGetNewsFilterByBeforeDateFromDb, nchGetNewsFilterByCategoryIdFromDb, nchGetNewsFilterByContentFromDb, nchGetNewsFilterByDateFromDb, nchGetNewsFilterByTagAllFromDb, nchGetNewsFilterByTagIdFromDb, nchGetNewsFilterByTagInFromDb, nchGetNewsFilterByTitleFromDb, nchGetNewsFromDb, nchGetgetNewsFilterByAuthorNameFromDb, nchLogger, nchParseRequestBody))
import           Responses                 (toResponseErrorMessage)
import           Text.Read                 (readMaybe)
import           Types.NewsAndComments     (CommentWithoutTokenLifeTime (..))
import           Types.Other               (Id,
                                            ResponseErrorMessage (BadRequest, MethodNotAllowed, NotFound),
                                            ResponseOkMessage (Created, OkJSON, OkMessage),
                                            SomeError (OtherError))

deleteCommentById ::
       Monad m
    => NewsAndCommentsHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
deleteCommentById operations req =
    if requestMethod req /= methodDelete
        then do
            logError (nchLogger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo
                (nchLogger operations)
                "Preparing data for deleting commentary"
            let token = takeToken req
            let commentId = toCommentId req
            result <- nchDeleteCommentFromDb operations token commentId
            case result of
                Left someError ->
                    return $
                    Left $
                    toResponseErrorMessage "Commentary not deleted." someError
                Right _ -> do
                    return $ Right $ OkMessage "Commentary deleted."

postCommentByNewsId ::
       Monad m
    => NewsAndCommentsHandle m
    -> Request
    -> Maybe Id
    -> m (Either ResponseErrorMessage ResponseOkMessage)
postCommentByNewsId operations req newsId =
    if requestMethod req /= methodPost
        then do
            logError (nchLogger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo
                (nchLogger operations)
                "Preparing data for adding commentary"
            (i, _) <- nchParseRequestBody operations req
            let comment =
                    CommentWithoutTokenLifeTime
                        { commentWTLToken = takeToken req
                        , commentWTLText = toCommentText i
                        , commentWTLNewsId = newsId
                        }
            result <- nchAddCommentToDb operations comment
            case result of
                Left someError ->
                    return $
                    Left $
                    toResponseErrorMessage "Commentary not added." someError
                Right _ -> do
                    return $ Right $ Created "Commentary added"

sendCommentsByNewsId ::
       Monad m
    => NewsAndCommentsHandle m
    -> Request
    -> Maybe Id
    -> m (Either ResponseErrorMessage ResponseOkMessage)
sendCommentsByNewsId operations req newsId =
    if requestMethod req /= methodGet
        then do
            logError (nchLogger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo
                (nchLogger operations)
                "Preparing data for sending commentary list"
            let pageParam = toPage req
            result <- nchGetCommentsByNewsIdFromDb operations newsId pageParam
            case result of
                Left someError ->
                    return $
                    Left $
                    toResponseErrorMessage "Commentaries not sended." someError
                Right ca -> do
                    return $ Right $ OkJSON $ encode ca

getNewsById ::
       Monad m
    => NewsAndCommentsHandle m
    -> Request
    -> Maybe Id
    -> m (Either ResponseErrorMessage ResponseOkMessage)
getNewsById operations req newsId =
    if requestMethod req /= methodGet
        then do
            logError (nchLogger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo (nchLogger operations) "Preparing data for sending news"
            result <- nchGetNewsByIdFromDb operations newsId
            case result of
                Left someError ->
                    return $
                    Left $ toResponseErrorMessage "News not sended." someError
                Right someNews -> return $ Right $ OkJSON $ encode someNews

getNews ::
       Monad m
    => NewsAndCommentsHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
getNews operations req =
    if requestMethod req == methodGet
        then do
            result <-
                case filterParamName of
                    Just "tag_in" ->
                        nchGetNewsFilterByTagInFromDb
                            operations
                            (toFilterParam req)
                            pageParam
                    Just "category" ->
                        nchGetNewsFilterByCategoryIdFromDb
                            operations
                            (toFilterParam req)
                            pageParam
                            sortParam
                    Just "title" ->
                        nchGetNewsFilterByTitleFromDb
                            operations
                            (toFilterParam req)
                            pageParam
                            sortParam
                    Just "author" ->
                        nchGetgetNewsFilterByAuthorNameFromDb
                            operations
                            (toFilterParam req)
                            pageParam
                            sortParam
                    Just "date" ->
                        nchGetNewsFilterByDateFromDb
                            operations
                            (toFilterParam req)
                            pageParam
                            sortParam
                    Just "tag_all" ->
                        nchGetNewsFilterByTagAllFromDb
                            operations
                            (toFilterParam req)
                            pageParam
                            sortParam
                    Just "content" ->
                        nchGetNewsFilterByContentFromDb
                            operations
                            (toFilterParam req)
                            pageParam
                            sortParam
                    Just "after_date" ->
                        nchGetNewsFilterByAfterDateFromDb
                            operations
                            (toFilterParam req)
                            pageParam
                            sortParam
                    Just "before_date" ->
                        nchGetNewsFilterByBeforeDateFromDb
                            operations
                            (toFilterParam req)
                            pageParam
                            sortParam
                    Just "tag" ->
                        nchGetNewsFilterByTagIdFromDb
                            operations
                            (toFilterParam req)
                            pageParam
                            sortParam
                    Just _ -> do
                        logError (nchLogger operations) "Bad request"
                        return . Left . OtherError $ "Bad request"
                    Nothing -> nchGetNewsFromDb operations sortParam pageParam
            case result of
                Left someError ->
                    return $
                    Left $ toResponseErrorMessage "News not sended." someError
                Right newsArray -> return $ Right $ OkJSON $ encode newsArray
        else do
            logError (nchLogger operations) "Bad request method"
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
       Monad m
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
                logError (nchLogger operations) "Bad url"
                return $ Left $ NotFound "Not Found"
    | pathElemC == 4 =
        case last pathElems of
            "add_comment" -> do
                let newsId = readByteStringToId . head . tail $ pathElems
                postCommentByNewsId operations req newsId
            "delete_comment" -> do
                let newsId =
                        readMaybe $ BC.unpack $ head $ tail pathElems :: Maybe Int
                case newsId of
                    Nothing -> do
                        logError (nchLogger operations) "Bad news id"
                        return $ Left $ BadRequest "Bad news id"
                    Just _ -> deleteCommentById operations req
            _ -> do
                logError (nchLogger operations) "Bad url"
                return $ Left $ NotFound "Not Found"
    | otherwise = do
        logError (nchLogger operations) "Bad url"
        return $ Left $ NotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemC = length pathElems
