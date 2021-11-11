{-# LANGUAGE OverloadedStrings #-}

module Answers.NewsAndComments where

import           Answer                    (AnswerHandle (..))
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
import           Types.NewsAndComments     (CommentArray,
                                            CommentWithoutTokenLifeTime (..),
                                            GetNews, NewsArray, Sort)
import           Types.Other               (Id, Page, ResponseErrorMessage,
                                            ResponseOkMessage (Created, OkJSON, OkMessage),
                                            SomeError (BadMethod, OtherError),
                                            Token)

deleteCommentParseInformation ::
       Monad m
    => NewsAndCommentsHandle m
    -> Request
    -> m (Either SomeError (Maybe Token, Maybe Id))
deleteCommentParseInformation handler request =
    if requestMethod request /= methodDelete
        then do
            logError (nchLogger handler) "Bad request method"
            return $ Left BadMethod
        else do
            logInfo (nchLogger handler) "Preparing data for deleting commentary"
            let token = takeToken request
            let commentId = toCommentId request
            return $ Right (token, commentId)

deleteCommentDatabaseOperation ::
       Monad m
    => NewsAndCommentsHandle m
    -> Either SomeError (Maybe Token, Maybe Id)
    -> m (Either SomeError ())
deleteCommentDatabaseOperation _ (Left message) = return $ Left message
deleteCommentDatabaseOperation commentsHandle (Right (token, commentId)) =
    nchDeleteCommentFromDb commentsHandle token commentId

deleteCommentSendResult ::
       Monad m
    => Either SomeError ()
    -> m (Either ResponseErrorMessage ResponseOkMessage)
deleteCommentSendResult result =
    case result of
        Left someError ->
            return $
            Left $ toResponseErrorMessage "Commentary not deleted." someError
        Right _ -> do
            return $ Right $ OkMessage "Commentary deleted."

deleteCommentHandle ::
       Monad m
    => NewsAndCommentsHandle m
    -> AnswerHandle m (Either SomeError (Maybe Token, Maybe Id)) ()
deleteCommentHandle commentsHandle =
    AnswerHandle
        { parseInformation = deleteCommentParseInformation commentsHandle
        , databaseOperation = deleteCommentDatabaseOperation commentsHandle
        , sendResult = deleteCommentSendResult
        }

--------------------------------------------------------------------------------------------------------------------
postCommentParseInformation ::
       Monad m
    => NewsAndCommentsHandle m
    -> Request
    -> m (Either SomeError CommentWithoutTokenLifeTime)
postCommentParseInformation handler request =
    if requestMethod request /= methodPost
        then do
            logError (nchLogger handler) "Bad request method"
            return $ Left BadMethod
        else do
            logInfo (nchLogger handler) "Preparing data for adding commentary"
            (i, _) <- nchParseRequestBody handler request
            let newsId = readByteStringToId . head . tail $ pathElems
            let comment =
                    CommentWithoutTokenLifeTime
                        { commentWTLToken = takeToken request
                        , commentWTLText = toCommentText i
                        , commentWTLNewsId = newsId
                        }
            return $ Right comment
  where
    path = BC.tail $ rawPathInfo request
    pathElems = BC.split '/' path

postCommentDatabaseOperation ::
       Monad m
    => NewsAndCommentsHandle m
    -> Either SomeError CommentWithoutTokenLifeTime
    -> m (Either SomeError ())
postCommentDatabaseOperation _ (Left message) = return $ Left message
postCommentDatabaseOperation commentsHandle (Right comment) =
    nchAddCommentToDb commentsHandle comment

postCommentSendResult ::
       Monad m
    => Either SomeError b
    -> m (Either ResponseErrorMessage ResponseOkMessage)
postCommentSendResult result =
    case result of
        Left someError ->
            return $
            Left $ toResponseErrorMessage "Commentary not added." someError
        Right _ -> do
            return $ Right $ Created "Commentary added"

postCommentHandle ::
       Monad m
    => NewsAndCommentsHandle m
    -> AnswerHandle m (Either SomeError CommentWithoutTokenLifeTime) ()
postCommentHandle commentsHandle =
    AnswerHandle
        { parseInformation = postCommentParseInformation commentsHandle
        , databaseOperation = postCommentDatabaseOperation commentsHandle
        , sendResult = postCommentSendResult
        }

--------------------------------------------------------------------------------------------------------------------
sendCommentsParseInformation ::
       Monad m
    => NewsAndCommentsHandle m
    -> Request
    -> m (Either SomeError (Maybe Page, Maybe Id))
sendCommentsParseInformation handler request =
    if requestMethod request /= methodGet
        then do
            logError (nchLogger handler) "Bad request method"
            return $ Left BadMethod
        else do
            logInfo
                (nchLogger handler)
                "Preparing data for sending commentary list"
            let newsId = readByteStringToId . head . tail $ pathElems
            return $ Right (toPage request, newsId)
  where
    path = BC.tail $ rawPathInfo request
    pathElems = BC.split '/' path

sendCommentsDatabaseOperation ::
       Monad m
    => NewsAndCommentsHandle m
    -> Either SomeError (Maybe Page, Maybe Id)
    -> m (Either SomeError CommentArray)
sendCommentsDatabaseOperation _ (Left message) = return $ Left message
sendCommentsDatabaseOperation commentsHandle (Right (page, newsId)) =
    nchGetCommentsByNewsIdFromDb commentsHandle newsId page

sendCommentsSendResult ::
       Monad m
    => Either SomeError CommentArray
    -> m (Either ResponseErrorMessage ResponseOkMessage)
sendCommentsSendResult result =
    case result of
        Left someError ->
            return $
            Left $ toResponseErrorMessage "Commentaries not sended." someError
        Right ca -> do
            return $ Right $ OkJSON $ encode ca

sendCommentsHandle ::
       Monad m
    => NewsAndCommentsHandle m
    -> AnswerHandle m (Either SomeError (Maybe Page, Maybe Id)) CommentArray
sendCommentsHandle commentsHandle =
    AnswerHandle
        { parseInformation = sendCommentsParseInformation commentsHandle
        , databaseOperation = sendCommentsDatabaseOperation commentsHandle
        , sendResult = sendCommentsSendResult
        }

--------------------------------------------------------------------------------------------------------------------
getNewsByIdParseInformation ::
       Monad m
    => NewsAndCommentsHandle m
    -> Request
    -> m (Either SomeError (Maybe Id))
getNewsByIdParseInformation handler request =
    if requestMethod request /= methodGet
        then do
            logError (nchLogger handler) "Bad request method"
            return $ Left BadMethod
        else do
            logInfo (nchLogger handler) "Preparing data for sending news"
            let newsId = readByteStringToId . last $ pathElems
            return $ Right newsId
  where
    path = BC.tail $ rawPathInfo request
    pathElems = BC.split '/' path

getNewsByIdDatabaseOperation ::
       Monad m
    => NewsAndCommentsHandle m
    -> Either SomeError (Maybe Id)
    -> m (Either SomeError GetNews)
getNewsByIdDatabaseOperation _ (Left message) = return $ Left message
getNewsByIdDatabaseOperation newsHandle (Right newsId) =
    nchGetNewsByIdFromDb newsHandle newsId

getNewsByIdSendResult ::
       Monad m
    => Either SomeError GetNews
    -> m (Either ResponseErrorMessage ResponseOkMessage)
getNewsByIdSendResult result =
    case result of
        Left someError ->
            return $ Left $ toResponseErrorMessage "News not sended." someError
        Right someNews -> return $ Right $ OkJSON $ encode someNews

getNewsByIdHandle ::
       Monad m
    => NewsAndCommentsHandle m
    -> AnswerHandle m (Either SomeError (Maybe Id)) GetNews
getNewsByIdHandle newsHandle =
    AnswerHandle
        { parseInformation = getNewsByIdParseInformation newsHandle
        , databaseOperation = getNewsByIdDatabaseOperation newsHandle
        , sendResult = getNewsByIdSendResult
        }

--------------------------------------------------------------------------------------------------------------------
getNewsParseInformation ::
       Monad m
    => NewsAndCommentsHandle m
    -> Request
    -> m (Either SomeError (Request, Maybe Page, Sort))
getNewsParseInformation handler request =
    if requestMethod request /= methodGet
        then do
            logError (nchLogger handler) "Bad request method"
            return $ Left BadMethod
        else return $ Right (request, toPage request, toSort request)

getNewsDatabaseOperation ::
       Monad m
    => NewsAndCommentsHandle m
    -> Either SomeError (Request, Maybe Page, Sort)
    -> m (Either SomeError NewsArray)
getNewsDatabaseOperation _ (Left message) = return $ Left message
getNewsDatabaseOperation newsHandle (Right (req, page, sort)) =
    case filterParamName of
        Just "tag_in" ->
            nchGetNewsFilterByTagInFromDb newsHandle (toFilterParam req) page
        Just "category" ->
            nchGetNewsFilterByCategoryIdFromDb
                newsHandle
                (toFilterParam req)
                page
                sort
        Just "title" ->
            nchGetNewsFilterByTitleFromDb
                newsHandle
                (toFilterParam req)
                page
                sort
        Just "author" ->
            nchGetgetNewsFilterByAuthorNameFromDb
                newsHandle
                (toFilterParam req)
                page
                sort
        Just "date" ->
            nchGetNewsFilterByDateFromDb
                newsHandle
                (toFilterParam req)
                page
                sort
        Just "tag_all" ->
            nchGetNewsFilterByTagAllFromDb
                newsHandle
                (toFilterParam req)
                page
                sort
        Just "content" ->
            nchGetNewsFilterByContentFromDb
                newsHandle
                (toFilterParam req)
                page
                sort
        Just "after_date" ->
            nchGetNewsFilterByAfterDateFromDb
                newsHandle
                (toFilterParam req)
                page
                sort
        Just "before_date" ->
            nchGetNewsFilterByBeforeDateFromDb
                newsHandle
                (toFilterParam req)
                page
                sort
        Just "tag" ->
            nchGetNewsFilterByTagIdFromDb
                newsHandle
                (toFilterParam req)
                page
                sort
        Just _ -> do
            logError (nchLogger newsHandle) "Bad request"
            return . Left . OtherError $ "Bad request"
        Nothing -> nchGetNewsFromDb newsHandle sort page
  where
    queryParams = queryString req
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

getNewsSendResult ::
       Monad m
    => Either SomeError NewsArray
    -> m (Either ResponseErrorMessage ResponseOkMessage)
getNewsSendResult result =
    case result of
        Left someError ->
            return $ Left $ toResponseErrorMessage "News not sended." someError
        Right newsArray -> return $ Right $ OkJSON $ encode newsArray

getNewsHandle ::
       Monad m
    => NewsAndCommentsHandle m
    -> AnswerHandle m (Either SomeError (Request, Maybe Page, Sort)) NewsArray
getNewsHandle newsHandle =
    AnswerHandle
        { parseInformation = getNewsParseInformation newsHandle
        , databaseOperation = getNewsDatabaseOperation newsHandle
        , sendResult = getNewsSendResult
        }
--------------------------------------------------------------------------------------------------------------------
