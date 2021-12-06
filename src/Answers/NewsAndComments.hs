{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Answers.NewsAndComments where

import           Answer                    (AnswerHandle (..))
import           Control.Monad.Except      (MonadError (throwError))
import qualified Data.ByteString.Char8     as BC
import           FromRequest               (takeToken, toCommentId,
                                            toCommentText, toFilterParams,
                                            toPage, toSort)
import           HelpFunction              (readByteStringToId)
import           Network.HTTP.Types.Method (methodDelete, methodGet, methodPost)
import           Network.Wai               (Request (rawPathInfo, requestMethod))
import           OperationsHandle          (NewsAndCommentsHandle (nchAddCommentToDb, nchDeleteCommentFromDb, nchGetCommentsByNewsIdFromDb, nchGetNewsByIdFromDb, nchGetNewsFilterByAfterDateFromDb, nchGetNewsFilterByBeforeDateFromDb, nchGetNewsFilterByCategoryIdFromDb, nchGetNewsFilterByContentFromDb, nchGetNewsFilterByDateFromDb, nchGetNewsFilterByTagAllFromDb, nchGetNewsFilterByTagIdFromDb, nchGetNewsFilterByTagInFromDb, nchGetNewsFilterByTitleFromDb, nchGetNewsFromDb, nchGetgetNewsFilterByAuthorNameFromDb, nchParseRequestBody))
import           Types.NewsAndComments     (CommentArray,
                                            CommentWithoutTokenLifeTime (..),
                                            FilterParams (..), GetNews,
                                            NewsArray, Sort)
import           Types.Other               (Id, MonadIOWithError, Page,
                                            SomeError (BadMethod, OtherError),
                                            Token)

-------------------------------------------------------------------------------------------------------------------- -}
postCommentParseInformation ::
       MonadIOWithError m
    => NewsAndCommentsHandle m
    -> Request
    -> m CommentWithoutTokenLifeTime
postCommentParseInformation handler request =
    if requestMethod request /= methodPost
        then throwError BadMethod
        else do
            (i, _) <- nchParseRequestBody handler request
            let newsId = readByteStringToId . head . tail $ pathElems
            let comment =
                    CommentWithoutTokenLifeTime
                        { commentWTLToken = takeToken request
                        , commentWTLText = toCommentText i
                        , commentWTLNewsId = newsId
                        }
            return comment
  where
    path = BC.tail $ rawPathInfo request
    pathElems = BC.split '/' path

postCommentDatabaseOperation ::
       MonadIOWithError m
    => NewsAndCommentsHandle m
    -> CommentWithoutTokenLifeTime
    -> m ()
postCommentDatabaseOperation = nchAddCommentToDb

postCommentHandle ::
       MonadIOWithError m
    => NewsAndCommentsHandle m
    -> AnswerHandle m CommentWithoutTokenLifeTime ()
postCommentHandle commentsHandle =
    AnswerHandle
        { parseInformation = postCommentParseInformation commentsHandle
        , databaseOperation = postCommentDatabaseOperation commentsHandle
        }

------------------------------------------------------------------
getNewsByIdParseInformation ::
       MonadIOWithError m => NewsAndCommentsHandle m -> Request -> m (Maybe Id)
getNewsByIdParseInformation _ request =
    if requestMethod request /= methodGet
        then throwError BadMethod
        else return (readByteStringToId . last $ pathElems)
  where
    path = BC.tail $ rawPathInfo request
    pathElems = BC.split '/' path

getNewsByIdDatabaseOperation ::
       MonadIOWithError m => NewsAndCommentsHandle m -> Maybe Id -> m GetNews
getNewsByIdDatabaseOperation = nchGetNewsByIdFromDb

getNewsByIdHandle ::
       MonadIOWithError m
    => NewsAndCommentsHandle m
    -> AnswerHandle m (Maybe Id) GetNews
getNewsByIdHandle newsHandle =
    AnswerHandle
        { parseInformation = getNewsByIdParseInformation newsHandle
        , databaseOperation = getNewsByIdDatabaseOperation newsHandle
        }

----------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------
getNewsParseInformation ::
       MonadIOWithError m
    => NewsAndCommentsHandle m
    -> Request
    -> m (FilterParams, Maybe Page, Sort)
getNewsParseInformation _ request =
    if requestMethod request /= methodGet
        then throwError BadMethod
        else return (toFilterParams request, toPage request, toSort request)

getNewsDatabaseOperation ::
       MonadIOWithError m
    => NewsAndCommentsHandle m
    -> (FilterParams, Maybe Page, Sort)
    -> m NewsArray
getNewsDatabaseOperation newsHandle (filters, page, sort) =
    case filters of
        TagIn tagIn -> nchGetNewsFilterByTagInFromDb newsHandle tagIn page
        CategoryFilter categoryFilter ->
            nchGetNewsFilterByCategoryIdFromDb
                newsHandle
                categoryFilter
                page
                sort
        TagFilter tagFilter ->
            nchGetNewsFilterByTagIdFromDb newsHandle tagFilter page sort
        TagAll tagAll ->
            nchGetNewsFilterByTagAllFromDb newsHandle tagAll page sort
        TitleFilter titleFilter ->
            nchGetNewsFilterByTitleFromDb newsHandle titleFilter page sort
        ContentFilter contentFilter ->
            nchGetNewsFilterByContentFromDb newsHandle contentFilter page sort
        DateFilter dateFilter ->
            nchGetNewsFilterByDateFromDb newsHandle dateFilter page sort
        BeforeDate beforeDate ->
            nchGetNewsFilterByBeforeDateFromDb newsHandle beforeDate page sort
        AfterDate afterDate ->
            nchGetNewsFilterByAfterDateFromDb newsHandle afterDate page sort
        AuthorFilter authorFilter ->
            nchGetgetNewsFilterByAuthorNameFromDb
                newsHandle
                authorFilter
                page
                sort
        NoFilter -> nchGetNewsFromDb newsHandle sort page
        BadFilter -> throwError $ OtherError "Bad filter param"

getNewsHandle ::
       MonadIOWithError m
    => NewsAndCommentsHandle m
    -> AnswerHandle m (FilterParams, Maybe Page, Sort) NewsArray
getNewsHandle newsHandle =
    AnswerHandle
        { parseInformation = getNewsParseInformation newsHandle
        , databaseOperation = getNewsDatabaseOperation newsHandle
        }

------------------------------------------------------------------------------------
deleteCommentParseInformation ::
       MonadIOWithError m
    => NewsAndCommentsHandle m
    -> Request
    -> m (Maybe Token, Maybe Id)
deleteCommentParseInformation _ request =
    if requestMethod request /= methodDelete
        then throwError BadMethod
        else return (takeToken request, toCommentId request)

deleteCommentDatabaseOperation ::
       MonadIOWithError m
    => NewsAndCommentsHandle m
    -> (Maybe Token, Maybe Id)
    -> m ()
deleteCommentDatabaseOperation commentsHandle (token, commentId) =
    nchDeleteCommentFromDb commentsHandle token commentId

deleteCommentHandle ::
       MonadIOWithError m
    => NewsAndCommentsHandle m
    -> AnswerHandle m (Maybe Token, Maybe Id) ()
deleteCommentHandle commentsHandle =
    AnswerHandle
        { parseInformation = deleteCommentParseInformation commentsHandle
        , databaseOperation = deleteCommentDatabaseOperation commentsHandle
        }

--------------------------------------------------------------------------------------------------------------------
sendCommentsParseInformation ::
       MonadIOWithError m
    => NewsAndCommentsHandle m
    -> Request
    -> m (Maybe Page, Maybe Id)
sendCommentsParseInformation _ request =
    if requestMethod request /= methodGet
        then throwError BadMethod
        else return
                 (toPage request, readByteStringToId . head . tail $ pathElems)
  where
    path = BC.tail $ rawPathInfo request
    pathElems = BC.split '/' path

sendCommentsDatabaseOperation ::
       MonadIOWithError m
    => NewsAndCommentsHandle m
    -> (Maybe Page, Maybe Id)
    -> m CommentArray
sendCommentsDatabaseOperation commentsHandle (page, newsId) =
    nchGetCommentsByNewsIdFromDb commentsHandle newsId page

sendCommentsHandle ::
       MonadIOWithError m
    => NewsAndCommentsHandle m
    -> AnswerHandle m (Maybe Page, Maybe Id) CommentArray
sendCommentsHandle commentsHandle =
    AnswerHandle
        { parseInformation = sendCommentsParseInformation commentsHandle
        , databaseOperation = sendCommentsDatabaseOperation commentsHandle
        }
