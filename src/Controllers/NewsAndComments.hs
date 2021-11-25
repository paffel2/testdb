{-# LANGUAGE OverloadedStrings #-}

module Controllers.NewsAndComments where

import           Answer                  (answer)
import           Answers.NewsAndComments (deleteCommentHandle,
                                          getNewsByIdHandle, getNewsHandle,
                                          postCommentHandle, sendCommentsHandle)
import           Control.Monad.Except    (ExceptT, MonadIO, runExceptT)
import           Data.Aeson              (encode)
import qualified Data.ByteString.Char8   as BC
import           Logger                  (LoggerHandle, logError, logInfo)
import           Network.Wai             (Request (rawPathInfo))
import           OperationsHandle        (NewsAndCommentsHandle)
import           Responses               (toResponseErrorMessage')
import           Text.Read               (readMaybe)
import           Types.NewsAndComments   (CommentArray, GetNews, NewsArray)
import           Types.Other             (ResponseErrorMessage (BadRequest, NotFound),
                                          ResponseOkMessage (Created, OkJSON, OkMessage),
                                          SomeError)

newsAndCommentsRouter ::
       MonadIO m
    => NewsAndCommentsHandle (ExceptT SomeError m)
    -> LoggerHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
newsAndCommentsRouter operations hLogger req
    | pathElemC == 1 =
        getNewsSendResult hLogger $ answer req (getNewsHandle operations)
    | pathElemC == 2 =
        getNewsByIdSendResult hLogger $
        answer req (getNewsByIdHandle operations)
    | pathElemC == 3 =
        if last pathElems == "comments"
            then sendCommentsSendResult hLogger $
                 answer req (sendCommentsHandle operations)
            else do
                return $ Left $ NotFound "Not Found"
    | pathElemC == 4 =
        case last pathElems of
            "add_comment" ->
                postCommentSendResult hLogger $
                answer req (postCommentHandle operations)
            "delete_comment" -> do
                let newsId =
                        readMaybe $ BC.unpack $ head $ tail pathElems :: Maybe Int
                case newsId of
                    Nothing -> do
                        logError hLogger "Bad news id"
                        return $ Left $ BadRequest "Bad news id"
                    Just _ ->
                        deleteCommentSendResult hLogger $
                        answer req (deleteCommentHandle operations)
            _ -> do
                logError hLogger "Bad url"
                return $ Left $ NotFound "Not Found"
    | otherwise = do
        logError hLogger "Bad url"
        return $ Left $ NotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemC = length pathElems

postCommentSendResult ::
       Monad m
    => LoggerHandle m
    -> ExceptT SomeError m ()
    -> m (Either ResponseErrorMessage ResponseOkMessage)
postCommentSendResult hLogger result = do
    a <- runExceptT result
    case a of
        Left someError ->
            Left <$>
            toResponseErrorMessage' hLogger "Commentary not added." someError
        Right _ -> do
            logInfo hLogger "Commentary added."
            return $ Right $ Created "Commentary added."

getNewsByIdSendResult ::
       Monad m
    => LoggerHandle m
    -> ExceptT SomeError m GetNews
    -> m (Either ResponseErrorMessage ResponseOkMessage)
getNewsByIdSendResult hLogger result = do
    a <- runExceptT result
    case a of
        Left someError ->
            Left <$>
            toResponseErrorMessage' hLogger "News not sended." someError
        Right someNews -> do
            logInfo hLogger "News sended."
            return $ Right $ OkJSON $ encode someNews

getNewsSendResult ::
       Monad m
    => LoggerHandle m
    -> ExceptT SomeError m NewsArray
    -> m (Either ResponseErrorMessage ResponseOkMessage)
getNewsSendResult hLogger result = do
    a <- runExceptT result
    case a of
        Left someError ->
            Left <$>
            toResponseErrorMessage' hLogger "News not sended." someError
        Right someNews -> do
            logInfo hLogger "News sended."
            return $ Right $ OkJSON $ encode someNews

deleteCommentSendResult ::
       Monad m
    => LoggerHandle m
    -> ExceptT SomeError m ()
    -> m (Either ResponseErrorMessage ResponseOkMessage)
deleteCommentSendResult hLogger result = do
    a <- runExceptT result
    case a of
        Left someError ->
            Left <$>
            toResponseErrorMessage' hLogger "Commentary not deleted." someError
        Right _ -> do
            logInfo hLogger "Commentary deleted."
            return $ Right $ OkMessage "Commentary deleted."

sendCommentsSendResult ::
       Monad m
    => LoggerHandle m
    -> ExceptT SomeError m CommentArray
    -> m (Either ResponseErrorMessage ResponseOkMessage)
sendCommentsSendResult hLogger result = do
    a <- runExceptT result
    case a of
        Left someError ->
            Left <$>
            toResponseErrorMessage' hLogger "Commentaries not sended." someError
        Right someNews -> do
            logInfo hLogger "Commentaries sended."
            return $ Right $ OkJSON $ encode someNews
