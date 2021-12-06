{-# LANGUAGE OverloadedStrings #-}

module Controllers.NewsAndComments where

import           Answer                  (answer)
import           Answers.NewsAndComments (deleteCommentHandle,
                                          getNewsByIdHandle, getNewsHandle,
                                          postCommentHandle, sendCommentsHandle)
import           Control.Monad.Except    (ExceptT, MonadIO)
import           Data.Aeson              (encode)
import qualified Data.ByteString.Char8   as BC
import           HelpFunction            (sendResult)
import           Logger                  (LoggerHandle, logError)
import           Network.Wai             (Request (rawPathInfo))
import           OperationsHandle        (NewsAndCommentsHandle)
import           Text.Read               (readMaybe)
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
        sendResult hLogger "News not sended." listOfNewsOK $
        answer req (getNewsHandle operations)
    | pathElemC == 2 =
        sendResult hLogger "News not sended." listOfNewsOK $
        answer req (getNewsByIdHandle operations)
    | pathElemC == 3 =
        if last pathElems == "comments"
            then sendResult hLogger "Commentaries not sended." listOfNewsOK $
                 answer req (sendCommentsHandle operations)
            else do
                return $ Left $ NotFound "Not Found"
    | pathElemC == 4 =
        case last pathElems of
            "add_comment" ->
                sendResult hLogger "Commentary not added." postCommentOk $
                answer req (postCommentHandle operations)
            "delete_comment" -> do
                let newsId =
                        readMaybe $ BC.unpack $ head $ tail pathElems :: Maybe Int
                case newsId of
                    Nothing -> do
                        logError hLogger "Bad news id"
                        return $ Left $ BadRequest "Bad news id"
                    Just _ ->
                        sendResult
                            hLogger
                            "Commentary not deleted."
                            deleteCommentOk $
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
    listOfNewsOK someNews = OkJSON $ encode someNews
    postCommentOk _ = Created "Commentary added."
    deleteCommentOk _ = OkMessage "Commentary deleted."
