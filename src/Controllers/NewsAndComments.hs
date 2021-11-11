{-# LANGUAGE OverloadedStrings #-}

module Controllers.NewsAndComments where

import           Answer                  (answer)
import           Answers.NewsAndComments (deleteCommentHandle,
                                          getNewsByIdHandle, getNewsHandle,
                                          postCommentHandle, sendCommentsHandle)
import qualified Data.ByteString.Char8   as BC
import           Logger                  (logError)
import           Network.Wai             (Request (rawPathInfo))
import           OperationsHandle        (NewsAndCommentsHandle (nchLogger))
import           Text.Read               (readMaybe)
import           Types.Other             (ResponseErrorMessage (BadRequest, NotFound),
                                          ResponseOkMessage)

newsAndCommentsRouter ::
       Monad m
    => NewsAndCommentsHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
newsAndCommentsRouter operations req
    | pathElemC == 1 = answer req (getNewsHandle operations)
    | pathElemC == 2 = answer req (getNewsByIdHandle operations)
    | pathElemC == 3 =
        if last pathElems == "comments"
            then answer req (sendCommentsHandle operations)
            else do
                logError (nchLogger operations) "Bad url"
                return $ Left $ NotFound "Not Found"
    | pathElemC == 4 =
        case last pathElems of
            "add_comment" -> answer req (postCommentHandle operations)
            "delete_comment" -> do
                let newsId =
                        readMaybe $ BC.unpack $ head $ tail pathElems :: Maybe Int
                case newsId of
                    Nothing -> do
                        logError (nchLogger operations) "Bad news id"
                        return $ Left $ BadRequest "Bad news id"
                    Just _ -> answer req (deleteCommentHandle operations)
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
