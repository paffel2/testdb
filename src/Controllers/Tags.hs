{-# LANGUAGE OverloadedStrings #-}

module Controllers.Tags where

import           Data.Aeson                (encode)
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as LBS
import           FromRequest               (takeToken, toEditTag, toPage,
                                            toTagName)
import           Logger                    (logError, logInfo)
import           Network.HTTP.Types.Method (methodDelete, methodGet, methodPost,
                                            methodPut)
import           Network.Wai               (Request (rawPathInfo, requestMethod))
import           OperationsHandle          (TagsHandle (thCreateTagInDb, thDeleteTagFromDb, thEditTagInDb, thGetTagsListFromDb, thLogger, thParseRequestBody))
import           Responses                 (toResponseErrorMessage)
import           Types.Other               (ResponseErrorMessage (MethodNotAllowed, NotFound),
                                            ResponseOkMessage (Created, OkJSON, OkMessage))

getTagsList ::
       Monad m
    => TagsHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
getTagsList operations req =
    if requestMethod req /= methodGet
        then do
            logError (thLogger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo
                (thLogger operations)
                "Preparing parameters for sending tags list."
            tagsList <- thGetTagsListFromDb operations page
            case tagsList of
                Left someError ->
                    return $
                    Left $
                    toResponseErrorMessage "List of tags not sended." someError
                Right someList -> do
                    return $ Right $ OkJSON $ encode someList
  where
    page = toPage req

postTag ::
       Monad m
    => TagsHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
postTag operations req =
    if requestMethod req /= methodPost
        then do
            logError (thLogger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo (thLogger operations) "Preparing data for creating tag."
            let token = takeToken req
            let tagNameParam = toTagName req
            result <- thCreateTagInDb operations token tagNameParam
            case result of
                Left someError ->
                    return $
                    Left $ toResponseErrorMessage "Tag not created." someError
                Right n -> do
                    return $ Right $ Created $ LBS.fromStrict $ BC.pack $ show n

deleteTag ::
       Monad m
    => TagsHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
deleteTag operations req =
    if requestMethod req /= methodDelete
        then do
            logError (thLogger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo (thLogger operations) "Preparing data for deleting tag."
            let token = takeToken req
            let tagNameParam = toTagName req
            result <- thDeleteTagFromDb operations token tagNameParam
            case result of
                Left someError ->
                    return $
                    Left $ toResponseErrorMessage "Tag not deleted." someError
                Right _ -> do
                    return $ Right $ OkMessage "Tag deleted."

updateTag ::
       Monad m
    => TagsHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
updateTag operations req =
    if requestMethod req /= methodPut
        then do
            logError (thLogger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo (thLogger operations) "Preparing data for editing tag."
            let token = takeToken req
            (i, _) <- thParseRequestBody operations req
            let tagEditParams = toEditTag i
            result <- thEditTagInDb operations token tagEditParams
            case result of
                Left someError ->
                    return $
                    Left $ toResponseErrorMessage "Tag not edited." someError
                Right _ -> do
                    return $ Right $ OkMessage "Tag edited."

tagsRouter ::
       Monad m
    => TagsHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
tagsRouter operations req
    | pathElemsC == 1 = getTagsList operations req
    | pathElemsC == 2 =
        case last pathElems of
            "create_tag" -> postTag operations req
            "delete_tag" -> deleteTag operations req
            "edit_tag"   -> updateTag operations req
            _            -> return $ Left $ NotFound "Not Found"
    | otherwise = return $ Left $ NotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
