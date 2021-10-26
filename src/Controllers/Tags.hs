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
import           OperationsHandle          (TagsHandle (create_tag_in_db, delete_tag_from_db, edit_tag_in_db, get_tags_list_from_db, tags_logger, tags_parse_request_body))
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
            logError (tags_logger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo
                (tags_logger operations)
                "Preparing parameters for sending tags list."
            tags_list <- get_tags_list_from_db operations page
            case tags_list of
                Left someError ->
                    return $
                    Left $
                    toResponseErrorMessage "List of tags not sended." someError
                Right tl -> do
                    return $ Right $ OkJSON $ encode tl
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
            logError (tags_logger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo (tags_logger operations) "Preparing data for creating tag."
            let token' = takeToken req
            let tag_name_param = toTagName req
            result <- create_tag_in_db operations token' tag_name_param
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
            logError (tags_logger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo (tags_logger operations) "Preparing data for deleting tag."
            let token' = takeToken req
            let tag_name_param = toTagName req
            result <- delete_tag_from_db operations token' tag_name_param
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
            logError (tags_logger operations) "Bad request method"
            return $ Left $ MethodNotAllowed "Bad request method"
        else do
            logInfo (tags_logger operations) "Preparing data for editing tag."
            let token' = takeToken req
            (i, _) <- tags_parse_request_body operations req
            let tag_edit_params = toEditTag i
            result <- edit_tag_in_db operations token' tag_edit_params
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
