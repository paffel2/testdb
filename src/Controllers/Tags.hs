{-# LANGUAGE OverloadedStrings #-}

module Controllers.Tags where

import           Control.Monad.IO.Class    (MonadIO (..))
import           Data.Aeson                (encode)
import qualified Data.ByteString.Char8     as BC
import qualified Data.ByteString.Lazy      as LBS
import           FromRequest               (takeToken, toEditTag, toPage,
                                            toTagName)
import           Logger                    (Handle, logError, logInfo)
import           Network.HTTP.Types.Method (methodDelete, methodGet, methodPost,
                                            methodPut)
import           Network.Wai               (Request (rawPathInfo, requestMethod),
                                            Response)
import           Network.Wai.Parse         (lbsBackEnd, parseRequestBody)
import           OperationsHandle          (TagsHandle (create_tag_in_db, delete_tag_from_db, edit_tag_in_db, get_tags_list_from_db))
import           Responses                 (responseBadRequest, responseCreated,
                                            responseForbidden,
                                            responseMethodNotAllowed,
                                            responseNotFound, responseOKJSON,
                                            responseOk)
import           Types.Other               (TokenLifeTime)

sendTagsList :: MonadIO m => Handle m -> TagsHandle m -> Request -> m Response
sendTagsList hLogger operations req =
    if requestMethod req /= methodGet
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo hLogger "Preparing parameters for sending tags list."
            tags_list <- get_tags_list_from_db operations hLogger page
            case tags_list of
                Left bs -> do
                    logError hLogger "Tags list not sended"
                    return $ responseBadRequest bs
                Right tl -> do
                    logInfo hLogger "Tags list sended"
                    return $ responseOKJSON $ encode tl
  where
    page = toPage req

newTag ::
       MonadIO m
    => Handle m
    -> TagsHandle m
    -> TokenLifeTime
    -> Request
    -> m Response
newTag hLogger operations token_lifetime req =
    if requestMethod req /= methodPost
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo hLogger "Preparing data for creating tag."
            let token' = takeToken req
            let tag_name_param = toTagName req
            result <-
                create_tag_in_db
                    operations
                    hLogger
                    token_lifetime
                    token'
                    tag_name_param
            case result of
                Left "Not admin" -> do
                    logError hLogger "Tag not created. Not admin."
                    return $ responseForbidden "Not admin"
                Left "Bad token" -> do
                    logError hLogger "Tag not created. Bad token."
                    return $ responseForbidden "Bad token"
                Left bs -> do
                    logError hLogger "Tag not created."
                    return $ responseBadRequest bs
                Right n -> do
                    logInfo hLogger "Tag created."
                    return $ responseCreated $ LBS.fromStrict $ BC.pack $ show n

deleteTag ::
       MonadIO m
    => Handle m
    -> TagsHandle m
    -> TokenLifeTime
    -> Request
    -> m Response
deleteTag hLogger operations token_lifetime req =
    if requestMethod req /= methodDelete
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo hLogger "Preparing data for deleting tag."
            let token' = takeToken req
            let tag_name_param = toTagName req
            result <-
                delete_tag_from_db
                    operations
                    hLogger
                    token_lifetime
                    token'
                    tag_name_param
            case result of
                Left "Not admin" -> do
                    logError hLogger "Tag not deleted. Not admin."
                    return $ responseForbidden "Not admin"
                Left "Bad token" -> do
                    logError hLogger "Tag not deleted. Bad token."
                    return $ responseForbidden "Bad token"
                Left bs -> do
                    logError hLogger "Tag not deleted."
                    return $ responseBadRequest bs
                Right bs -> do
                    logInfo hLogger "Tag deleted."
                    return $ responseOk bs

editTag ::
       MonadIO m
    => Handle m
    -> TagsHandle m
    -> TokenLifeTime
    -> Request
    -> m Response
editTag hLogger operations token_lifetime req =
    if requestMethod req /= methodPut
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo hLogger "Preparing data for editing tag."
            let token' = takeToken req
            (i, _) <- liftIO $ parseRequestBody lbsBackEnd req
            let tag_edit_params = toEditTag i
            result <-
                edit_tag_in_db
                    operations
                    hLogger
                    token_lifetime
                    token'
                    tag_edit_params
            case result of
                Left "Not admin" -> do
                    logError hLogger "Tag not edited. Not admin."
                    return $ responseForbidden "Not admin"
                Left "Bad token" -> do
                    logError hLogger "Tag not edited. Bad token."
                    return $ responseForbidden "Bad token"
                Left bs -> do
                    logError hLogger "Tag not edited."
                    return $ responseBadRequest bs
                Right bs -> do
                    logInfo hLogger "Tag edited."
                    return $ responseOk bs

tagsRouter ::
       MonadIO m
    => Handle m
    -> TagsHandle m
    -> TokenLifeTime
    -> Request
    -> m Response
tagsRouter hLogger operations token_lifetime req
    | pathElemsC == 1 = sendTagsList hLogger operations req
    | pathElemsC == 2 =
        case last pathElems of
            "create_tag" -> newTag hLogger operations token_lifetime req
            "delete_tag" -> deleteTag hLogger operations token_lifetime req
            "edit_tag"   -> editTag hLogger operations token_lifetime req
            _            -> return $ responseNotFound "Not Found"
    | otherwise = return $ responseNotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
