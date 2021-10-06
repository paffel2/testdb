{-# LANGUAGE OverloadedStrings #-}

module Controllers.Drafts where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Binary(Binary), Connection)
import FromRequest (takeToken, toDraftId, toDraftInf, toDraftTags, toImage)
import HelpFunction (foundParametr, readByteStringToId)
import Logger (Handle, logError, logInfo)
import Network.HTTP.Types.Method
    ( methodDelete
    , methodGet
    , methodPost
    , methodPut
    )
import Network.Wai (Request(rawPathInfo, requestMethod), Response)
import Network.Wai.Parse
    ( FileInfo(fileContent)
    , lbsBackEnd
    , noLimitParseRequestBodyOptions
    , parseRequestBodyEx
    )
import OperationsHandle
    ( DraftsHandle(create_draft_on_db, delete_draft_from_db,
             get_draft_by_id_from_db, get_drafts_by_author_token,
             public_news_on_db, update_draft_in_db)
    )
import Responses
    ( responseBadRequest
    , responseCreated
    , responseForbidden
    , responseMethodNotAllowed
    , responseNotFound
    , responseOKJSON
    , responseOk
    )
import Types.Images (Image(Image, image_content_type))
import Types.Other (Id, TokenLifeTime)

sendDrafts ::
       MonadIO m
    => Handle m
    -> DraftsHandle m
    -> Pool Connection
    -> TokenLifeTime
    -> Request
    -> m Response
sendDrafts hLogger operations pool token_liferime req =
    if requestMethod req /= methodGet
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo hLogger "Preparing data for sending drafts"
            let token' = takeToken req
            drafts' <-
                get_drafts_by_author_token
                    operations
                    hLogger
                    pool
                    token_liferime
                    token'
            case drafts' of
                Left bs -> return $ responseBadRequest bs
                Right draftsA -> do
                    logInfo hLogger "Sending drafts to user"
                    return $ responseOKJSON (encode draftsA)

createDraft ::
       MonadIO m
    => Handle m
    -> DraftsHandle m
    -> Pool Connection
    -> TokenLifeTime
    -> Request
    -> m Response
createDraft hLogger operations pool token_lifetime req =
    if requestMethod req /= methodPost
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo hLogger "Preparing data for creating draft"
            (i, f) <-
                liftIO $
                parseRequestBodyEx noLimitParseRequestBodyOptions lbsBackEnd req
            let draft_inf = toDraftInf req i
            let list_of_tags = toDraftTags i
            let main'_image = foundParametr "main_image" f
            let images = foundParametr "images" f
            let main_image_triple =
                    if fileContent (Prelude.head main'_image) == ""
                        then Nothing
                        else Just $ toImage $ Prelude.head main'_image
            let images_list =
                    if fileContent (Prelude.head images) == ""
                        then Nothing
                        else Just $ toImage <$> images
            let con_type =
                    any
                        (/= "image")
                        (BC.take 5 . image_content_type <$>
                         fromMaybe [Image "" "" (Binary "")] images_list)
            if ((BC.take
                     5
                     (image_content_type $
                      fromMaybe (Image "" "" (Binary "")) main_image_triple) /=
                 "image") &&
                main_image_triple /= Just (Image "" "" (Binary ""))) ||
               (fromMaybe [] images_list /= [] && con_type)
                then do
                    logError hLogger "Bad image file"
                    return $ responseBadRequest "Bad image file"
                else do
                    result <-
                        create_draft_on_db
                            operations
                            hLogger
                            pool
                            token_lifetime
                            draft_inf
                            list_of_tags
                            main_image_triple
                            images_list
                    case result of
                        Left "Bad token" ->
                            return $ responseForbidden "Bad token"
                        Left bs -> do
                            logError hLogger "Draft not created."
                            return $ responseBadRequest bs
                        Right n -> do
                            logInfo hLogger "Draft created."
                            return $
                                responseCreated $
                                LBS.fromStrict $ BC.pack $ show n

deleteDraft ::
       MonadIO m
    => Handle m
    -> DraftsHandle m
    -> Pool Connection
    -> TokenLifeTime
    -> Request
    -> m Response
deleteDraft hLogger operations pool token_lifetime req =
    if requestMethod req /= methodDelete
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo hLogger "Preparing data for deleting draft"
            let token' = takeToken req
            let draft_id = toDraftId req
            result <-
                delete_draft_from_db
                    operations
                    hLogger
                    pool
                    token_lifetime
                    token'
                    draft_id
            case result of
                Left "Bad token" -> return $ responseForbidden "Bad token"
                Left bs -> do
                    logError hLogger "Draft not deleted."
                    return $ responseBadRequest bs
                Right bs -> do
                    logInfo hLogger "Draft deletedd."
                    return $ responseOk bs

getDraftById ::
       MonadIO m
    => Handle m
    -> DraftsHandle m
    -> Pool Connection
    -> TokenLifeTime
    -> Id
    -> Request
    -> m Response
getDraftById hLogger operations pool token_lifetime draft_id req =
    if requestMethod req /= methodGet
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo hLogger "Preparing data for sending draft"
            let token' = takeToken req
            result <-
                get_draft_by_id_from_db
                    operations
                    hLogger
                    pool
                    token_lifetime
                    token'
                    draft_id
            case result of
                Left "Bad token" -> return $ responseForbidden "Bad token"
                Left bs -> do
                    logError hLogger "Draft not sended."
                    return $ responseBadRequest bs
                Right draft -> do
                    logInfo hLogger "Draft not sended."
                    return $ responseOKJSON $ encode draft

updateDraft ::
       MonadIO m
    => Handle m
    -> DraftsHandle m
    -> Pool Connection
    -> TokenLifeTime
    -> Id
    -> Request
    -> m Response
updateDraft hLogger operations pool token_lifetime draft_id req =
    if requestMethod req /= methodPut
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo hLogger "Preparing data for updating draft"
            (i, f) <-
                liftIO $
                parseRequestBodyEx noLimitParseRequestBodyOptions lbsBackEnd req
            let dr_inf_update = toDraftInf req i
            let list_of_tags = toDraftTags i
            let main'_image = foundParametr "main_image" f
            let images = foundParametr "images" f
            let main_image_triple =
                    if fileContent (Prelude.head main'_image) == ""
                        then Nothing
                        else Just $ toImage $ Prelude.head main'_image
            let images_list =
                    if fileContent (Prelude.head images) == ""
                        then Nothing
                        else Just $ toImage <$> images
            let con_type =
                    any
                        (/= "image")
                        (BC.take 5 . image_content_type <$>
                         fromMaybe [Image "" "" (Binary "")] images_list)
            if ((BC.take
                     5
                     (image_content_type $
                      fromMaybe (Image "" "" (Binary "")) main_image_triple) /=
                 "image") &&
                main_image_triple /= Just (Image "" "" (Binary ""))) ||
               (fromMaybe [] images_list /= [] && con_type)
                then do
                    logError hLogger "Bad image file"
                    return $ responseBadRequest "Bad image file"
                else do
                    result <-
                        update_draft_in_db
                            operations
                            hLogger
                            pool
                            token_lifetime
                            dr_inf_update
                            list_of_tags
                            main_image_triple
                            images_list
                            draft_id
                    case result of
                        Left "Bad token" ->
                            return $ responseForbidden "Bad token"
                        Left bs -> do
                            logError hLogger "Draft not updated"
                            return $ responseBadRequest bs
                        Right bs -> do
                            logInfo hLogger "Draft updated"
                            return $ responseOk bs

publicNews ::
       MonadIO m
    => Handle m
    -> DraftsHandle m
    -> Pool Connection
    -> TokenLifeTime
    -> Id
    -> Request
    -> m Response
publicNews hLogger operations pool token_lifetime draft_id req =
    if requestMethod req /= methodPut
        then do
            logError hLogger "Bad request method"
            return $ responseMethodNotAllowed "Bad method request"
        else do
            logInfo hLogger "Preparing data for public news"
            let token' = takeToken req
            result <-
                public_news_on_db
                    operations
                    hLogger
                    pool
                    token_lifetime
                    token'
                    draft_id
            case result of
                Left "Bad token" -> return $ responseForbidden "Bad token"
                Left bs -> do
                    logError hLogger "News not created"
                    return $ responseBadRequest bs
                Right n -> do
                    logInfo hLogger "News published"
                    return $ responseCreated $ LBS.fromStrict $ BC.pack $ show n

draftsRouter ::
       MonadIO m
    => Handle m
    -> DraftsHandle m
    -> Pool Connection
    -> TokenLifeTime
    -> Request
    -> m Response
draftsRouter hLogger operations pool token_lifetime req
    | pathElemsC == 1 = sendDrafts hLogger operations pool token_lifetime req
    | pathElemsC == 2 =
        case readByteStringToId $ last pathElems of
            Just n -> getDraftById hLogger operations pool token_lifetime n req
            Nothing ->
                case last pathElems of
                    "delete_draft" ->
                        deleteDraft hLogger operations pool token_lifetime req
                    _ -> return $ responseNotFound "Not Found"
    | pathElemsC == 3 =
        case readByteStringToId $ head $ tail pathElems of
            Nothing -> return $ responseBadRequest "bad request"
            Just n ->
                case last pathElems of
                    "update_draft" ->
                        updateDraft hLogger operations pool token_lifetime n req
                    "public_news" ->
                        publicNews hLogger operations pool token_lifetime n req
                    _ -> return $ responseNotFound "Not Found"
    | otherwise = return $ responseNotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
