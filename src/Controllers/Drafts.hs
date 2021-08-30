{-# LANGUAGE OverloadedStrings #-}

module Controllers.Drafts where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import qualified Data.Text.Encoding as E
import Database.PostgreSQL.Simple (Binary(Binary), Connection)
import Databaseoperations.Drafts
    ( checkAuthor
    , createDraftOnDb'
    , deleteDraftFromDb
    , getDraftByIdFromDb
    , getDraftsByAuthorToken
    , publicNewsOnDb
    , updateDraftInDb'
    )
import FromRequest (takeToken, toImage)
import HelpFunction (foundParametr, readByteStringToInt)
import Logger (Handle, logError, logInfo)
import Network.Wai (Request(queryString, rawPathInfo), Response)
import Network.Wai.Parse
    ( FileInfo(fileContent)
    , lbsBackEnd
    , noLimitParseRequestBodyOptions
    , parseRequestBodyEx
    )
import Responses (responseBadRequest, responseOKJSON, responseOk)
import Types (Image(Image, image_content_type), TokenLifeTime)

sendDrafts ::
       Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
sendDrafts hLogger pool token_liferime req = do
    let token' = E.decodeUtf8 <$> takeToken req
    drafts' <- getDraftsByAuthorToken hLogger pool token_liferime token'
    case drafts' of
        Left bs -> return $ responseBadRequest bs
        Right draftsA -> do
            logInfo hLogger "Sending drafts to user"
            return $ responseOKJSON (encode draftsA)

createDraft ::
       Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
createDraft hLogger pool token_lifetime req = do
    (i, f) <- parseRequestBodyEx noLimitParseRequestBodyOptions lbsBackEnd req
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
            let token' = takeToken req
            let category = E.decodeUtf8 <$> lookup "category" i
            let tags_list = lookup "tags" i
            let short'_title = E.decodeUtf8 <$> lookup "short_title" i
            let text = E.decodeUtf8 <$> lookup "news_text" i
            result <-
                createDraftOnDb'
                    hLogger
                    pool
                    token_lifetime
                    token'
                    category
                    tags_list
                    short'_title
                    text
                    main_image_triple
                    images_list
            case result of
                Left bs -> return $ responseBadRequest bs
                Right n ->
                    return $ responseOk $ LBS.fromStrict $ BC.pack $ show n

deleteDraft ::
       Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
deleteDraft hLogger pool token_lifetime req = do
    let token' = E.decodeUtf8 <$> takeToken req
    ca <- checkAuthor hLogger pool token_lifetime token'
    case ca of
        Left bs -> return $ responseBadRequest bs
        Right n -> do
            let draft_id =
                    fromMaybe Nothing (lookup "draft_id" $ queryString req)
            result <- deleteDraftFromDb hLogger n draft_id
            case result of
                Left bs -> return $ responseBadRequest bs
                Right bs -> return $ responseOk bs

getDraftById ::
       Handle
    -> Pool Connection
    -> TokenLifeTime
    -> Int
    -> Request
    -> IO Response
getDraftById hLogger pool token_lifetime draft_id req = do
    let token' = E.decodeUtf8 <$> takeToken req
    result <- getDraftByIdFromDb hLogger pool token_lifetime token' draft_id
    case result of
        Left bs -> return $ responseBadRequest bs
        Right draft -> return $ responseOKJSON $ encode draft

updateDraft ::
       Handle
    -> Pool Connection
    -> TokenLifeTime
    -> Int
    -> Request
    -> IO Response
updateDraft hLogger pool token_lifetime draft_id req = do
    let token' = takeToken req
    (i, f) <- parseRequestBodyEx noLimitParseRequestBodyOptions lbsBackEnd req
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
    let category = E.decodeUtf8 <$> lookup "category" i
    let tags_list = lookup "tags" i
    let short'_title = E.decodeUtf8 <$> lookup "short_title" i
    let text = E.decodeUtf8 <$> lookup "news_text" i
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
                updateDraftInDb'
                    hLogger
                    pool
                    token_lifetime
                    token'
                    category
                    tags_list
                    short'_title
                    text
                    main_image_triple
                    images_list
                    draft_id
            case result of
                Left bs -> return $ responseBadRequest bs
                Right bs -> return $ responseOk bs

publicNews ::
       Handle
    -> Pool Connection
    -> TokenLifeTime
    -> Int
    -> Request
    -> IO Response
publicNews hLogger pool token_lifetime draft_id req = do
    let token' = E.decodeUtf8 <$> takeToken req
    ca <- checkAuthor hLogger pool token_lifetime token'
    case ca of
        Left bs -> return $ responseBadRequest bs
        Right author'_id -> do
            result <- publicNewsOnDb hLogger pool author'_id draft_id
            case result of
                Left bs -> return $ responseBadRequest bs
                Right n ->
                    return $ responseOk $ LBS.fromStrict $ BC.pack $ show n

draftsBlock ::
       Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
draftsBlock hLogger pool token_lifetime req
    | pathElemsC == 1 = sendDrafts hLogger pool token_lifetime req
    | pathElemsC == 2 =
        case readByteStringToInt $ last pathElems of
            Just n -> getDraftById hLogger pool token_lifetime n req
            Nothing ->
                case last pathElems of
                    "delete_draft" ->
                        deleteDraft hLogger pool token_lifetime req
                    _ -> return $ responseBadRequest "bad request"
    | pathElemsC == 3 =
        case readByteStringToInt $ head $ tail pathElems of
            Nothing -> return $ responseBadRequest "bad request"
            Just n ->
                case last pathElems of
                    "update_draft" ->
                        updateDraft hLogger pool token_lifetime n req
                    "public_news" ->
                        publicNews hLogger pool token_lifetime n req
                    _ -> return $ responseBadRequest "bad request"
    | otherwise = return $ responseBadRequest "bad request"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
