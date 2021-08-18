{-# LANGUAGE OverloadedStrings #-}
module Draftshandle where
import Network.Wai
--import Network.Wai.Handler.Warp
import Network.Wai.Parse
--import Network.HTTP.Types
--import GHC.Generics
import Data.Aeson
--import Testdatabase
--import Control.Exception
--import qualified Data.Text.Lazy.Encoding as EL
--import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.Encoding as E
--import qualified Data.Text as T
--import qualified Data.Text.IO as TIO
--import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BC
import Types
import Data.Maybe
--import Text.Read
--import Control.Applicative
import HelpFunction
import Responses
--import NewsAndComments
--import Categories
--import Users
import Logger
import FromRequest
import Databaseoperations
import Data.Pool
import Database.PostgreSQL.Simple
--import qualified Data.Text.IO as TIO
--import Database.PostgreSQL.Simple

{-sendDrafts :: Handle -> Request -> IO Response
sendDrafts hLogger req = do
    let token = takeToken req--fromMaybe Nothing (lookup "token" $ queryString req)
    cauth <- Databaseoperations.checkAuthor hLogger $ E.decodeUtf8 $ fromMaybe "" token
    case cauth of
      Left bs -> return $ responseBadRequest bs
      Right n -> do
            drafts <- getDraftsByAuthorId n
            return $ responseOk (encode drafts)-}
sendDrafts :: Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
sendDrafts hLogger pool token_liferime req = do
    let token' = E.decodeUtf8 <$> takeToken req--fromMaybe Nothing (lookup "token" $ queryString req)
    drafts' <- getDraftsByAuthorToken hLogger pool token_liferime token'
    case drafts' of
      Left bs -> return $ responseBadRequest bs
      Right draftsA -> do
            logInfo hLogger "Sending drafts to user"
            return $ responseOk (encode draftsA)

{-createDraft' :: Request -> IO Response
createDraft' req = do
    (i,f) <- parseRequestBodyEx noLimitParseRequestBodyOptions lbsBackEnd req
    let main_image = foundParametr "main_image" f
    let images = foundParametr "images" f
    let main_image_triple = if fileContent (Prelude.head main_image) == "" then ("","","")
        else (BC.unpack $ fileName $ Prelude.head main_image, BC.unpack $ fileContentType $ Prelude.head main_image, fileContent $ Prelude.head main_image  )
    let images_list = if fileContent (Prelude.head images) == "" then []
        else toTriple images
    let con_type = any (/= "image") (take 5 . sndTriple <$> images_list)
    if ((Prelude.take 5  (sndTriple main_image_triple) /= "image") && main_image_triple /= ("","","")) || (images_list /= [] && con_type)
                then return $ responseOk "bad image"       
                else do
                    let token = E.decodeUtf8 $ fromMaybe "" (fromMaybe Nothing (lookup "token" $ queryString req))
                    ca <- checkAuthor' token
                    case ca of
                        Nothing -> return $ responseOk "author not founded"
                        Just a_id -> do
                                let cat = lookup "category" i
                                case cat of
                                    Nothing -> return $ responseBadRequest "no category field"
                                    Just cat' -> do
                                        c_id <- checkCategory $ T.toLower $ E.decodeUtf8 cat'
                                        case c_id of
                                            Nothing -> return $ responseBadRequest "category not exsist"
                                            Just cat_id -> do
                                                    let t = lookup "tags" i
                                                    case t of
                                                        Nothing -> return $ responseBadRequest "no tags field"
                                                        Just tg -> do
                                                            let sht = lookup "short_title" i
                                                            case sht of
                                                                Nothing -> return $ responseBadRequest "no short title field"
                                                                Just sh -> do
                                                                    let sh_title = E.decodeUtf8  sh
                                                                    --let tags = T.toLower $ E.decodeUtf8 tg
                                                                    tags_ids <- checkTag $ splitOnPunctuationMark $ T.toLower $ E.decodeUtf8 tg
                                                                    case tags_ids of
                                                                        Left bs -> return $ responseBadRequest bs
                                                                        Right tag_list -> do
                                                                            if T.length sh_title > 20 then
                                                                                return $ responseBadRequest "too long title"
                                                                            else do
                                                                                let text = E.decodeUtf8 $ fromMaybe "" (lookup "news_text" i)
                                                                                result <- createDraftOnDb main_image_triple images_list a_id cat_id tag_list text sh_title
                                                                                case result of
                                                                                    Left bs -> return $ responseBadRequest bs
                                                                                    Right n -> return $ responseOk $ LBS.fromStrict $ BC.pack $ show n-}
createDraft :: Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
createDraft hLogger pool token_lifetime req = do
    (i,f) <- parseRequestBodyEx noLimitParseRequestBodyOptions lbsBackEnd req
    let main'_image = foundParametr "main_image" f
    let images = foundParametr "images" f
    let main_image_triple = if fileContent (Prelude.head main'_image) == "" then ("","","")
        else (BC.unpack $ fileName $ Prelude.head main'_image, BC.unpack $ fileContentType $ Prelude.head main'_image, fileContent $ Prelude.head main'_image  )
    let main_image_triple' = if fileContent (Prelude.head main'_image) == "" then Nothing 
        else Just $ toImage $ Prelude.head main'_image--Image''' (fileName $ Prelude.head main_image) (fileContentType $ Prelude.head main_image) (Binary $ fileContent $ Prelude.head main_image)
    let images_list = if fileContent (Prelude.head images) == "" then []
        else toTriple images
    let images_list' = if fileContent (Prelude.head images) == "" then Nothing
        else Just $ toImage <$> images
    let con_type = any (/= "image") (take 5 . sndTriple <$> images_list)
    if ((Prelude.take 5  (sndTriple main_image_triple) /= "image") && main_image_triple /= ("","","")) || (images_list /= [] && con_type)
                then do
                    logError hLogger "Bad image file"
                    return $ responseBadRequest "Bad image file"       
                else do
                    let token' = takeToken req
                    let category = E.decodeUtf8 <$> lookup "category" i
                    let tags_list = lookup "tags" i
                    --TIO.putStrLn $ E.decodeUtf8 $ fromMaybe "" tags_list
                    let short'_title = E.decodeUtf8 <$> lookup "short_title" i
                    let text = E.decodeUtf8 <$> lookup "news_text" i
                    result <- createDraftOnDb' hLogger pool token_lifetime token' category tags_list short'_title text main_image_triple' images_list'
                    case result of
                      Left bs -> return $ responseBadRequest bs
                      Right n -> return $ responseOk $ LBS.fromStrict $ BC.pack $ show n


deleteDraft :: Handle -> Request -> IO Response 
deleteDraft hLogger req = do
    let token' = E.decodeUtf8 <$> takeToken req
    ca <- Databaseoperations.checkAuthor hLogger token'
    case ca of
        Left bs -> return $ responseBadRequest bs
        Right n -> do
            let draft_id = fromMaybe Nothing (lookup "draft_id" $ queryString req)
            result <- Databaseoperations.deleteDraftFromDb hLogger n draft_id
            case result of
                Left bs -> return $ responseBadRequest bs
                Right bs -> return $ responseOk bs


getDraftById :: Handle -> Pool Connection -> TokenLifeTime -> Int -> Request -> IO Response
getDraftById hLogger pool token_lifetime draft_id req = do
    let token' = E.decodeUtf8 <$> takeToken req
    result <- getDraftByIdFromDb hLogger pool token_lifetime token' draft_id
    case result of
        Left bs -> return $ responseBadRequest bs
        Right draft -> return $ responseOk $ encode draft


updateDraft :: Handle -> Pool Connection -> TokenLifeTime -> Int -> Request -> IO Response 
updateDraft hLogger pool token_lifetime draft_id req = do
    let token' = takeToken req
    --ca <- Databaseoperations.checkAuthor hLogger $ E.decodeUtf8 <$> token'
    --case ca of
    --    Left bs -> return $ responseBadRequest bs
    --    Right author'_id -> do
    (i,f) <- parseRequestBodyEx noLimitParseRequestBodyOptions lbsBackEnd req
    let main'_image = foundParametr "main_image" f
    let images = foundParametr "images" f
    let main_image_triple = if fileContent (Prelude.head main'_image) == "" then Nothing 
                                else Just $ toImage $ Prelude.head main'_image
    let images_list = if fileContent (Prelude.head images) == "" then Nothing
                                else Just $ toImage <$> images
            --let con_type = any (/= "image") (take 5 . sndTriple <$> images_list)
    let category = E.decodeUtf8 <$> lookup "category" i
    let tags_list = lookup "tags" i
    let short'_title = E.decodeUtf8 <$> lookup "short_title" i
    let text = E.decodeUtf8 <$> lookup "news_text" i
    result <- updateDraftInDb' hLogger pool token_lifetime token' category tags_list short'_title text main_image_triple images_list draft_id
            --let result = Left ""
    case result of
        Left bs -> return $ responseBadRequest bs
        Right bs -> return $ responseOk bs


                    {-let cat = lookup "category" i
                    case cat of
                        Nothing -> return $ responseBadRequest "no category field"
                        Just cat' -> do
                            c_id <- checkCategory $ T.toLower $ E.decodeUtf8 cat'
                            case c_id of
                                Nothing -> return $ responseBadRequest "category not exsist"
                                Just cat_id -> do
                                            let t = lookup "tags" i
                                            case t of
                                                Nothing -> return $ responseBadRequest "no tags field"
                                                Just tg -> do
                                                        let sht = lookup "short_title" i
                                                        case sht of
                                                            Nothing -> return $ responseBadRequest "no short title field"
                                                            Just sh -> do
                                                                let sh_title = E.decodeUtf8  sh
                                                                --let tags = T.toLower $ E.decodeUtf8 tg
                                                                tags_ids <- checkTag $ splitOnPunctuationMark $ T.toLower $ E.decodeUtf8 tg
                                                                case tags_ids of
                                                                    Left bs -> return $ responseBadRequest bs
                                                                    Right tag_list -> do
                                                                        if T.length sh_title > 20 then
                                                                            return $ responseBadRequest "too long title"
                                                                        else do
                                                                            let text = E.decodeUtf8 $ fromMaybe "" (lookup "news_text" i)
                                                                            result <- updateDraftInDb main_image_triple images_list author_id cat_id tag_list text sh_title draft_id
                                                                            case result of
                                                                                Left bs -> return $ responseBadRequest bs
                                                                                Right bs -> return $ responseOk bs -}

{-publicNews :: Int -> Request -> IO Response
publicNews draft_id req = do
  let token = E.decodeUtf8 $ fromMaybe "" (fromMaybe Nothing (lookup "token" $ queryString req))
  ca <- checkAuthor' token
  case ca of
    Nothing -> return $ responseBadRequest "author not founded"
    Just author_id -> do
                    result <- publicNewsOnDb author_id draft_id
                    case result of
                      Left bs -> return $ responseBadRequest bs
                      Right bs -> return $ responseOk bs-}

publicNews :: Handle ->Pool Connection -> TokenLifeTime -> Int -> Request -> IO Response
publicNews hLogger pool token_lifetime draft_id req = do
    let token' = E.decodeUtf8 <$> takeToken req
    ca <- checkAuthor' hLogger pool token_lifetime token'
    case ca of
        Left bs -> return $ responseBadRequest bs
        Right author'_id -> do
                    result <- publicNewsOnDb' hLogger pool author'_id draft_id
                    case result of
                      Left bs -> return $ responseBadRequest bs
                      Right n -> return $ responseOk $ LBS.fromStrict $ BC.pack $ show n

draftsBlock :: Handle -> Pool Connection -> TokenLifeTime -> [BC.ByteString] -> Request -> IO Response 
draftsBlock hLogger pool token_lifetime pathElems req   
                                    | pathElemsC == 1 = sendDrafts hLogger pool token_lifetime req
                                    | pathElemsC == 2 = case readByteStringToInt $ last pathElems of
                                              Just n -> getDraftById hLogger pool token_lifetime n req--return $ responseOk "draft)by)id"
                                              Nothing -> case last pathElems of
                                                              --"create_draft" -> Draftshandle.createDraft hLogger req
                                                              "delete_draft" -> deleteDraft hLogger req
                                                              _ -> return $ responseBadRequest "bad request"
                                    | pathElemsC == 3 = case readByteStringToInt $ head $ tail pathElems of
                                              Nothing -> return $ responseBadRequest "bad request"
                                              Just n -> case last pathElems of
                                                              "update_draft" -> updateDraft hLogger pool token_lifetime n req
                                                              "public_news" -> publicNews hLogger pool token_lifetime n req
                                                              _ -> return $ responseBadRequest "bad request"
                                           
                                    | otherwise = return $ responseBadRequest "bad request"
        where pathElemsC = length pathElems

  