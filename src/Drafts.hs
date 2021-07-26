{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
module Drafts where
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Parse
import Network.HTTP.Types
import GHC.Generics
import Data.Aeson
import Testdatabase
import Control.Exception
import qualified Data.Text.Lazy.Encoding as EL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.Encoding as E
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BC
import Types
import Data.Maybe
import Text.Read
import Control.Applicative
import HelpFunction
import Responses
--import NewsAndComments
--import Categories
--import Users



sendDrafts :: Request -> IO Response
sendDrafts req = do
    let token = fromMaybe Nothing (lookup "token" $ queryString req)
    cauth <- checkAuthor' $ E.decodeUtf8 $ fromMaybe "" token
    case cauth of
      Nothing -> return $ responseBadRequest "Bad token"
      Just n -> do
            drafts <- getDraftsByAuthorId n
            return $ responseOk (encode drafts)

createDraft' :: Request -> IO Response
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
                                                                    Right n -> return $ responseOk $ LBS.fromStrict $ BC.pack $ show n

deleteDraft :: Request -> IO Response 
deleteDraft req = do
  let token = E.decodeUtf8 $ fromMaybe "" (fromMaybe Nothing (lookup "token" $ queryString req))
  ca <- checkAuthor' token
  case ca of
    Nothing -> return $ responseBadRequest "author not founded"
    Just author_id -> do
      let d_id = readByteStringToInt $ fromMaybe "" (fromMaybe Nothing (lookup "draft_id" $ queryString req))
      case d_id of
        Nothing -> return $ responseBadRequest "bad draft_id"
        Just draft_id -> do
          result <- deleteDraftFromDb author_id draft_id
          case result of
            Left bs -> return $ responseBadRequest bs
            Right bs -> return $ responseOk bs

getDraftById :: Int -> Request -> IO Response
getDraftById draft_id req = do
  let token = E.decodeUtf8 $ fromMaybe "" (fromMaybe Nothing (lookup "token" $ queryString req))
  ca <- checkAuthor' token
  case ca of
    Nothing -> return $ responseBadRequest "author not founded"
    Just author_id -> do
                    result <- getDraftByIdDromDb author_id draft_id
                    case result of
                      Left bs -> return $ responseBadRequest bs
                      Right draft -> return $ responseOk $ encode draft

updateDraft :: Int -> Request -> IO Response 
updateDraft draft_id req = do
  let token = E.decodeUtf8 $ fromMaybe "" (fromMaybe Nothing (lookup "token" $ queryString req))
  ca <- checkAuthor' token
  case ca of
    Nothing -> return $ responseBadRequest "author not founded"
    Just author_id -> do
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
                                                                  result <- updateDraftInDb main_image_triple images_list author_id cat_id tag_list text sh_title draft_id
                                                                  case result of
                                                                    Left bs -> return $ responseBadRequest bs
                                                                    Right bs -> return $ responseOk bs

publicNews :: Int -> Request -> IO Response
publicNews draft_id req = do
  let token = E.decodeUtf8 $ fromMaybe "" (fromMaybe Nothing (lookup "token" $ queryString req))
  ca <- checkAuthor' token
  case ca of
    Nothing -> return $ responseBadRequest "author not founded"
    Just author_id -> do
                    result <- publicNewsOnDb author_id draft_id
                    case result of
                      Left bs -> return $ responseBadRequest bs
                      Right bs -> return $ responseOk bs

draftsBlock :: [BC.ByteString] -> Request -> IO Response 
draftsBlock pathElems req | pathElemsC == 1 = sendDrafts req
                          | pathElemsC == 2 = case readByteStringToInt $ last pathElems of
                                              Just n -> getDraftById n req--return $ responseOk "draft)by)id"
                                              Nothing -> case last pathElems of
                                                              "create_draft" -> createDraft' req
                                                              "delete_draft" -> deleteDraft req
                                                              _ -> return $ responseBadRequest "bad request"
                          | pathElemsC == 3 = case readByteStringToInt $ head $ tail pathElems of
                                              Nothing -> return $ responseBadRequest "bad request"
                                              Just n -> case last pathElems of
                                                              "update_draft" -> updateDraft n req
                                                              "public_news" -> publicNews n req
                                                              _ -> return $ responseBadRequest "bad request"
                                           
                          | otherwise = return $ responseBadRequest "bad request"
        where pathElemsC = length pathElems

  
