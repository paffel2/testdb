{-# LANGUAGE OverloadedStrings #-}
module NewsAndCommentshandle where
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
import Text.Read
import Control.Applicative
import Responses
import HelpFunction
import Logger
import Databaseoperations
import FromRequest


newsMethodBlock :: Handle  -> [BC.ByteString] -> Request -> IO Response
newsMethodBlock hLogger pathElems req
    | pathElemC == 1 = do
        result <- sendNews hLogger req
        case result of
          Left bs ->  return $ responseBadRequest bs
          Right na -> return $ responseOk $ encode na
    | pathElemC == 2 = do
        let newsId = readMaybe $ BC.unpack $ last pathElems :: Maybe Int
        result <- sendNewsById hLogger req newsId
        case result of
            Left bs -> return $ responseBadRequest bs
            Right na -> return $ responseOk $ encode na
    | pathElemC == 3 = do
        let newsId = readMaybe $ BC.unpack $ head $ tail pathElems :: Maybe Int
        if last pathElems == "comments"
                  then do
                      result <- sendCommentsByNewsId hLogger req newsId
                      case result of
                        Left bs -> return $ responseBadRequest bs
                        Right ca -> return $ responseOk $ encode ca
                  else  do
                        logError hLogger "Bad url"
                        return $ responseBadRequest "Bad url"
    | pathElemC == 4 =
        case last pathElems of
            "add_comment" -> do
                let news'_id = readMaybe $ BC.unpack $ head $ tail pathElems :: Maybe Int
                result <- addCommentByNewsId hLogger req news'_id
                case result of
                    Left bs -> return $ responseBadRequest bs
                    Right bs -> return $ responseOk bs

            "delete_comment" -> do
                let news'_id = readMaybe $ BC.unpack $ head $ tail pathElems :: Maybe Int
                case news'_id of
                    Nothing -> do
                        logError hLogger "bad comment id"
                        return $ responseBadRequest "bad comment id"
                    --Just n -> do
                    Just _ -> do
                        result <- deleteCommentById hLogger req
                        case result of
                            Left bs ->  return $ responseBadRequest bs
                            Right bs ->  return $ responseOk bs
            _ -> do
                logError hLogger "Bad url"
                return $ responseBadRequest "bad request"
    | otherwise = do
        logError hLogger "Bad url"
        return $ responseBadRequest "Bad url"
    where
        pathElemC = length pathElems

sendNews :: Handle -> Request -> IO (Either LBS.ByteString NewsArray')
sendNews hLogger req = do
        case filterParamName of
            Just "tag_in" -> getNewsFilterByTagInFromDb hLogger filterParam pageParam
            Just "category" -> getNewsFilterByCategoryIdFromDb hLogger filterParam pageParam sortParam'
            Just "title" -> getNewsFilterByTitleFromDb hLogger filterParam pageParam sortParam'
            Just "author" -> getNewsFilterByAuthorNameFromDb hLogger filterParam pageParam sortParam'
            Just "date" -> getNewsFilterByDateFromDb hLogger filterParam pageParam sortParam'
            Just "tag_all" -> getNewsFilterByTagAllFromDb hLogger filterParam pageParam sortParam'
            Just "content" -> getNewsFilterByContentFromDb hLogger filterParam pageParam sortParam'
            Just "after_date" -> getNewsFilterByAfterDateFromDb hLogger filterParam pageParam sortParam'
            Just "before_date" -> getNewsFilterByBeforeDateFromDb hLogger filterParam pageParam sortParam'
            Just "tag" -> getNewsFilterByTagIdFromDb hLogger fstParam pageParam sortParam'
            Just _ -> do
                logError hLogger "Bad request"
                return $ Left "Bad request"
            Nothing -> getNewsFromDb' hLogger sortParam' pageParam

            --Nothing -> getNewsFromDb hLogger sortParam' pageParam


    where
        queryParams = queryString req
        --fstQueryParam@(fstParamName,fstParam) = head queryParams
        (_,fstParam) = head queryParams
        pageParam = fromMaybe Nothing (lookup "page" queryParams)
        sortParam = fromMaybe Nothing (lookup "sort" queryParams)
        filterParamName = myLookup "tag_in" queryParams <|> myLookup "category" queryParams <|> myLookup "tag" queryParams
                      <|> myLookup "tag_all" queryParams <|> myLookup "author" queryParams
                      <|> myLookup "title" queryParams <|> myLookup "content" queryParams
                      <|> myLookup "date" queryParams <|> myLookup "after_date" queryParams
                      <|> myLookup "before_date" queryParams

        filterParam = fromMaybe Nothing (lookup (fromMaybe "" filterParamName) queryParams)
        sortParam' = case sortParam of
                        Nothing -> ""
                        Just "author_name" -> "author_name"
                        Just "date_creation" -> "date_creation"
                        Just "category_name" -> "category_name"
                        Just _ -> ""


sendNewsById :: Handle -> Request -> Maybe Int -> IO (Either LBS.ByteString NewsArray')
sendNewsById hLogger req newsId = do
    let queryParams = rawQueryString req
    if queryParams == "" then
            getNewsByIdFromDb hLogger newsId
        else
            return $ Left "unexpected params"


sendCommentsByNewsId :: Handle -> Request -> Maybe Int -> IO (Either LBS.ByteString CommentArray)
sendCommentsByNewsId hLogger req news'_id = do
    let pageParam = fromMaybe Nothing (lookup "page" $ queryString req)
    getCommentsByNewsIdFromDb hLogger news'_id pageParam




addCommentByNewsId :: Handle -> Request -> Maybe Int -> IO (Either LBS.ByteString LBS.ByteString)
addCommentByNewsId hLogger req news'_id = do
    (i,_) <- parseRequestBodyEx noLimitParseRequestBodyOptions lbsBackEnd req
    let token' = fromMaybe Nothing (lookup "token" $ queryString req)
    let comment = lookup "comment_text" i
    case comment of
        Nothing -> do
            logError hLogger "Someone try add comment without comment parametr"
            return $ Left "No comment field"
        --Just bs' -> do
        Just _ -> do
                addCommentToDb hLogger (E.decodeUtf8 $ fromMaybe "" token') news'_id (E.decodeUtf8 $ fromMaybe "" comment)



deleteCommentById :: Handle -> Request -> IO (Either LBS.ByteString LBS.ByteString)
deleteCommentById hLogger req  = do
    let token' = takeToken req
    --(i,_) <- parseRequestBodyEx noLimitParseRequestBodyOptions lbsBackEnd req
    let comment_id = fromMaybe Nothing (lookup "comment_id" $ queryString req)
    let c_id' = read . BC.unpack <$> comment_id :: Maybe Int
    deleteCommentFromDb hLogger (E.decodeUtf8 $ fromMaybe "" token') c_id'
   