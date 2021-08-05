
module NewsAndComments where
{-import Network.Wai
--import Network.Wai.Handler.Warp
import Network.Wai.Parse
--import Network.HTTP.Types
import GHC.Generics
import Data.Aeson
import Testdatabase
--import Control.Exception
--import qualified Data.Text.Lazy.Encoding as EL
--import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.Encoding as E
--import qualified Data.Text as T
import qualified Data.Text.IO as TIO
--import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BC
import Types
import Data.Maybe
import Text.Read
import Control.Applicative
import Responses
import HelpFunction

newsMethodBlock :: BC.ByteString -> [BC.ByteString] -> Request ->  IO Response
newsMethodBlock path pathElems req     
    | pathElemC == 1 = do
        result <- sendNews req
        case result of
          Left bs ->  return $ responseBadRequest bs
          Right na -> return $ responseOk $ encode na
    | pathElemC == 2 = do
        let newsId = readMaybe $ BC.unpack $ last pathElems :: Maybe Int
        case newsId of
          Nothing -> return $ responseBadRequest "bad news id"
          Just n -> do
              result <- sendNewsById req n
              case result of
                Left bs -> return $ responseBadRequest bs
                Right na ->  return $ responseOk $ encode na
    | pathElemC == 3 = do
        let newsId = readMaybe $ BC.unpack $ head $ tail pathElems :: Maybe Int
        case newsId of
          Nothing -> return $ responseBadRequest "bad news id"
          Just n -> 
              if last pathElems == "comments"
                  then do
                      result <- sendCommentsByNewsId req n
                      case result of
                        Left bs -> return $ responseBadRequest "bad news id"
                        Right ca -> return $ responseOk $ encode ca

                  else
                        return $ responseBadRequest "Not founded"
    | pathElemC == 4 =
        case last pathElems of
            "add_comment" -> do
                let news_id = readMaybe $ BC.unpack $ head $ tail pathElems :: Maybe Int
                case news_id of
                  Nothing -> return $ responseBadRequest "bad news id"
                  Just n -> do
                            result <- addCommentByNewsId req n
                            case result of
                              Left bs -> return $ responseBadRequest bs
                              Right bs -> return $ responseOk bs

            "delete_comment" -> do
                let news_id = readMaybe $ BC.unpack $ head $ tail pathElems :: Maybe Int
                case news_id of
                  Nothing -> return $ responseBadRequest "bad news id"
                  Just n -> do
                    result <- deleteCommentById req
                    case result of
                      Left bs ->  return $ responseBadRequest bs
                      Right bs ->  return $ responseOk bs
            _ -> return $ responseBadRequest "bad request1"
        --return $ responseOk "add comment"
    | otherwise = 
        return $ responseBadRequest "bad request2"
    where
        pathElemC = length pathElems

sendNews :: Request  -> IO (Either LBS.ByteString NewsArray')
sendNews req = do
        print filterParamName
        let p = fromMaybe "no param" filterParam
        TIO.putStrLn $ E.decodeUtf8 p
        case filterParamName of 
            Just "tag_in" -> getNewsFilterByTagIn filterParam pageParam
            Just "category" -> getNewsFilterByCategoryId filterParam pageParam sortParam'
            Just "title" -> getNewsFilterByTitle filterParam pageParam sortParam'
            Just "author" -> getNewsFilterByAuthorName filterParam pageParam sortParam'
            Just "date" -> getNewsFilterByDate filterParam pageParam sortParam'
            Just "tag_all" -> getNewsFilterByTagAll filterParam pageParam sortParam'
            Just "content" -> getNewsFilterByContent filterParam pageParam sortParam'
            Just "after_date" -> getNewsFilterByAfterDate filterParam pageParam sortParam'
            Just "before_date" -> getNewsFilterByBeforeDate filterParam pageParam sortParam'
            Just "tag" -> getNewsFilterByTagId fstParam pageParam sortParam'
            Just _ -> return $ Left  "Bad request3"
            Nothing -> Right <$> getNews' sortParam' pageParam


    where 
        queryParams = queryString req
        fstQueryParam@(fstParamName,fstParam) = head queryParams
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

sendNewsById :: Request -> Int -> IO (Either LBS.ByteString NewsArray')
sendNewsById req nid = do
    let queryParams = rawQueryString req
    if queryParams == "" then
            Right <$> getNewsById nid
        else
            return $ Left "unexpected params"



  


sendCommentsByNewsId :: Request -> Int -> IO (Either LBS.ByteString CommentArray)
sendCommentsByNewsId req news_id = do
    let pageParam = fromMaybe Nothing (lookup "page" $ queryString req)
    getCommentsByNewsId news_id pageParam


{-addCommentByNewsId' :: Request -> Int -> IO (Either LBS.ByteString LBS.ByteString)
addCommentByNewsId' req news_id = do
    (i,_) <- parseRequestBodyEx noLimitParseRequestBodyOptions lbsBackEnd req
    let login = lookup "login" i
    case login of
      Nothing -> return $ Left "No login field"
      Just bs -> do
            let comment = lookup "comment_text" i
            case comment of
              Nothing -> return $ Left "No login field"
              Just bs' -> do
                addComment (E.decodeUtf8 $ fromMaybe "" login) news_id (E.decodeUtf8 $ fromMaybe "" comment)-}



addCommentByNewsId :: Request -> Int -> IO (Either LBS.ByteString LBS.ByteString)
addCommentByNewsId req news_id = do
    (i,_) <- parseRequestBodyEx noLimitParseRequestBodyOptions lbsBackEnd req
    let token = fromMaybe Nothing (lookup "token" $ queryString req)
    --let c = E.decodeUtf8 $ fromMaybe "" token
    --print c
    let comment = lookup "comment_text" i
    case comment of
        Nothing -> return $ Left "No comment field"
        Just bs' -> do
                addComment (E.decodeUtf8 $ fromMaybe "" token) news_id (E.decodeUtf8 $ fromMaybe "" comment)

{-deleteCommentById :: Request ->  IO (Either LBS.ByteString LBS.ByteString)
deleteCommentById req  = do
    (i,_) <- parseRequestBodyEx noLimitParseRequestBodyOptions lbsBackEnd req
    let comment_id = lookup "comment_id" i
    case comment_id of
      Nothing -> return $ Left "no comment_id field"
      Just bs -> deleteComment bs-}
            --return $ Right ""
    --addComment login news_id comment

deleteCommentById :: Request ->  IO (Either LBS.ByteString LBS.ByteString)
deleteCommentById req  = do
    let token = fromMaybe Nothing (lookup "token" $ queryString req)
    ct <- checkAdmin (E.decodeUtf8 $ fromMaybe "" token)
    case ct of
        (True,_) -> do
                (i,_) <- parseRequestBodyEx noLimitParseRequestBodyOptions lbsBackEnd req
                let comment_id = lookup "comment_id" i
                case comment_id of
                    Nothing -> return $ Left "no comment_id field"
                    Just bs -> deleteComment bs
        (False, bs) -> return $ Left bs
        -}