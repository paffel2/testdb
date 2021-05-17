{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
module Server where
import Network.Wai
import Network.Wai.Handler.Warp
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

{-simpleApp :: Application 
simpleApp _ respond = respond $
    responseLBS status200 
                [(hContentType, "text/plain")]
                (encode tst)

simpleServer :: IO ()
simpleServer = do
    putStrLn "Serving..."
    run 8000 simpleApp-}

{-simpleApp :: ToJSON a => a -> Application 
simpleApp jsonInfo _ respond = respond $
    responseLBS status200
     []
     (encode jsonInfo)

simpleServer :: IO ()
simpleServer = do
    putStrLn "Serving..."
    run 8000 (simpleApp tst)-}


simpleApp :: ToJSON a => a -> Application
simpleApp jsonInfo _ respond = respond $
    responseLBS status200
     []
     (encode jsonInfo)

simpleServer :: IO ()
simpleServer = do
    putStrLn "Serving..."
    news <- getNews
    run 8000 (simpleApp news)


data SimpleJson = SimpleJson { name :: String,
                               age :: Int
                               } deriving (Show, Generic)
instance ToJSON SimpleJson where
    toJSON = genericToJSON defaultOptions


tst :: SimpleJson
tst = SimpleJson "Paul" 26



responseOk, responseNotFound, responseBadRequest :: LBS.ByteString -> Response

responseOk = responsePlainText status200
responseNotFound = responsePlainText notFound404
responseBadRequest = responsePlainText badRequest400

responsePlainText :: Status -> LBS.ByteString -> Response
responsePlainText = (`responseLBS` [(hContentType, "text/plain")])


{-app :: Application 
app req respond = respond $
    if (requestMethod req /= methodGet) || (requestMethod req /= methodPost)
        then responseBadRequest "Only GET and POST methods is allowed!"
        else
            responseOk "Working"-}

servMain :: IO ()
servMain = do
    putStrLn "Serving..."
    run 8000 app


app :: Application
app req respond
    | requestMethod req `notElem` [methodGet, methodPost] =
      respond $ responseBadRequest "Only GET and POST methods is allowed!"

    | path == "findNewsByTitle" =
                if queryB' == ""
                then respond $  responseBadRequest "No query parameters needed!"
                else encodeParamsFindByTitle' >>= respond

    | path == "getNews" = encodeAllNews >>= respond


    | path == "" =
        respond $
            if queryB' /= ""
            then responseBadRequest "No query parameters needed!"
            else responseOk "some work"
    | otherwise =
      respond $ responseOk ""

    where
        queryB' = rawQueryString req -- BC.ByteString
        queryQ = queryString req
        q = queryToQueryText queryQ
        path = BC.tail $ rawPathInfo req
        encodeParamsFindByTitle = do
                            --let title = fst qT
                            --let title_param = case snd qT of
                            --            Just x -> x
                            --            _ -> ""
                            --let page = fst qP
                            --let titleParams = 
                            --let page_param = case snd qP of
                            --            Just x -> read x :: Int
                            --            _ -> 1
                            let (title, page) = fnbtParams q
                            TIO.putStrLn title
                            TIO.putStrLn page
                            case title of
                                 "" -> return $ responseOk  "wrong argument"
                                                    -- do news <- findNewsByTitle title_param
                                                    --let (p:ps) = pathInfo req
                                                    --TIO.putStrLn p
                                                    --let n = length (pathInfo req)
                                                    --print n
                                                    --print $ length q
                                                    --TIO.putStrLn (fst qT)
                                                    --return $ responseOk $ encode news
                                 t -> do
                                     news <- findNewsByTitle' page t
                                     return $ responseOk $ encode news
        encodeParamsFindByTitle' = do
            catch encodeParamsFindByTitle $ \e -> do let err = show (e :: IOException)
                                                     Prelude.putStrLn err
                                                     return $ responseOk "какой то бред"
        encodeAllNews = do
            responseOk . encode <$> getNews


{-app :: Application
app req respond
    | requestMethod req `notElem` [methodGet, methodPost] =
      respond $ responseBadRequest "Only GET and POST methods is allowed!"

    | path == "findNewsByTitle" =
                if queryB' == ""
                then respond $  responseBadRequest "No query parameters needed!"
                else encodeJson' >>= respond

    | path == "getNews" = encodeAllNews >>= respond


    | path == "" =
        respond $
            if queryB' /= ""
            then responseBadRequest "No query parameters needed!"
            else responseOk "some work"
    | otherwise =
      respond $ responseOk ""

    where
        queryB' = rawQueryString req -- BC.ByteString
        queryQ = queryString req
        q@(qT:qtS) = queryToQueryText queryQ
        path = BC.tail $ rawPathInfo req
        encodeJson = do
            let a = fst qT
            let b = case snd qT of
                        Just x -> x
                        _ -> "ничего"
            news <- findNewsByTitle a
            let (p:ps) = pathInfo req
            TIO.putStrLn p
            let n = length (pathInfo req)
            print n
            print $ length q
            --TIO.putStrLn b
            TIO.putStrLn (fst qT)
            return $ responseOk $ encode news
        encodeJson' = do
            catch encodeJson $ \e -> do let err = show (e :: IOException)
                                        Prelude.putStrLn err
                                        return $ responseOk "какой то бред"
        encodeAllNews = do
            responseOk . encode <$> getNews-}

rusText :: T.Text 
rusText = "Катастрофа"

printRusText :: IO ()
printRusText = TIO.putStrLn rusText


params :: [(T.Text, Maybe T.Text)]
params = [("title", Just "s"),("page", Just "1")]

--readTextToInt :: T.Text -> Int



{-textTtext :: T.Text 
textTtext = "123"


readTextToInt :: T.Text -> IO Int
readTextToInt text = catch (readIO $ T.unpack text) $ \e -> do
   let err = displayException (e :: IOException)
   print "not int"
   return 0-}

fnbtParams :: [(T.Text, Maybe T.Text)] -> (T.Text,T.Text)
fnbtParams s = (t,p) where
    t = case findTitle of
        Just (Just x) -> x
        _ -> ""
    p = case findPage of
        Just (Just x) -> x
        _ -> "1"
    findTitle = lookup "title" s
    findPage = lookup "page" s

{-    where
        queryB' = rawQueryString req -- BC.ByteString
        queryQ = queryString req
        q@(qT:qP:qtS) = queryToQueryText queryQ
        path = BC.tail $ rawPathInfo req
        encodeParamsFindByTitle = do
                            --let title = fst qT
                            --let title_param = case snd qT of
                                        --Just x -> x
                                       -- _ -> ""
                            --let page = fst qP
                            --let titleParams = 
                            --let page_param = case snd qP of
                            --            Just x -> read x :: Int
                            --            _ -> 1
                            let (title,page) = fnbtParams q
                            case (title,page) of
                                ("",_) -> return $ responseOk  "wrong argument"
                                                    --news <- findNewsByTitle' t p
                                                    --let (p:ps) = pathInfo req
                                                    --TIO.putStrLn p
                                                    --let n = length (pathInfo req)
                                                    --print n
                                                    --print $ length q
                                                    --TIO.putStrLn (fst qT)

                                (t,p) -> do
                                            news <- findNewsByTitle' t p
                                            return $ responseOk $ encode news-}