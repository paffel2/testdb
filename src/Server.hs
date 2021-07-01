{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
module Server where
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

{-servMain :: IO ()
servMain = do
    putStrLn "Serving..."
    run 8000 app-}

servMain :: IO ()
servMain = do
    putStrLn "Serving..."
    runSettings  (setMaximumBodyFlush (Just 1048576) $ setPort 8000 defaultSettings) app

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
    
    | path == "upload_image" = 
        upload_image >>= respond

    | path == "new_author" = 
        if queryB' == ""
                then respond $  responseBadRequest "No query parameters needed!"
        else newAuthor >>= respond
    | path == "login" = login >>= respond

    | path == "registration" =
        registration >>= respond

    | otherwise =
      respond $ responseOk ""

    where
        queryB' = rawQueryString req -- BC.ByteString
        queryQ = queryString req
        q = queryToQueryText queryQ
        path = BC.tail $ rawPathInfo req
        encodeParamsFindByTitle = do
                            let (title, page) = fnbtParams q
                            TIO.putStrLn title
                            TIO.putStrLn page
                            case title of
                                 "" -> return $ responseOk  "wrong argument"
                                 t -> do
                                     news <- findNewsByTitle' page t
                                     return $ responseOk $ encode news
        encodeParamsFindByTitle' = do
            catch encodeParamsFindByTitle $ \e -> do let err = show (e :: IOException)
                                                     Prelude.putStrLn err
                                                     return $ responseOk "какой то бред"
        encodeAllNews = do
            responseOk . encode <$> getNews
        upload_image = do
            --print "1"
            --a <- getRequestBodyChunk req
            --print a
            (_, file) <- parseRequestBody lbsBackEnd req
            let [(_,info)] = file
            print $ fileName info
            print $ fileContentType info
            let filePath = BC.unpack (fileName info)
            let contentType  = BC.unpack (fileContentType info)
            LBS.writeFile filePath (fileContent info)
            x <- loadImage filePath contentType (fileContent info)
            print x
            return $ responseOk "File Dl"
        login = do
            (i,f) <- parseRequestBody lbsBackEnd req
            let log = BC.unpack $ fromMaybe "" (lookup "login" i)
            let pass = BC.unpack $ fromMaybe "" (lookup "user_password" i)
            check <- auth' log pass
            if check then do
                print ("user " ++ log ++ " logged")
                return $ responseOk "logged"
                else do
                    print ("someone try login with login " ++ log)
                    return $ responseOk "not logged"
        newAuthor = do
            let login' = authorParams q
            case login' of
                "" -> return $ responseOk  "wrong argument"
                l -> do
                    userId <- findUser l
                    case userId of
                        Nothing -> return $ responseOk  "no user"
                        Just x -> do 
                            (i,_) <- parseRequestBody lbsBackEnd req
                            let desc = T.take 50 $ E.decodeUtf8 $ fromMaybe "" (lookup "description" i)
                            print $ T.length desc
                            createAuthor x desc
                            return $ responseOk  "new author created"
        registration = do
            (i, f) <- parseRequestBody lbsBackEnd req
            print i
            let [(_,file)] = f
            print $ fileName file
            print $ fileContentType file
            if BC.take 5 (fileContentType file) /= "image" then
                return $ responseOk "bad avatar"
            else do
                let f_name = T.take 50 $ E.decodeUtf8 $ fromMaybe "" (lookup "f_name" i)
                let l_name = T.take 50 $ E.decodeUtf8 $ fromMaybe "" (lookup "l_name" i)
                let login = T.take 50 $ E.decodeUtf8 $ fromMaybe "" (lookup "login" i)
                let password = T.take 50 $ E.decodeUtf8 $ fromMaybe "" (lookup "password" i)
                createUser' login password f_name l_name (BC.unpack $ fileName file) (BC.unpack $ fileContentType file) (fileContent file)
                return $ responseOk "test"


                    



        


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

authorParams :: [(T.Text, Maybe T.Text)] -> T.Text
authorParams s = l where
    l = case findLogin of
        Just (Just x) -> x
        _ -> ""
    findLogin = lookup "login" s


