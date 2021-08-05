
module Server where
{-import Network.Wai
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
import NewsAndComments
import Categories
import Users
import Drafts-}


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
    run 8000 (simpleApp tst)


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



{-responseOk, responseNotFound, responseBadRequest :: LBS.ByteString -> Response

responseOk = responsePlainText status200
responseNotFound = responsePlainText notFound404
responseBadRequest = responsePlainText badRequest400

responsePlainText :: Status -> LBS.ByteString -> Response
responsePlainText = (`responseLBS` [(hContentType, "text/plain")])-}


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

    | path == "getNews" = 
        --print path
        encodeAllNews >>= respond


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

    | path == "new_draft" =
        new_draft >>= respond
    | path == "new_comment" = 
        newComment req >>= respond

    | otherwise = 
        respond $ responseOk "wrong url"

    where
        queryB' = rawQueryString req -- BC.ByteString
        queryQ = queryString req
        q = queryToQueryText queryQ
        path = BC.tail $ rawPathInfo req
        pathParams = BC.split '/' path
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
                return $ responseOk "new user register"
        new_draft = do
            (i,f) <- parseRequestBodyEx noLimitParseRequestBodyOptions lbsBackEnd req
            let main_image = foundParametr "main_image" f
            let images = foundParametr "images" f
            let main_image_triple = if fileContent (Prelude.head main_image) == "" then ("","","")
                                                                                   else (BC.unpack $ fileName $ Prelude.head main_image, BC.unpack $ fileContentType $ Prelude.head main_image, fileContent $ Prelude.head main_image  )
            
            let images_list = if fileContent (Prelude.head images) == "" then []
                                                                             else toTriple images

            let conT = any (/= "image") (take 5 . sndTriple <$> images_list)
            if ((Prelude.take 5  (sndTriple main_image_triple) /= "image") && main_image_triple /= ("","","")) || (images_list /= [] && conT)
                then return $ responseOk "bad image"
                else do
                    let login = E.decodeUtf8 $ fromMaybe "" (lookup "login" i)
                    l <- checkAuthor login
                    case l of
                        Nothing -> return $ responseOk "author not founded"
                        Just x -> do
                            let cat = fromMaybe "" (lookup "category" i)
                            let catT = T.toLower $ E.decodeUtf8 cat
                            TIO.putStrLn catT                    
                            c <- checkCategory catT
                            case c of
                                Nothing -> return $ responseOk "Category not founded"
                                Just cn -> do
                                    let tags = T.toLower $ E.decodeUtf8 $ fromMaybe "" (lookup "tags" i)
                                    TIO.putStrLn tags
                                    tags_ids <- checkTag $ splitOnPunctuationMark tags
                                    case tags_ids of
                                        Left mess -> return $ responseOk mess
                                        Right tl -> do
                                                    let text = E.decodeUtf8 $ fromMaybe "" (lookup "news_text" i)
                                                    let sh_title = E.decodeUtf8 $ fromMaybe "" (lookup "short_title" i)
                                                    --print i
                                                    TIO.putStrLn sh_title
                                                    if T.length sh_title > 20 then
                                                        return $ responseBadRequest "too long title"
                                                        else do
                                                        result <- createDraft main_image_triple images_list x cn tl text sh_title
                                                        return $ responseOk result

        toTriple (x:xs) = (BC.unpack $ fileName x, BC.unpack $ fileContentType x, fileContent x) :  toTriple xs
        toTriple [] = []
        fstTriple (a,b,c) = a
        sndTriple (a,b,c) = b
        thrdTriple (a,b,c) = c
        foundParametr :: B.ByteString -> [(B.ByteString, Network.Wai.Parse.FileInfo c)] -> [Network.Wai.Parse.FileInfo c]
        foundParametr param ((p,c):xs) = if p == param then  
                                                c : foundParametr param xs
                                                else foundParametr param xs
        foundParametr _ [] = []

 


                    



        


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

tagsString :: T.Text 
tagsString = "sport news"

{-splitOnPunctuationMark :: T.Text -> [T.Text]
splitOnPunctuationMark  = T.splitOn " " -}



newComment :: Request -> IO Response 
newComment req = do
    (i,f) <- parseRequestBodyEx noLimitParseRequestBodyOptions lbsBackEnd req
    let news_id' = readMaybe $ BC.unpack $ fromMaybe "" (lookup "news_id" i) :: Maybe Int
    case news_id' of
      Nothing -> return $ responseOk "bad news_id"
      Just n ->  do
                    print i
                    --print n
                    chN <- checkNews n
                    if not chN then
                            return $ responseOk "news not exist"
                           else
                            do
                                let login = E.decodeUtf8 $ fromMaybe "" (lookup "login" i)
                                chU <- checkUser login
                                case chU of
                                  Nothing -> return $ responseOk "user not exist"
                                  Just user_id' -> do
                                        let comment = E.decodeUtf8 $ fromMaybe "" (lookup "comment_text" i)
                                        createComment user_id' n comment
                                        return $ responseOk "comment added"-}






{-notMainServ :: IO ()
notMainServ = do
    putStrLn "Serving..."
    runSettings defaultSettings  appPath

appPath :: Application 
appPath req respond
    | requestMethod req `notElem` [methodGet, methodPost] =
      respond $ responseBadRequest "Only GET and POST methods is allowed!"

    | path == "findNewsByTitle" =
                respond $ responseOk "ok"
    | otherwise =  do
        --print pathParams
        --BC.putStrLn $ BC.concat pathParams
        respond $ responseOk "some work!"
        where
            path = BC.tail $ rawPathInfo req
            pathParams = BC.split '/' path-}


{------------------------------------------------------------------------------------------------------------

notMainServ :: IO ()
notMainServ = do
    putStrLn "Serving..."
    runSettings  (setMaximumBodyFlush (Just 1048576) $ setPort 8000 defaultSettings) appPath'

{-appPath' :: Application
appPath' req respond 
    | pathElemC == 1 = do
        print path
        print $ rawQueryString req
        respond $ responseOk "send all news"
    | pathElemC == 2 = 
        respond $ responseOk "send one news"
    | pathElemC == 3 = 
        respond $ responseOk "send comments"
    | pathElemC == 4 =
        respond $ responseOk "add comment"
    | otherwise = 
        respond $ responseBadRequest "bad url"
    where
        path = BC.tail $ rawPathInfo req
        pathElems = BC.split '/' path
        pathElemC = length pathElems
        pathHead = head pathElems-}


appPath' :: Application
appPath' req respond 
    | pathHead == "news" = newsMethodBlock path pathElems req >>= respond
    | pathHead == "login" = login req >>= respond
    | pathHead == "registration" = registration req >>= respond
    | pathHead == "categories" = categoriesBlock path pathElems req >>= respond
    | pathHead == "deleteUser" = deleteUser req >>= respond
    | pathHead == "drafts" = draftsBlock pathElems req >>= respond
    -- pathHead == "new_draft" = createDraft' req >>= respond
    | otherwise = 
        respond $ responseBadRequest "bad url"
    where
        path = BC.tail $ rawPathInfo req
        pathElems = BC.split '/' path
        pathElemC = length pathElems
        pathHead = head pathElems
-------------------------------------------------------------------------------------------}

{-newsMethodBlock :: BC.ByteString -> [BC.ByteString] -> Request ->  IO Response
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
        (False, bs) -> return $ Left bs-}

{-myLookup :: Eq a => a -> [(a, b)] -> Maybe a
myLookup _key []          =  Nothing
myLookup  key ((x,_):xys)
    | key == x           =  Just key
    | otherwise         =  myLookup key xys-}


{-login :: Request -> IO Response
login req = do
    (i,_) <- parseRequestBody lbsBackEnd req
    let log = fromMaybe "" (lookup "login" i)
    let pass = fromMaybe "" (lookup "user_password" i)
    check <- auth log pass
    case check of
      Left bs -> return $ responseBadRequest bs
      Right bs -> return $ responseOk bs

registration :: Request -> IO Response 
registration req = do
    (i,f) <- parseRequestBody lbsBackEnd req
    let [(_,file)] = f
    let file_contentType = fileContentType file
    --TIO.putStrLn $ E.decodeUtf8 $ fileName file
    if BC.take 5 file_contentType /= "image"
        then return $ responseBadRequest "Bad avatar file  "
        else do
            let f_name = E.decodeUtf8 $ fromMaybe "" (lookup "f_name" i)
            let l_name = E.decodeUtf8 $ fromMaybe "" (lookup "l_name" i)
            let login = fromMaybe "" (lookup "login" i)
            let password = fromMaybe "" (lookup "password" i)
            if T.length f_name > 50 || T.length l_name > 50 || T.length (E.decodeUtf8 login) > 50 || T.length (E.decodeUtf8 password) > 50
                then return $ responseBadRequest "one parametr more then 50 symbols"
                else do
                    result <- createUser'' login password f_name l_name (BC.unpack $ fileName file) (BC.unpack $ fileContentType file) (fileContent file)
                    case result of
                      Left bs -> return $ responseBadRequest bs
                      Right bs -> return $ responseOk bs-}

{-sendCategoriesList :: Request -> IO Response 
sendCategoriesList req = do
    result <- getCategoriesList pageParam
    return $ responseOk $ encode result
    where 
        queryParams = queryString req
        pageParam = fromMaybe Nothing (lookup "page" queryParams)

createCategory' :: Request -> IO Response
createCategory' req = do
    let token = fromMaybe Nothing (lookup "token" $ queryString req)
    ct <- checkAdmin $ E.decodeUtf8 $ fromMaybe "" token
    case ct of 
        (False,bs) -> return $ responseBadRequest bs
        (True,_) -> do
                (i,_) <- parseRequestBody lbsBackEnd req
                let category_name = lookup "category_name" i
                case category_name of
                    Nothing -> return $ responseBadRequest "Bad category name"
                    Just bs -> do
                        ch <- checkCategory $ E.decodeUtf8 bs
                        case ch of
                            Just n -> return $ responseBadRequest "Category already exist"
                            Nothing -> do
                                    let maternal_category_name = lookup "maternal_category_name" i
                                    case maternal_category_name of
                                        Nothing -> do
                                            result <- createCategory (E.decodeUtf8 bs) Nothing
                                            return $ responseOk result

                                        Just bs' -> do
                                            chm <- checkCategory $ E.decodeUtf8 bs'
                                            case chm of
                                              Nothing -> return $ responseBadRequest "Maternal category not exist"
                                              Just n -> do
                                                  result <- createCategory (E.decodeUtf8 bs) (Just n)
                                                  return $ responseOk result
deleteCategory' :: Request -> IO Response 
deleteCategory' req = do
    let token = fromMaybe Nothing (lookup "token" $ queryString req)
    ct <- checkAdmin $ E.decodeUtf8 $ fromMaybe "" token
    case ct of 
        (False,bs) -> return $ responseBadRequest bs
        (True,_) -> do
            (i,_) <- parseRequestBody lbsBackEnd req
            let category_name = lookup "category_name" i
            case category_name of
              Nothing -> return $ responseBadRequest "no categoty_name field"
              Just bs -> do
                  result <- deleteCategory $ E.decodeUtf8 bs
                  return $ responseOk result

editCategory' :: Request -> IO Response
editCategory' req = do
    let token = fromMaybe Nothing (lookup "token" $ queryString req)
    ct <- checkAdmin $ E.decodeUtf8 $ fromMaybe "" token
    case ct of 
        (False,bs) -> return $ responseBadRequest bs
        (True,_) -> do
                    let category_name_parametr = lookup "category_name" $ queryString req
    --print category_name_parametr
                    case category_name_parametr of
                        Nothing -> return $ responseBadRequest "no category name parametr"
                        Just m_bs -> do
                            let category_name = fromMaybe "" m_bs
                            cc <- checkCategory $ E.decodeUtf8 category_name
                            case cc of
                                Nothing -> return $ responseBadRequest "category not exist"
                                _ -> do
                                    let new_maternal_parametr = lookup "new_maternal" $ queryString req
                                    let new_name_parametr = lookup "new_name" $ queryString req
                                    case (new_maternal_parametr,new_name_parametr) of 
                                        (Nothing, Nothing) -> return $ responseBadRequest "no editable parametrs"
                                        (Nothing, Just nnp) -> do 
                                                    result <- editCategoryName category_name nnp
                                                    case result of
                                                            Left bs -> return $ responseBadRequest bs
                                                            Right bs -> return $ responseOk bs

                                        (Just nmp,Nothing) -> do 
                                                    result <-editCategoryMaternal category_name nmp
                                                    case result of
                                                            Left bs -> return $ responseBadRequest bs
                                                            Right bs -> return $ responseOk bs

                                        (Just nmp,Just nnp) -> do
                                                    result <-fullEditCategory category_name nnp nmp
                                                    case result of
                                                            Left bs -> return $ responseBadRequest bs
                                                            Right bs -> return $ responseOk bs


            

categoriesBlock :: BC.ByteString -> [BC.ByteString] -> Request -> IO Response 
categoriesBlock path pathElems req | pathElemsC == 1 = sendCategoriesList req
                                   | pathElemsC == 2 = 
                                       case last pathElems of
                                           "delete_category" -> deleteCategory' req
                                           "create_category" -> createCategory' req
                                           "edit_category" -> editCategory' req
                                           _ -> return $ responseBadRequest "bad request"
                                   | otherwise = return $ responseBadRequest "bad request"


    where pathElemsC = length pathElems-}






{-(i, f) <- parseRequestBody lbsBackEnd req
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
                return $ responseOk "new user register"-}
