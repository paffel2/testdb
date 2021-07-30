{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Testdatabase where
import Database.PostgreSQL.Simple
--import Database.PostgreSQL.Simple.FromField
--import Control.Monad
--import Control.Applicative
import Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Types
import Data.Time
import Control.Exception
--import Database.PostgreSQL.Simple.Internal
import qualified Data.Text.Encoding as E
import Data.Maybe
--import Text.Read
--import Data.String
import HelpFunction
import Database.PostgreSQL.Simple.Types
import Logger






hello :: IO Int
hello = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    --conn <- connect defaultConnectInfo
    [Only i] <- query_ conn "select count(user_id) from users group by user_id"
    rows <- query_ conn "select first_name, last_name from users" :: IO [[Maybe T.Text]]
    printLst rows
    close conn
    return i

b :: BC.ByteString
b = "Павел"


printMaybeText :: Maybe T.Text -> IO ()
printMaybeText (Just a) = TIO.putStr $ T.concat [a, " "]
printMaybeText Nothing = TIO.putStr ""


printMaybeList :: [Maybe T.Text] -> IO ()
printMaybeList (x:xs) = do
    printMaybeText x
    printMaybeList xs
printMaybeList [] = TIO.putStrLn ""


printLst :: [[Maybe T.Text]] -> IO ()
printLst (x:xs) = do
    printMaybeList x
    printLst xs
printLst [] = TIO.putStr ""


createUser :: IO ()
createUser = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    now <- getCurrentTime
    let newUser = User (Just "Павел") (Just "Зарубин") Nothing "dahaku" "123" now True
    execute conn "insert into users (first_name, last_name, avatar, login, user_password, creation_date, admin_mark) values (?,?,?,?,crypt(?,gen_salt('md5')),?,?)" newUser
    close conn
    Prelude.putStrLn "done"



{-auth :: String  -> String -> IO String
auth login password = do
    catch ( do
            conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
            [Only check] <- query conn "select (case check_pass  when TRUE then 'token' when FALSE then 'wrong password' else 'wrong login' end) from (select (user_password = crypt(?,user_password)) as check_pass from users where login = ?) ce" (password,login)
            close conn
            return check)
        $ \e -> do
                    let err = show (e :: IOException)
                    return "wrong login"-}


auth' :: String  -> String -> IO Bool
auth' login password = do
    catch ( do
            conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
            [Only check] <- query conn "select (user_password = crypt(?,user_password)) as check_pass from users where login = ?" (password,login)
            close conn
            return check)
        $ \e -> do
                    let err = show (e :: IOException)
                    Prelude.putStrLn "wrong login"
                    return False

auth :: BC.ByteString  -> BC.ByteString -> IO (Either LBS.ByteString LBS.ByteString)
auth login password = do
            ui <- checkUser' login password
            case ui of
              Nothing -> return $ Left "wrong login or password"
              Just n -> do
                    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                    (token,now) <- generateToken login
                    let q = "insert into tokens (user_id, token ,creation_date) values (?, ? ,?) ON CONFLICT(user_id) DO UPDATE set token = ?, creation_date= ?"
                    execute conn q (n,token,now,token,now)
                    close conn
                    return $ Right $ LBS.pack $ B.unpack token

generateToken :: BC.ByteString -> IO (BC.ByteString, UTCTime)
generateToken login = do
    now <- getCurrentTime
    let token = Prelude.filter (`Prelude.notElem` filt ) (show now)
    return (BC.concat [login,BC.pack token],now)
        where filt = " :.-UTC" :: String




            --[Only check] <- query conn "select (user_password = crypt(?,user_password)) as check_pass from users where login = ?" (password,login) :: IO [Only Bool]
            {-if check then do
                now <- getCurrentTime
                let token = BC.concat [login, BC.pack $ show now]
                execute conn "insert into tokens (user_id" [token]
                close conn
                return $ Right token
            else do
                close conn
                BC.putStrLn $ BC.concat["someone try login use login: ",login ," and password: ", password]
                return $ Left "wrong login or password"-}
 --"insert into tokens (user_id, token ,creation_date) (select user_id, concat(login,now()) as l,now() from users where user_id = ?) ON CONFLICT(user_id) DO UPDATE SET token = (select concat(login,now()) from users where user_id = ?), creation_date = now()"
checkUser' :: BC.ByteString -> BC.ByteString -> IO (Maybe Int)
checkUser' login password = do
    catch ( do 
            conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
            [Only user_id] <- query conn "select user_id from users where user_password = crypt(?,user_password) and login = ? " (password,login) :: IO [Only (Maybe Int)]
            close conn
            return user_id)
        $ \e -> do
            let err = show (e :: IOException)
            --print "wrong login"
            return Nothing

lg :: BC.ByteString
lg = "dahaku"
ps :: BC.ByteString 
ps = "qwerty"

ps' :: BC.ByteString 
ps' = "qwerty1"

lg' :: BC.ByteString
lg' = "dahaku22222222"
{-getBytesFromImages :: String -> IO ()
getBytesFromImages fp  = do
    file <- BC.readFile fp
    BC.writeFile "src/newfile.jpg" file

testBFI :: IO ()
testBFI = getBytesFromImages "src/avatar.jpg"-}

createAuthor :: Int -> T.Text -> IO ()
createAuthor usid desc= do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    let newAuthor = Author usid (Just desc)
    execute conn "insert into authors (author_user_id, description) values (?,?)" newAuthor
    close conn
    Prelude.putStrLn "author created"

createCategory :: T.Text -> Maybe Int -> IO LBS.ByteString 
createCategory categoryName maternalCategoryId = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    let newCategory = Category (T.toLower  categoryName) maternalCategoryId
    execute conn "insert into categories (category_name, maternal_category) values (?,?)" newCategory
    close conn
    return "Category created"

{-newCat :: IO ()
newCat = createCategory "Наука" Nothing-}

createNews :: String -> Int -> Int -> T.Text -> String -> IO ()
createNews shortTitle authorId categoryId newsText mainImage = do
    mImage <- BC.readFile mainImage
    now <- getCurrentTime
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    let newNews = News shortTitle now authorId categoryId newsText (Binary mImage)
    execute conn "insert into news (short_title, date_creation, author_id, category_id, news_text, main_image) values (?,?,?,?,?,?)" newNews
    close conn
    Prelude.putStrLn "done"

{-newNews :: IO ()
newNews = createNews "Достижение" 1 1 "Волки едят траву" "src/avatar.jpg" -}



downloadImages :: IO ()
downloadImages = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='arraays'"
    [Only (Binary i)] <- query_ conn "select image_b from images_data where image_id = 1"
    close conn
    BC.writeFile "src/dl.jpg" i
    Prelude.putStrLn "done"


getNews :: IO NewsArray
getNews = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='arraays'"
    rows <- query_ conn "select title, news_id from news order by 2 DESC" :: IO [GetNews]
    close conn
    return (NewsArray rows)



findNewsByTitle :: T.Text -> IO NewsArray
findNewsByTitle title = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='arraays'"
    rows <- query conn "select title, news_id from news where title ilike ? order by 2" $ Only $ In [T.concat ["%",title,"%"]] :: IO [GetNews]
    close conn
    return (NewsArray rows)


text :: T.Text
text = "ката"

printTitle :: T.Text -> IO ()
printTitle tst = do
    arr <- findNewsByTitle tst
    let (c:cs) = news arr
    TIO.putStrLn $ title c

readTextToInt :: T.Text -> IO Int
readTextToInt text = catch (readIO $ T.unpack text) $ \e -> do
   let err = displayException (e :: IOException)
   print "not int"
   return 1

findNewsByTitle' :: T.Text -> T.Text -> IO NewsArray
findNewsByTitle' page title = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='arraays'"
    pg <- readTextToInt page
    let fnews = FindNewsByTitle (T.concat ["%",title,"%"]) ((pg-1)*10)
    rows <- query conn "select title, news_id from news where title ilike ? order by 2 DESC limit 10 offset ? " fnews :: IO [GetNews]
    close conn
    return (NewsArray rows)

fnbt :: IO NewsArray
fnbt = findNewsByTitle' "1" "к"

page' :: T.Text
page' = "1"

title' :: T.Text
title' = "s"

{-testloginpassword :: [(String, String)]
testloginpassword = [("dahaku","qwerty"),
                     ("niamh","qwerty1"),
                     ("phir","qwerty2"),
                     ("josethai","qwerty3"),
                     ("mirosha","qwerty4"),
                     ("uman","qwerty5"),
                     ("ylan","qwerty6"),
                     ("ppoleg","qwerty7"),
                     ("cybelin","qwerty8"),
                     ("xieana","qwerty9"),
                     ("zantony","qwerty10"),
                     ("hawkaaa","qwerty11"),
                     ("lannalie","qwerty12"),
                     ("charl","qwerty13"),
                     ("cianal","qwerty14"),
                     ("helm","qwerty15"),
                     ("nahal","qwerty16"),
                     ("soneli","qwerty17"),
                     ("hell","qwerty18"),
                     ("tamal","qwerty19"),
                     ("taxian","qwerty20"),
                     ("xadr","qwerty21"),
                     ("alenal","qwerty22"),
                     ("dahaku","qwerty23"),
                     ("dahak","qwerty")]

authtest :: [(String, String)] -> IO ()
authtest (x:xs) = do
    a <- uncurry auth' x
    Prelude.putStrLn $ show a ++ " " ++ uncurry sumWithSpace x
    authtest xs
authtest _ =  Prelude.putStrLn "end"


sumWithSpace :: String -> String -> String 
sumWithSpace a b = a ++ " " ++ b-}

loadImage :: String -> String -> LBS.ByteString -> IO (Maybe Int)
loadImage fileName contentType content' = do
    if fileName == "" && contentType == "" && content' == ""
        then return Nothing
        else do
            conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
            let image = Image fileName contentType (Binary content')
            let q = "insert into images (image_name, image_b, content_type,) values (?,?,?) returning image_id"
    --(n:ns) :: [Myid] <- returning conn q [image] 
            [Only n] <- returning conn q [image]
            close conn
    --print n
            Prelude.putStrLn "image loaded"
            return (Just n)
tstLoadImage :: IO (Maybe Int)
tstLoadImage = loadImage "" "" ""

findUser :: T.Text -> IO (Maybe Int)
findUser s = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    rows <- query conn "select user_id from users where login = ?" (Login s) :: IO [Myid]
    if  Prelude.null rows then
        return Nothing
    else
        return (Just (fMyidTInt (Prelude.head rows)))

tl :: T.Text
tl = "dahaku"

fMyidTInt :: Myid -> Int
fMyidTInt (Myid x) = x




createUser' :: T.Text -> T.Text -> T.Text -> T.Text -> String -> String -> LBS.ByteString  -> IO ()
createUser' login password f_name l_name avatar_name avatar_contentType avatar= do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    now <- getCurrentTime
    n <- loadImage avatar_name avatar_contentType avatar
    let newUser = User' (Just f_name) (Just l_name) n login password now False
    execute conn "insert into users (first_name, last_name, avatar, login, user_password, creation_date, admin_mark) values (?,?,?,?,crypt(?,gen_salt('md5')),?,?)" newUser
    close conn
    Prelude.putStrLn "done"




loadImages :: [(String, String,LBS.ByteString)] -> IO [Int]
loadImages x = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='arraays'"
    let image = toImagesList x
    let q = "insert into images_data (image_name, content_type, image_b) values (?,?,?) returning image_id"
    --(n:ns) :: [Myid] <- returning conn q [image] 
    n <- returning conn q image
    close conn
    let ans = myidtointlst n
    Prelude.putStrLn "image loaded"
    return ans

toImagesList :: [(String, String,LBS.ByteString)] -> [Image]
toImagesList ((fileName, contentType, content'):xs) = Image fileName contentType (Binary content') : toImagesList xs
toImagesList [] = []

checkAuthor :: T.Text -> IO (Maybe Int)
checkAuthor login = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    rows <- query conn "select author_id from authors join users on authors.author_user_id = users.user_id where login = ?" [login]:: IO [Myid]
    if  Prelude.null rows then
        return Nothing
    else
        return (Just (fMyidTInt (Prelude.head rows)))


checkCategory :: T.Text  -> IO (Maybe Int)
checkCategory category = do
     conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
     rows <- query conn "select category_id from categories where category_name = ?" [category]:: IO [Only Int]
     if Prelude.null rows then
         return Nothing
     else
         return (Just (fromOnly $ Prelude.head rows))

tstCategory :: T.Text
tstCategory  = "Наука"



createTag :: T.Text -> IO ()
createTag tag  = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    let newTag = Tag (T.toLower  tag)
    if T.length tag > 20 then do
        close conn
        Prelude.putStrLn "to long tag"
                         else do
                            execute conn "insert into tags (tag_name) values (?)" newTag
                            close conn
                            Prelude.putStrLn "done"

checkTag :: [T.Text] -> IO (Either LBS.ByteString  [Int])
checkTag tagsList = do
    let len = T.length <$> tagsList
    --let nums = [1,2,3] :: [Int]
    if Prelude.any (>20) len then
        return $ Left "someone tag to long"
        else do
             conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
             rows <- query conn "select tag_id from tags where tag_name in ?" (Only (In tagsList)) :: IO [Myid]
             if Prelude.length rows < Prelude.length tagsList then return $ Left "someone tag not exist"
                else return $ Right $ fMyidTInt<$> rows

textsToTags :: [T.Text] -> [Tag]
textsToTags = Prelude.map Tag

tagL :: [T.Text]
tagL = ["новости", "наука"]


createDraft :: (String,String,LBS.ByteString) -> [(String,String,LBS.ByteString )] -> Int -> Int -> [Int] -> T.Text -> T.Text -> IO LBS.ByteString
createDraft main_image_triple images_list author_id' category_id' tags_ids text sh_title= do
    main_image_id <- loadImage (fstTriple main_image_triple) (sndTriple main_image_triple) (thrdTriple main_image_triple)
    other_images_ids <- loadImages images_list
    now <- getCurrentTime
    let (Just mid) = main_image_id
    let dr = Draft author_id' sh_title now category_id' text mid
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    [Only n] <- returning conn "insert into drafts (author_id, short_title, date_of_changes, category_id, draft_text, main_image) values (?,?,?,?,?,?) returning draft_id" [dr] :: IO [Only Int]
    --print "1"
    let drtg = drIdTagId n tags_ids
    executeMany conn "insert into draft_tags (draft_id, tag_id) values (?,?)" drtg
    let drim = drIdTagId n other_images_ids
    executeMany conn "insert into drafts_images (draft_id, image_id) values (?,?)" drim
    return "draft created"


{-insertDraftTag :: [DrIdTgId] -> IO ()
insertDraftTag xs = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    executeMany conn "insert into draft_tags (draft_id, tag_id) values (?,?)" xs
    print "done"-}


tst'' :: [DrIdTgId]
tst'' = [DrIdTgId 1 2, DrIdTgId 1 3]

{-stTriple (a,b,c) = a
sndTriple (a,b,c) = b
thrdTriple (a,b,c) = c-}

drIdTagId :: Int -> [Int] -> [DrIdTgId]
drIdTagId x (y:ys) = DrIdTgId x y : drIdTagId x ys
drIdTagId _ [] = []


checkNews :: Int -> IO Bool
checkNews n' = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='arraays'"
    rows <- query conn "select news_id from news where news_id = ?" [n'] :: IO [Only Int]
    close conn
    if Prelude.null rows then
        return False
        else return True

checkUser :: T.Text -> IO (Maybe Int)
checkUser login' = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    rows <- query conn "select user_id from users where login = ?" [login']:: IO [Myid]
    close conn
    if  Prelude.null rows then
        return Nothing
    else
        return (Just (fMyidTInt (Prelude.head rows)))
tst''' :: T.Text 
tst''' = "paffel2"

createComment :: Int -> Int -> T.Text -> IO Int
createComment userId newsId comment = do
    now <- getCurrentTime
    let new_comm = Comment userId comment newsId now
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    [Only n] <- returning conn "insert into users_comments (user_id, comment_text, news_id, comment_time) values (?,?,?,?) returning comment_id" [new_comm] :: IO [Only Int]
    close conn
    return n



getNewsFilterByTitle  :: Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByTitle  titleName page sortParam =
    case titleName of
      Nothing -> return (Left "No tag parametr")
      Just bs -> do
                    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                    --let cn = BC.pack $ show $ Prelude.length n - 1
                    let sort' = if sortParam == "" then ""
                                            else BC.concat [" order by ",sortParam," DESC"]
                    let pg = if isNothing page then " limit 10 offset 0"
                                        else 
                                     BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page)) - 1)*10 ]
                    let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, category_name, news_text from news join authors using (author_id) join users using (user_id) join categories using (category_id) where short_title = ? ",sort', pg]
                    rows <- query conn q [bs]
                    close conn
                    return (Right $ NewsArray' rows)

getNews' :: ByteString -> Maybe ByteString-> IO NewsArray'
getNews' sortParam pageParam = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    let sort' = if sortParam == "" then ""
                else BC.concat [" order by ",sortParam," DESC"]
    let pg = if isNothing pageParam then " limit 10 offset 0"
            else 
                BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" pageParam)) - 1)*10 ]
    let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, category_name, news_text  from news join authors using (author_id) join users using (user_id) join categories using (category_id)", sort',pg]
    --rows <- query_ conn "select news_id, short_title, date_creation, author_id, category_id, news_text  from news" :: IO [GetNews']
    rows <- query_ conn q:: IO [GetNews']
    close conn
    return (NewsArray' rows)


{-getNewsFromDb :: Handle -> ByteString -> Maybe ByteString-> IO (Either LBS.ByteString NewsArray')
getNewsFromDb hLogger sortParam pageParam = do
    logInfo hLogger "Someone try get news"
    let sort' = if sortParam == "" then ""
                else BC.concat [" order by ",sortParam," DESC"]
    let pg = if isNothing pageParam then " limit 10 offset 0"
            else 
                BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" pageParam)) - 1)*10 ]
    let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, category_name, news_text  from news join authors using (author_id) join users using (user_id) join categories using (category_id)", sort',pg]
    --rows <- query_ conn "select news_id, short_title, date_creation, author_id, category_id, news_text  from news" :: IO [GetNews']
    catch (do 
        conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
        rows <- query_ conn q:: IO [GetNews']
        close conn
        return $ Right (NewsArray' rows)) $ \e -> do
                                        let err = T.pack $ BC.unpack $ sqlErrorMsg e
                                        logError hLogger err
                                        return $ Left "Database error"-}

getNewsFilterByCategoryId :: Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByCategoryId cat_id page sortParam =
    case cat_id of
      Nothing -> return (Left "No category parametr")
      Just bs -> do
            case readByteStringToInt (fromMaybe "" cat_id) of
              Nothing -> return ( Left "bad category parametr")
              Just n -> do
                        conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                        let sort' = if sortParam == "" then ""
                                                else BC.concat [" order by ",sortParam," DESC"]
                        let pg = if isNothing page then " limit 10 offset 0"
                                            else 
                                         BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page)) - 1)*10 ]
                        let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, category_name, news_text  from news  join authors using (author_id) join users using (user_id) join categories using (category_id) where category_id = ? ", sort',pg]
                        rows <- query conn q [n]
                        close conn
                        return (Right $ NewsArray' rows)

getNewsFilterByTagId :: Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByTagId tag_id page sortParam =
    case tag_id of
      Nothing -> return (Left "No tag parametr")
      Just bs -> do
            case readByteStringToInt (fromMaybe "" tag_id) of
              Nothing -> return ( Left "bad tag parametr")
              Just n -> do
                        conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                        let sort' = if sortParam == "" then ""
                                                else BC.concat [" order by ",sortParam," DESC"]
                        --let pg = fromMaybe 1 (readByteStringToInt (fromMaybe "" page))
                        let pg = if isNothing page then " limit 10 offset 0"
                                            else 
                                         BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page)) - 1)*10 ]
                        --let fnews = FindNewsByCategory n ((pg-1)*10)
                        let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, category_name, news_text  from news  join authors using (author_id) join users using (user_id) join categories using (category_id) join news_tags using (news_id) where tag_id = ?", sort',pg]
                        --rows <- query conn "select news_id, short_title, date_creation, author_id, category_id, news_text  from news join news_tags using (news_id) where tag_id = ? order by 2 DESC limit 10 offset ?" fnews :: IO [GetNews']
                        rows <- query conn q [n]
                        close conn
                        return (Right $ NewsArray' rows)

getNewsFilterByTagIn  :: Maybe ByteString -> Maybe ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByTagIn  tag_lst page =
    case tag_lst of
      Nothing -> return (Left "No tag parametr")
      Just bs -> do
            case readByteStringListInt (fromMaybe "" tag_lst) of
              Nothing -> return ( Left "bad tag parametr")
              Just n -> do
                        conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                        
                        let pg = fromMaybe 1 (readByteStringToInt (fromMaybe "" page))
                        rows <- query conn "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, category_name, news_text  from news join authors using (author_id) join users using (user_id) join categories using (category_id) join news_tags using (news_id) where tag_id in ? group by news_id,author_name,category_name order by 2 DESC " (Only (In n))
                        close conn
                        return (Right $ NewsArray' $ takePage pg rows)

getNewsFilterByTagAll  :: Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByTagAll  tag_lst page sortParam =
    case tag_lst of
      Nothing -> return (Left "No tag parametr")
      Just bs -> do
            case readByteStringListInt (fromMaybe "" tag_lst) of
              Nothing -> return ( Left "bad tag_all parametr")
              Just n -> do
                        conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                        let cn = BC.pack $ show $ Prelude.length n - 1
                        let sort' = if sortParam == "" then ""
                                                else BC.concat [" order by ",sortParam," DESC"]
                        let pg = if isNothing page then " limit 10 offset 0"
                                            else 
                                         BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page)) - 1)*10 ]
                        let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, category_name, news_text  from news join authors using (author_id) join users using (user_id) join categories using (category_id) join news_tags using (news_id) where tag_id in ? group by news_id,author_name,category_name having count(*) > ",cn, " ",sort',pg]
                        rows <- query conn q (Only (In n))
                        close conn
                        return (Right $ NewsArray' rows)
getNewsFilterByAuthorName  :: Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByAuthorName  authorName page sortParam =
    case authorName of
      Nothing -> return (Left "No tag parametr")
      Just bs -> do
                    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                    let sort' = if sortParam == "" then ""
                                            else BC.concat [" order by ",sortParam," DESC"]
                    let pg = if isNothing page then " limit 10 offset 0"
                                        else 
                                     BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page)) - 1)*10 ]
                    let q = toQuery $ BC.concat ["select * from (select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, category_name, news_text from news join authors using (author_id) join users using (user_id) join categories using (category_id)) as temp_t where author_name = ? ",sort', pg]
                    rows <- query conn q [bs]
                    close conn
                    return (Right $ NewsArray' rows)

getNewsFilterByContent  :: Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByContent  content page sortParam =
    case content of
      Nothing -> return (Left "No tag parametr")
      Just bs -> do
                    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                    let sort' = if sortParam == "" then ""
                                            else BC.concat [" order by ",sortParam," DESC"]
                    let pg = if isNothing page then " limit 10 offset 0"
                                        else 
                                     BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page)) - 1)*10 ]
                    let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, category_name, news_text from news join authors using (author_id) join users using (user_id) join categories using (category_id) where news_text like ? ",sort', pg]
                    rows <- query conn q [BC.concat ["%",bs,"%"]]
                    close conn
                    return (Right $ NewsArray' rows)

getNewsFilterByDate  :: Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByDate  date page sortParam =
    case date of
      Nothing -> return (Left "No date parametr")
      Just bs -> do
                    let date' = readByteStringToDay bs
                    case date' of
                      Nothing -> return (Left "Bad date parametr")
                      Just day -> do
                                conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                                let sort' = if sortParam == "" then ""
                                            else BC.concat [" order by ",sortParam," DESC"]
                                let pg = if isNothing page then " limit 10 offset 0"
                                         else 
                                            BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page)) - 1)*10 ]
                                let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, category_name, news_text from news join authors using (author_id) join users using (user_id) join categories using (category_id) where date_creation = ? ",sort', pg]
                                rows <- query conn q [day]
                                close conn
                                return (Right $ NewsArray' rows)


getNewsFilterByAfterDate  :: Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByAfterDate  date page sortParam =
    case date of
      Nothing -> return (Left "No date parametr")
      Just bs -> do
                    let date' = readByteStringToDay bs
                    case date' of
                      Nothing -> return (Left "Bad date parametr")
                      Just day -> do
                                conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                                let sort' = if sortParam == "" then ""
                                            else BC.concat [" order by ",sortParam," DESC"]
                                let pg = if isNothing page then " limit 10 offset 0"
                                         else 
                                            BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page)) - 1)*10 ]
                                let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, category_name, news_text from news join authors using (author_id) join users using (user_id) join categories using (category_id) where date_creation > ? ",sort', pg]
                                rows <- query conn q [day]
                                close conn
                                return (Right $ NewsArray' rows)
getNewsFilterByBeforeDate  :: Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByBeforeDate  date page sortParam =
    case date of
      Nothing -> return (Left "No date parametr")
      Just bs -> do
                    let date' = readByteStringToDay bs
                    case date' of
                      Nothing -> return (Left "Bad date parametr")
                      Just day -> do
                                conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                                let sort' = if sortParam == "" then ""
                                            else BC.concat [" order by ",sortParam," DESC"]
                                let pg = if isNothing page then " limit 10 offset 0"
                                         else 
                                            BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page)) - 1)*10 ]
                                let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, category_name, news_text from news join authors using (author_id) join users using (user_id) join categories using (category_id) where date_creation < ? ",sort', pg]
                                rows <- query conn q [day]
                                close conn
                                return (Right $ NewsArray' rows)

getNewsById :: Int-> IO NewsArray'
getNewsById news_id = do
    if news_id /= 0 then do
        conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
        let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, category_name, news_text  from news join authors using (author_id) join users using (user_id) join categories using (category_id) where news_id = ?"]
        rows <- query conn q [news_id]
        close conn
        return (NewsArray' rows)
        else return (NewsArray' [])



getCommentsByNewsId :: Int -> Maybe ByteString ->  IO (Either LBS.ByteString CommentArray)
getCommentsByNewsId news_id page = do
        --conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
        check <- checkNews news_id
        if check then do
            conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
            let q = "select (concat(first_name, ' ', last_name)) as author_name, comment_text, comment_time, comment_id from users_comments join users using (user_id) where news_id = ? order by comment_time"
            rows <- query conn q [news_id]
            close conn
            return (Right $ CommentArray rows)
        else return (Left "News not exist")

{-addComment' :: T.Text  -> Int -> T.Text -> IO (Either LBS.ByteString LBS.ByteString)
addComment' login newsId comment = do
    cn <- checkNews newsId
    if not cn then
        return $ Left "News not exsist"
    else do
        cu <- checkUser login
        case cu of
          Nothing -> return $ Left "User not exsist"
          Just userId -> do
              now <- getCurrentTime 
              let com = Comment userId comment newsId now
              conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
              execute conn "insert into users_comments (user_id, comment_text, news_id, comment_time) values (?,?,?,?)" com
              close conn
              return $ Right "comment added"-}

addComment :: T.Text  -> Int -> T.Text -> IO (Either LBS.ByteString LBS.ByteString)
addComment token newsId comment = do
    cn <- checkNews newsId
    if not cn then
        return $ Left "News not exsist"
    else do
        ct <- checkToken token
        case ct of
            (0, mes) -> return $ Left mes
            (userId,_) -> do
                now <- getCurrentTime 
                let com = Comment userId comment newsId now
                conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                execute conn "insert into users_comments (user_id, comment_text, news_id, comment_time) values (?,?,?,?)" com
                close conn
                return $ Right "comment added"

    {-let new_comm = Comment userId comment newsId now
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    [Only n] <- returning conn "insert into users_comments (user_id, comment_text, news_id, comment_time) values (?,?,?,?) returning comment_id" [new_comm] :: IO [Only Int]
    close conn
    return n-}
deleteComment :: ByteString  -> IO (Either LBS.ByteString LBS.ByteString)
deleteComment comment_id = do
    let ci = readByteStringToInt comment_id
    case ci of
      Nothing -> return $ Left "Bad comment_id"
      Just n -> do
          conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
          execute conn "delete from users_comments where comment_id = ?" [n]
          return $ Right "Comment deleted"
        {-return $ Left "News not exsist"
    else do
        cu <- checkUser login
        case cu of
          Nothing -> return $ Left "User not exsist"
          Just userId -> do
              now <- getCurrentTime 
              let com = Comment userId comment newsId now
              conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
              execute conn "insert into users_comments (user_id, comment_text, news_id, comment_time) values (?,?,?,?)" com
              close conn
              return $ Right "comment added"-}

checkToken :: T.Text -> IO (Int, LBS.ByteString)
checkToken token = do
    --TIO.putStrLn token
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    rows <- query conn "select user_id, creation_date from tokens where token = ?" [token] :: IO [CheckToken]
    close conn
    --print rows
    --return (1,"")
    --print rows
    --return (True,"")
    if Prelude.null rows then
        return (0, "Token not exist")
        else do
            now <- getCurrentTime
            if diffUTCTime now (ct_creation_date $ Prelude.head rows) > 86400 then  return (0,"Token too old")
                                         else return (ct_user_id $ Prelude.head rows,"")


checkAdmin :: T.Text -> IO (Bool, LBS.ByteString)
checkAdmin token = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    rows <- query conn "select admin_mark from users join tokens using (user_id) where token = ? and (current_timestamp - tokens.creation_date < interval '1 day')" [token] :: IO [Only Bool]
    close conn
    if Prelude.null rows then
        return (False, "Bad token")
        else do
            let ct = fromOnly $ Prelude.head rows
            if ct then
                return (ct,"")
            else
                return (ct, "Not admin")



createUser'' :: BC.ByteString  -> BC.ByteString  -> T.Text -> T.Text -> String -> String -> LBS.ByteString  -> IO (Either LBS.ByteString LBS.ByteString )
createUser'' login password f_name l_name avatar_name avatar_contentType avatar= do
    cu <- checkUser $ E.decodeUtf8 login
    --TIO.putStrLn $ E.decodeUtf8 login
    case cu of
      Just _ -> return $ Left "User with same login already exist"
      Nothing -> do
            conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
            now <- getCurrentTime
            n <- loadImage' avatar_name avatar_contentType avatar
            case n of
              Nothing -> return $ Left "Avatar not loaded"
              i -> do
                let newUser = User'' (Just f_name) (Just l_name) i (E.decodeUtf8 login) (E.decodeUtf8 password) now False (E.decodeUtf8 login)
                --print newUser
                catch (do
                    rows <- query conn "insert into users (first_name, last_name, avatar, login, user_password, creation_date, admin_mark) values (?,?,?,?,crypt(?,gen_salt('md5')),?,?); select user_id from users where login = ?" newUser :: IO [Only Int]
                --execute conn "update users set user_password = crypt(?,gen_salt('md5')) where login = ?" (T (E.decodeUtf8 password) (E.decodeUtf8 login))
                    close conn
                    if Prelude.null rows then
                        return $ Left "cant register new user"
                        else auth login password) $ \e ->  do
                                        let err = sqlErrorMsg e
                                        TIO.putStrLn $ E.decodeUtf8 err
                                        return $ Left "Database error"


loadImage' :: String -> String -> LBS.ByteString -> IO (Maybe Int)
loadImage' fileName contentType content' = do
    if fileName == "" && contentType == "" && content' == ""
        then return Nothing
        else do
            conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
            let image = Image' fileName (Binary content') contentType
            let q = "insert into images (image_name, image_b, content_type) values (?,?,?) returning image_id"
    --(n:ns) :: [Myid] <- returning conn q [image] 
            rows <- returning conn q [image] :: IO [Only Int]
            close conn
    --print n
            if Prelude.null rows then do
                Prelude.putStrLn "Image not loaded"
                return Nothing
            else do
                Prelude.putStrLn "image loaded"
                return (Just (fromOnly $ Prelude.head rows))



{-tstToken :: T.Text 
tstToken = "dahaku20210713085011962691"

tstToken' :: T.Text 
tstToken' = "niamh20210713095646487796"

tstToken'' :: T.Text 
tstToken'' = "12"-}

{-readByteStringToInt :: BC.ByteString -> Maybe Int
readByteStringToInt num = readMaybe $ BC.unpack num

readByteStringListInt :: BC.ByteString -> Maybe [Int]
readByteStringListInt lst = readMaybe $ BC.unpack lst


takePage :: Int -> [a] -> [a]
takePage p list = Prelude.take 10 $ Prelude.drop ((p-1)*10) list


readByteStringToDay :: BC.ByteString -> Maybe Day
readByteStringToDay bs = readMaybe $ BC.unpack bs



toQuery :: BC.ByteString -> Query 
toQuery s = fromString $ BC.unpack s-}


{-crut :: IO Int
crut = do
    now <- getCurrentTime 
    let newUser = Rt "123" (Just "f_name") (Just "l_name") (Just 25) "123" now  False
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    let q = "insert into tst (p, f, l , a , login , c, m) values (crypt(?,gen_salt('md5')),?,?,?,  ?, ? ) RETURNING tst_id"
    rows <- returning conn q [newUser] :: IO [Only Int]
    --execute conn "update tst set user_password = crypt(?,gen_salt('md5')) where login = ?" (T "pas" "321")
    print rows
    close conn
    return 1-}




{-cr :: IO ()
cr = do--login password f_name l_name avatar_name avatar_contentType avatar= do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    now <- getCurrentTime
    --n <- loadImage avatar_name avatar_contentType avatar
    --let newUser = User' (Just "f_name") (Just "l_name") (Just 5) "login" "password" now False 
    let newUser = Rt (Just "f_name") (Just "l_name") (Just 5) "login1" "password" now False "login1"
    --execute conn "insert into tst (f, l, a, login, p, c, m) values (?,?,?,?,crypt(?,gen_salt('md5')),?,?)" newUser
    let q = "insert into tst (f, l, a, login, p, c, m) values (?,?,?,?,crypt(?,gen_salt('md5')),?,?); select tst_id from tst where login = ?"
    rows <- query conn q newUser :: IO [Only Int]
    print rows
    close conn
    Prelude.putStrLn "done"-}


getCategoriesList :: Maybe BC.ByteString -> IO ListOfCategories
getCategoriesList pageParam = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    let pg = if isNothing pageParam then " limit 10 offset 0"
                else 
                    BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" pageParam)) - 1)*10 ]
    let q = toQuery $ BC.concat ["select category_name from categories",pg]
    rows <- query_ conn q :: IO [Category']
    close conn
    return (ListOfCategories rows)

deleteCategory :: T.Text -> IO LBS.ByteString 
deleteCategory categoryName = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    execute conn "delete from categories where category_name = ?" [categoryName]
    close conn
    --execute conn "delete from tst where c_name = ?" [categoryName]
    return "Category deleted"

editCategoryName :: ByteString -> Maybe ByteString -> IO (Either LBS.ByteString LBS.ByteString )
editCategoryName old_name new_name = do
    let new_name' = fromMaybe "" new_name
    cnn <- checkCategory $ E.decodeUtf8 new_name'
    case cnn of
      Just n -> return $ Left "category with new name already exist"
      Nothing -> do
          if new_name' /= "" then do
            conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
            let q = "update categories set category_name = ? where category_name = ?"
            let edit = EditCategoryName (E.decodeUtf8 new_name') (E.decodeUtf8 old_name)
            --let q = "update tst set c_name = ? where c_name = ?" 
            execute conn q edit
            return $ Right "Category name changed"
            else
                return $ Left "Bad new name"

editCategoryMaternal :: ByteString -> Maybe ByteString -> IO (Either LBS.ByteString LBS.ByteString )
editCategoryMaternal category_name new_maternal = do
    let new_maternal' = fromMaybe "" new_maternal
    cnn <- checkCategory $ E.decodeUtf8 new_maternal'
    case cnn of
      Nothing -> return $ Left "maternal category not exist"
      Just n -> do
            conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
            let q = "update categories set maternal_category = ? where category_name = ?"
            let edit = EditCategoryMaternal n (E.decodeUtf8 category_name)
            --let q = "update tst set m_id = ? where c_name = ?" 
            execute conn q edit
            return $ Right "Maternal category changed"

fullEditCategory :: ByteString -> Maybe ByteString -> Maybe ByteString -> IO (Either LBS.ByteString LBS.ByteString )
fullEditCategory old_name new_name new_maternal = do
    let new_name' = fromMaybe "" new_name
    cnn <- checkCategory $ E.decodeUtf8 new_name'
    case cnn of
      Just n -> return $ Left "category with new name already exist"
      Nothing -> do
            if new_name' /= "" then do
                let new_maternal' = fromMaybe "" new_maternal
                cnn <- checkCategory $ E.decodeUtf8 new_maternal'
                case cnn of
                    Nothing -> return $ Left "maternal category not exist"
                    Just n -> do
                        conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                        let edit = EditCategory (E.decodeUtf8 new_name') n (E.decodeUtf8 old_name)
                        let q = "update categories set category_name =? ,maternal_category = ? where category_name = ?"
                        --let q = "update tst set c_name = ?, m_id = ? where c_name = ?" 
                        execute conn q edit
                        return $ Right "Category edited"
            else
                return $ Left "Bad new name"


deleteUserFromDb :: ByteString -> IO LBS.ByteString
deleteUserFromDb login = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    let q = "delete from users where login = ?"
    execute conn q [login]
    close conn
    return "User deleted"



checkAuthor' :: T.Text -> IO (Maybe Int)
checkAuthor' token = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    --TIO.putStrLn token
    rows <- query conn "select author_id from authors join users using (user_id) join tokens using (user_id) where token = ?" [token]:: IO [Only Int]
    if  Prelude.null rows then
        return Nothing
    else
        return $ Just (fromOnly $ Prelude.head rows)

getDraftsByAuthorId :: Int -> IO DraftArray 
getDraftsByAuthorId a_id = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    --let q = "select short_title, date_of_changes, category_id, draft_text, main_image from drafts where author_id = ?"
    let q = "select short_title, date_of_changes, category_id, draft_text, main_image, array_agg(image_id) from drafts join drafts_images using (draft_id) where author_id = ? group by draft_id"
    rows <- query conn q [a_id] :: IO [Draft']
    return $ DraftArray rows

tstC :: (Maybe T.Text, Maybe Int, Maybe T.Text)
tstC = (Just "", Just 20, Just "" )
{-getNews' :: ByteString -> Maybe ByteString-> IO NewsArray'
getNews' sortParam pageParam = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    let sort' = if sortParam == "" then ""
                else BC.concat [" order by ",sortParam," DESC"]
    let pg = if isNothing pageParam then " limit 10 offset 0"
            else 
                BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" pageParam)) - 1)*10 ]
    let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, category_name, news_text  from news join authors using (author_id) join users using (user_id) join categories using (category_id)", sort',pg]
    --rows <- query_ conn "select news_id, short_title, date_creation, author_id, category_id, news_text  from news" :: IO [GetNews']
    rows <- query_ conn q:: IO [GetNews']
    close conn
    return (NewsArray' rows)-}



createDraftOnDb :: (String,String,LBS.ByteString) -> [(String,String,LBS.ByteString )] -> Int -> Int -> [Int] -> T.Text -> T.Text -> IO (Either LBS.ByteString Int)
createDraftOnDb main_image_triple images_list author_id' category_id' tags_ids text sh_title= do
    main_image_id <- loadImage' (fstTriple main_image_triple) (sndTriple main_image_triple) (thrdTriple main_image_triple)
    case main_image_id of
      Nothing -> return $ Left "Images not loaded"
      Just main_i_id -> do
            other_images <- loadImages' images_list
            case other_images of
              Left bs -> return $ Left bs
              Right other_images_ids -> do
                    now <- getCurrentTime
                    --let (Just mid) = main_image_id
                    let dr = Draft author_id' sh_title now category_id' text main_i_id
                    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                    d_rows <- returning conn "insert into drafts (author_id, short_title, date_of_changes, category_id, draft_text, main_image) values (?,?,?,?,?,?) returning draft_id" [dr] :: IO [Only Int]
                    if Prelude.null d_rows then
                        return $ Left "Draft not created"
                    else do
                        let n = fromOnly $ Prelude.head d_rows
                        let drtg = drIdTagId n tags_ids
                        rows_dt <- executeMany conn "insert into draft_tags (draft_id, tag_id) values (?,?)" drtg
                        if rows_dt /= 0 then do
                            let drim = drIdTagId n other_images_ids
                            rows_di <- executeMany conn "insert into drafts_images (draft_id, image_id) values (?,?)" drim
                            if rows_di /= 0 then do
                                close conn
                                return $ Right n
                            else do
                                execute conn "delete from drafts where drfat_id = ?" [n]
                                close conn
                                return $ Left "draft not created"
                        else do
                            execute conn "delete from drafts where drfat_id = ?" [n]
                            close conn
                            return $ Left "draft not created"




loadImages' :: [(String, String,LBS.ByteString)] -> IO (Either LBS.ByteString [Int])
loadImages' x = do 
    catch  ( do
            conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
            let image = toImagesList' x
            let q = "insert into images (image_name, image_b, content_type) values (?,?,?) returning image_id"
    --(n:ns) :: [Myid] <- returning conn q [image] 
            n <- returning conn q image :: IO [Only Int]
            close conn
            let ans = fromOnly <$> n
            --print ans
            Prelude.putStrLn "images loaded"
            return $ Right ans) $ \e -> do
                    let err = show (e :: IOException)
                    return $ Left "cant loaded images"



toImagesList' :: [(String, String,LBS.ByteString)] -> [Image']
toImagesList' ((fileName, contentType, content'):xs) = Image' fileName (Binary content') contentType  : toImagesList' xs
toImagesList' [] = []


getDraftIdsByAuthor :: Int -> IO (Either LBS.ByteString  [Int])
getDraftIdsByAuthor author_id = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    let q = "select draft_id from drafts where author_id = ?"
    rows <- query conn q [author_id] :: IO [Only Int]
    if Prelude.null rows then
        return $ Left "No drafts"
    else
        return $ Right (fromOnly <$> rows)

deleteDraftFromDb :: Int -> Int -> IO (Either LBS.ByteString LBS.ByteString)
deleteDraftFromDb author_id draft_id = do
    draft_list <- getDraftIdsByAuthor author_id
    case draft_list of
      Left bs -> return $ Left bs
      Right draft_ids_list -> do
              if draft_id `Prelude.notElem` draft_ids_list then
                  return $ Left "You cant delete this draft"
                else do
                    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                    let q = "delete from drafts where draft_id = ?"
                    execute conn q [draft_id]
                    close conn
                    return $ Right "Drafts deleted"


getDraftByIdDromDb :: Int -> Int -> IO (Either LBS.ByteString  Draft')
getDraftByIdDromDb author_id draft_id = do
    draft_list <- getDraftIdsByAuthor author_id
    case draft_list of
      Left bs -> return $ Left bs
      Right draft_ids_list -> do
            if draft_id `Prelude.notElem` draft_ids_list then
                return $ Left "Wrong draft_id"
            else do
                conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                let q = "select short_title, date_of_changes, category_id, draft_text, main_image, array_agg(image_id) from drafts join drafts_images using (draft_id) where author_id = ? and draft_id = ? group by draft_id"
                --execute conn q [draft_id]
                --let req = GetDraftById author_id draft_id
                rows <- query conn q (author_id,draft_id) :: IO [Draft']
                close conn
                if Prelude.null rows then
                    return $ Left "Drafts deleted"
                else return $ Right $ Prelude.head rows


updateDraftInDb :: (String,String,LBS.ByteString) -> [(String,String,LBS.ByteString )] -> Int -> Int -> [Int] -> T.Text -> T.Text -> Int -> IO (Either LBS.ByteString LBS.ByteString)
updateDraftInDb main_image_triple images_list author_id category_id tags_id text short_title draft_id  = do
    draft_list <- getDraftIdsByAuthor author_id
    case draft_list of
      Left bs -> return $ Left bs
      Right draft_ids_list -> do
                    if draft_id `Prelude.notElem` draft_ids_list then
                        return $ Left "Wrong draft_id"
                    else do
                        new_draft <- createDraftOnDb main_image_triple images_list author_id category_id tags_id text short_title
                        case new_draft of
                          Left bs -> return $ Left bs
                          Right new_id -> do
                              conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                              let delete_q = "delete from drafts where draft_id = ?"
                              execute conn delete_q [draft_id]
                              let q = "update drafts set draft_id = ? where draft_id = ?"
                              execute conn q (draft_id,new_id)
                              close conn
                              return $ Right "Draft updated"


publicNewsOnDb :: Int -> Int -> IO (Either LBS.ByteString LBS.ByteString)
publicNewsOnDb author_id draft_id = do
    dr <- getDraftByIdDromDb author_id draft_id
    case dr of
        Left bs -> return $ Left bs
        Right draft -> do
                conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                let q = "insert into news (short_title, date_creation, author_id, category_id, news_text, main_image) values (?,?,?,?,?,?) returning news_id"
                now <- getCurrentTime 
                n_id <- returning conn q [(draft_short_title' draft,now, author_id,draft_category_id' draft, draft_text' draft,draft_main_image_id' draft)] :: IO [Only Int]
                if Prelude.null n_id then do
                    close conn
                    return $ Left "News not published"
                else do
                    --let news_id = fromOnly $ Prelude.head n_id
                    --let q_images = toQuery $ BC.concat ["insert into News_images (news_id,image_id) values (", BC.pack $ show news_id, ",?)"]
                    --let images_ids = fromPGArray $ draft_images draft
                    --execute conn q_images images_ids
                    execute conn "delete from drafts where draft_id = ? " [draft_id]
                    close conn
                    return $ Right "news published"

                        {--conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                        --let delete_q = "delet from drafts where draft_id = ?"
                        --execute conn delete_q [draft_id]
                        --let q = "insert into drafts"
                        main_image_id <- loadImage' (fstTriple main_image_triple) (sndTriple main_image_triple) (thrdTriple main_image_triple)
                        case main_image_id of
                            Nothing -> return $ Left "Images not loaded"
                            Just main_i_id -> do
                                other_images <- loadImages' images_list
                                case other_images of
                                        Left bs -> return $ Left bs
                                        Right other_images_ids -> do
                                                    now <- getCurrentTime
                                                    close conn
                                                    return $ Right ""  -}
--createDraftOnDb :: (String,String,LBS.ByteString) -> [(String,String,LBS.ByteString )] -> Int -> Int -> [Int] -> T.Text -> T.Text -> IO (Either LBS.ByteString Int)
--createDraftOnDb main_image_triple images_list author_id' category_id' tags_ids text sh_title= do