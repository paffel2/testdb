{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Testdatabase where
import Database.PostgreSQL.Simple
--import Database.PostgreSQL.Simple.FromField
import Control.Monad
import Control.Applicative
import Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Types
import Data.Time
import Control.Exception
import Database.PostgreSQL.Simple.Internal
import qualified Data.Text.Encoding as E
import Data.Maybe





a :: Int 
a = 1 + 1

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



auth :: String  -> String -> IO String
auth login password = do 
    catch ( do
            conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
            [Only check] <- query conn "select (case check_pass  when TRUE then 'token' when FALSE then 'wrong password' else 'wrong login' end) from (select (user_password = crypt(?,user_password)) as check_pass from users where login = ?) ce" (password,login)        
            close conn
            return check) 
        $ \e -> do 
                    let err = show (e :: IOException)
                    return "wrong login"


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

createCategory :: T.Text -> Maybe Int -> IO ()
createCategory categoryName maternalCategoryId = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    let newCategory = Category (T.toLower  categoryName) maternalCategoryId
    execute conn "insert into categories (category_name, maternal_category) values (?,?)" newCategory
    close conn
    Prelude.putStrLn "done"

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
            conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='arraays'"
            let image = Image fileName contentType (Binary content')
            let q = "insert into images_data (image_name, content_type, image_b) values (?,?,?) returning image_id"
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
     rows <- query conn "select category_id from categories where category_name = ?" [category]:: IO [Myid]
     if Prelude.null rows then
         return Nothing 
     else
         return (Just (fMyidTInt (Prelude.head rows)))

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
    let nums = [1,2,3] :: [Int]
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
    print "1"
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

fstTriple (a,b,c) = a
sndTriple (a,b,c) = b
thrdTriple (a,b,c) = c

drIdTagId :: Int -> [Int] -> [DrIdTgId]
drIdTagId x (y:ys) = DrIdTgId x y : drIdTagId x ys
drIdTagId _ [] = []