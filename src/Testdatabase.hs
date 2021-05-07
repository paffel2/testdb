{-# LANGUAGE OverloadedStrings #-}
module Testdatabase where
import Database.PostgreSQL.Simple
import Control.Monad
import Control.Applicative
import Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Types
import Data.Time
import Control.Exception
import Database.PostgreSQL.Simple.Internal



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

createAuthor :: IO ()
createAuthor = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    let newAuthor = Author 1 (Just "ньюфаг")
    execute conn "insert into authors (author_user_id, description) values (?,?)" newAuthor
    close conn
    Prelude.putStrLn "done"

createCategory :: String -> Maybe Int -> IO ()
createCategory categoryName maternalCategoryId = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    let newCategory = Category categoryName maternalCategoryId
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



{-downloadImages :: IO ()
downloadImages = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    [Only (Binary i)] <- query_ conn "select main_image from news where news_id = 1"
    close conn
    BC.writeFile "src/dl.jpg" i
    Prelude.putStrLn "done"-}
    















