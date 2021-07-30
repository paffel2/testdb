{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Databaseoperations where
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


getNewsFromDb :: Handle -> ByteString -> Maybe ByteString-> IO (Either LBS.ByteString NewsArray')
getNewsFromDb hLogger sortParam pageParam = do
    logInfo hLogger "Someone try get news list"
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
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        logError hLogger "Database error in getting news list"
                                        return $ Left "Database error"

getNewsByIdFromDb :: Handle -> Maybe Int-> IO (Either LBS.ByteString  NewsArray')
getNewsByIdFromDb hLogger news_id = do
    case news_id of
      Nothing -> do 
          logError hLogger "Bad news_id parameter"
          return $ Left "Bad news_id parameter"
      Just n -> do
            catch ( do
                logInfo hLogger $ T.concat ["Someone try get news №", T.pack $ show n]
                conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, category_name, news_text  from news join authors using (author_id) join users using (user_id) join categories using (category_id) where news_id = ?"]
                rows <- query conn q [news_id]
                close conn
                logInfo hLogger $ T.concat ["Sending news with id = ", T.pack $ show n]
                return $ Right (NewsArray' rows) )$ \e -> do
                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                            logError hLogger  $ T.concat["Database error in getting news №", T.pack $ show n]
                                            return $ Left "Database error"


getCommentsByNewsIdFromDb :: Handle -> Maybe Int -> Maybe ByteString ->  IO (Either LBS.ByteString CommentArray)
getCommentsByNewsIdFromDb hLogger news_id page = do
              check <- checkNews hLogger news_id
              case check of
                    (0, e) -> do
                        --logError hLogger e
                        return $ Left e
                    (n,_) ->  catch ( do
                            logInfo hLogger $ T.concat ["Someone try get comments from news №", T.pack $ show n]
                            conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                            let q = "select (concat(first_name, ' ', last_name)) as author_name, comment_text, comment_time, comment_id from users_comments join users using (user_id) where news_id = ? order by comment_time"
                            rows <- query conn q [n]
                            close conn
                            return (Right $ CommentArray rows)) $ \e -> do 
                                                    let err = E.decodeUtf8 $ sqlErrorMsg e
                                                    logError hLogger $ T.concat["Database error in getting comments from news №", T.pack $ show n]
                                                    return $ Left "Database error"

checkNews :: Handle -> Maybe Int -> IO (Int, LBS.ByteString )
checkNews hLogger news_id = do
    case news_id of
      Nothing -> do
            logError hLogger "Bad news id"
            return  (0, "Bad news id")
      Just n -> catch (do
        conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='arraays'"
        rows <- query conn "select news_id from news where news_id = ?" [n] :: IO [Only Int]
        close conn
        if Prelude.null rows then do
            logError hLogger "News not exist"
            return (0, "News not exist")
        else return (fromOnly $ Prelude.head rows, "") )$ \e -> do
                                let err = E.decodeUtf8 $ sqlErrorMsg e
                                logError hLogger err
                                return (0,"Database error")




addCommentToDb :: Handle -> T.Text  -> Maybe Int -> T.Text -> IO (Either LBS.ByteString LBS.ByteString)
addCommentToDb hLogger token newsId comment = do
            check_news_id <- checkNews hLogger newsId
            case check_news_id of
                (0, e') -> do
                        return $ Left e'
                (n, _) -> do
                    check_token <- checkToken hLogger token
                    case check_token of
                        (0, e) -> do
                            return $ Left e
                        (userId,_) -> catch (do
                            logInfo hLogger $ T.concat ["User №", T.pack $ show userId, " try add commentary to news №", T.pack $ show n]
                            now <- getCurrentTime 
                            let com = Comment userId comment n now
                            conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                            execute conn "insert into users_comments (user_id, comment_text, news_id, comment_time) values (?,?,?,?)" com
                            close conn
                            logInfo hLogger $ T.concat ["User №", T.pack $ show userId, "  add news commentary to news №", T.pack $ show n]
                            return $ Right "comment added") $ \e -> do
                                                                    let err = E.decodeUtf8 $ sqlErrorMsg e
                                                                    logError hLogger err
                                                                    return $ Left "Database error"

checkToken :: Handle -> T.Text -> IO (Int, LBS.ByteString )
checkToken hLogger token = catch (do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    rows <- query conn "select user_id, creation_date from tokens where token = ?" [token] :: IO [CheckToken]
    close conn
    if Prelude.null rows then do
        logError hLogger "Token not exist"
        return (0, "Token not exist")
        else do
            now <- getCurrentTime
            if diffUTCTime now (ct_creation_date $ Prelude.head rows) > 86400 then  return (0,"Token too old")
                                         else return (ct_user_id $ Prelude.head rows,"")) $ \e -> do
                                                                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                                                                            logError hLogger err
                                                                                            return (0,"Database error")


deleteCommentFromDb :: Handle -> T.Text -> Maybe Int -> IO (Either LBS.ByteString LBS.ByteString)
deleteCommentFromDb hLogger token comment_id = do
    --let ci = readByteStringToInt comment_id
    case comment_id of
      Nothing -> do
            logError hLogger "Bad comment id"
            return $ Left "Bad comment id"
      Just n -> do
        isAdmin <- checkAdmin hLogger token
        case isAdmin of
            (False,e) -> return $ Left e
            (True ,_) -> catch (do
                conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                execute conn "delete from users_comments where comment_id = ?" [n]
                return $ Right "Comment deleted") $ \e -> do
                                                let err = E.decodeUtf8 $ sqlErrorMsg e
                                                logError hLogger err
                                                return $ Left "Database error"



checkAdmin :: Handle -> T.Text -> IO (Bool, LBS.ByteString)
checkAdmin hLogger token = catch (do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    rows <- query conn "select admin_mark from users join tokens using (user_id) where token = ? and (current_timestamp - tokens.creation_date < interval '1 day')" [token] :: IO [Only Bool]
    close conn
    if Prelude.null rows then do
        logError hLogger "Bad token"
        return (False, "Bad token")
        else do
            let admin_mark = fromOnly $ Prelude.head rows
            if admin_mark then
                return (admin_mark,"")
            else
                return (admin_mark, "Not admin")) $ \e -> do
                                                let err = E.decodeUtf8 $ sqlErrorMsg e
                                                logError hLogger err
                                                return (False,"Database error")


getNewsFilterByTagInFromDb :: Handle -> Maybe ByteString -> Maybe ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByTagInFromDb hLogger tag_lst page = do
    logInfo hLogger "Someone try get news list filtered by tag_in parameter"
    case tag_lst of
        Nothing -> do
            logError hLogger "No tag parameter"
            return $ Left "No tag parameter"
        Just bs -> do
            case readByteStringListInt (fromMaybe "" tag_lst) of
                Nothing -> do
                    logError hLogger "Bad tag parameter"
                    return $ Left "bad tag parameter"
                Just n -> catch (do
                        conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                        let pg = fromMaybe 1 (readByteStringToInt (fromMaybe "" page))
                        rows <- query conn "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, category_name, news_text  from news join authors using (author_id) join users using (user_id) join categories using (category_id) join news_tags using (news_id) where tag_id in ? group by news_id,author_name,category_name order by 2 DESC " (Only (In n))
                        close conn
                        return (Right $ NewsArray' $ takePage pg rows)) $ \e -> do
                                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                                            logError hLogger err
                                                            return $ Left "Database error"

getNewsFilterByCategoryIdFromDb :: Handle -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByCategoryIdFromDb hLogger cat_id page sortParam = do
    logInfo hLogger "Someone try get news list filtered by category id"
    case cat_id of
        Nothing -> do
            logError hLogger "No category parameter"
            return $ Left "No category parameter"
        Just bs -> do
            case readByteStringToInt (fromMaybe "" cat_id) of
                Nothing -> do
                    logError hLogger "Bad category parameter"
                    return $ Left "Bad category parameter"
                Just n -> catch (do
                        conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                        let sort' = if sortParam == "" then ""
                                                else BC.concat [" order by ",sortParam," DESC"]
                        let pg = if isNothing page then " limit 10 offset 0"
                                            else 
                                         BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page)) - 1)*10 ]
                        let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, category_name, news_text  from news  join authors using (author_id) join users using (user_id) join categories using (category_id) where category_id = ? ", sort',pg]
                        rows <- query conn q [n]
                        close conn
                        return (Right $ NewsArray' rows)) $ \e -> do
                                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                                            logError hLogger err
                                                            return $ Left "Database error"


getNewsFilterByTitleFromDb  :: Handle -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByTitleFromDb  hLogger titleName page sortParam = do
    logInfo hLogger "Someone try get news list filtered by title"
    case titleName of
        Nothing -> do
            logError hLogger "No title parameter"
            return $ Left "No title parameter"
        Just bs -> catch (do
                    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                    let sort' = if sortParam == "" then ""
                                            else BC.concat [" order by ",sortParam," DESC"]
                    let pg = if isNothing page then " limit 10 offset 0"
                                        else 
                                     BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page)) - 1)*10 ]
                    let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, category_name, news_text from news join authors using (author_id) join users using (user_id) join categories using (category_id) where short_title = ? ",sort', pg]
                    rows <- query conn q [bs]
                    close conn
                    return (Right $ NewsArray' rows)) $ \e -> do
                                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                                            logError hLogger err
                                                            return $ Left "Database error"

getNewsFilterByAuthorNameFromDb  :: Handle -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByAuthorNameFromDb hLogger authorName page sortParam = do
    logInfo hLogger "Someone try get news list filtered by author's name"
    case authorName of
        Nothing -> do
            logError hLogger "No author_name parameter"
            return (Left "No author_name parameter")
        Just bs -> catch (do
                    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                    let sort' = if sortParam == "" then ""
                                            else BC.concat [" order by ",sortParam," DESC"]
                    let pg = if isNothing page then " limit 10 offset 0"
                                        else 
                                     BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page)) - 1)*10 ]
                    let q = toQuery $ BC.concat ["select * from (select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, category_name, news_text from news join authors using (author_id) join users using (user_id) join categories using (category_id)) as temp_t where author_name = ? ",sort', pg]
                    rows <- query conn q [bs]
                    close conn
                    return (Right $ NewsArray' rows)) $ \e -> do
                                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                                            logError hLogger err
                                                            return $ Left "Database error"

getNewsFilterByDateFromDb  :: Handle -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByDateFromDb  hLogger date page sortParam = do
    logInfo hLogger "Someone try get news list filtered by date"
    case date of
        Nothing -> do
            logError hLogger "No date parameter"
            return (Left "No date parameter")
        Just bs -> do
                    let date' = readByteStringToDay bs
                    case date' of
                        Nothing -> do
                            logError hLogger "Bad date parameter"
                            return (Left "Bad date parameter")
                        Just day -> catch (do
                                conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                                let sort' = if sortParam == "" then ""
                                            else BC.concat [" order by ",sortParam," DESC"]
                                let pg = if isNothing page then " limit 10 offset 0"
                                         else 
                                            BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page)) - 1)*10 ]
                                let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, category_name, news_text from news join authors using (author_id) join users using (user_id) join categories using (category_id) where date_creation = ? ",sort', pg]
                                rows <- query conn q [day]
                                close conn
                                return (Right $ NewsArray' rows)) $ \e -> do
                                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                                            logError hLogger err
                                                            return $ Left "Database error"

getNewsFilterByTagAllFromDb  :: Handle -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByTagAllFromDb  hLogger tag_lst page sortParam = do
    logInfo hLogger "Someone try get news list filtered by tag_all parameter"
    case tag_lst of
        Nothing -> do
            logError hLogger "No tag parameter"
            return $ Left "No tag parameter"
        Just bs -> do
            case readByteStringListInt (fromMaybe "" tag_lst) of
                Nothing -> do
                    logError hLogger "Bad tag parameter"
                    return ( Left "bad tag_all parameter")
                Just tag_list -> catch (do
                        conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                        let cn = BC.pack $ show $ Prelude.length tag_list - 1
                        let sort' = if sortParam == "" then ""
                                                else BC.concat [" order by ",sortParam," DESC"]
                        let pg = if isNothing page then " limit 10 offset 0"
                                            else 
                                         BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page)) - 1)*10 ]
                        let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, category_name, news_text  from news join authors using (author_id) join users using (user_id) join categories using (category_id) join news_tags using (news_id) where tag_id in ? group by news_id,author_name,category_name having count(*) > ",cn, " ",sort',pg]
                        rows <- query conn q (Only (In tag_list))
                        close conn
                        return (Right $ NewsArray' rows)) $ \e -> do
                                                                let err = E.decodeUtf8 $ sqlErrorMsg e
                                                                logError hLogger err
                                                                return $ Left "Database error"

getNewsFilterByContentFromDb  :: Handle -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByContentFromDb  hLogger content page sortParam = do
    logInfo hLogger "Someone try get news list filtered by content"
    case content of
        Nothing -> do
            logError hLogger "No content parameter"
            return $ Left "No content parameter"
        Just bs -> catch (do
                conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                let sort' = if sortParam == "" then ""
                                            else BC.concat [" order by ",sortParam," DESC"]
                let pg = if isNothing page then " limit 10 offset 0"
                                        else 
                                     BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page)) - 1)*10 ]
                let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, category_name, news_text from news join authors using (author_id) join users using (user_id) join categories using (category_id) where news_text like ? ",sort', pg]
                rows <- query conn q [BC.concat ["%",bs,"%"]]
                close conn
                return (Right $ NewsArray' rows)) $ \e -> do
                                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                                        logError hLogger err
                                                        return $ Left "Database error"

getNewsFilterByAfterDateFromDb  :: Handle -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByAfterDateFromDb hLogger  date page sortParam = do
    logInfo hLogger "Someone try get news list filtered by after_date parameter"
    case date of
        Nothing -> do
            logError hLogger "No date parameter"
            return $ Left "No date parameter"
        Just bs -> do
                    let date' = readByteStringToDay bs
                    case date' of
                        Nothing -> do
                            logError hLogger "Bad date parameter"
                            return $ Left "Bad date parameter"
                        Just day -> catch (do
                                conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                                let sort' = if sortParam == "" then ""
                                            else BC.concat [" order by ",sortParam," DESC"]
                                let pg = if isNothing page then " limit 10 offset 0"
                                         else 
                                            BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page)) - 1)*10 ]
                                let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, category_name, news_text from news join authors using (author_id) join users using (user_id) join categories using (category_id) where date_creation > ? ",sort', pg]
                                rows <- query conn q [day]
                                close conn
                                return (Right $ NewsArray' rows)) $ \e -> do
                                                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                                                        logError hLogger err
                                                                        return $ Left "Database error"

getNewsFilterByBeforeDateFromDb  :: Handle -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByBeforeDateFromDb hLogger date page sortParam = do
    logInfo hLogger "Someone try get news list filtered by before_date parameter"
    case date of
        Nothing -> do
            logError hLogger "No date parameter"
            return $ Left "No date parameter"
        Just bs -> do
                let date' = readByteStringToDay bs
                case date' of
                    Nothing -> do
                        logError hLogger "Bad date parameter"
                        return $ Left "Bad date parameter"
                    Just day -> catch (do
                        conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                        let sort' = if sortParam == "" then ""
                                            else BC.concat [" order by ",sortParam," DESC"]
                        let pg = if isNothing page then " limit 10 offset 0"
                                         else 
                                            BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page)) - 1)*10 ]
                        let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, category_name, news_text from news join authors using (author_id) join users using (user_id) join categories using (category_id) where date_creation < ? ",sort', pg]
                        rows <- query conn q [day]
                        close conn
                        return (Right $ NewsArray' rows)) $ \e -> do
                                                                let err = E.decodeUtf8 $ sqlErrorMsg e
                                                                logError hLogger err
                                                                return $ Left "Database error"

getNewsFilterByTagIdFromDb :: Handle -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByTagIdFromDb hLogger tag_id page sortParam = do
    logInfo hLogger "Someone try get news list filtered by tag"
    case tag_id of
        Nothing -> do
            logError hLogger "No tag parameter"
            return $ Left "No tag parameter"
        Just bs -> do
            case readByteStringToInt (fromMaybe "" tag_id) of
                Nothing -> do
                    logError hLogger "Bad tag parameter"
                    return $ Left "Bad tag parameter"
                Just n -> catch (do
                        conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                        let sort' = if sortParam == "" then ""
                                                else BC.concat [" order by ",sortParam," DESC"]
                        let pg = if isNothing page then " limit 10 offset 0"
                                            else 
                                         BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page)) - 1)*10 ]
                        let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, category_name, news_text  from news  join authors using (author_id) join users using (user_id) join categories using (category_id) join news_tags using (news_id) where tag_id = ?", sort',pg]
                        rows <- query conn q [n]
                        close conn
                        return (Right $ NewsArray' rows)) $ \e -> do
                                                                let err = E.decodeUtf8 $ sqlErrorMsg e
                                                                logError hLogger err
                                                                return $ Left "Database error"
-------------------------------------------------------------------------------------------
----------------Users database block-------------------------------------------------------


{-checkUser :: BC.ByteString -> BC.ByteString -> IO (Maybe Int)
checkUser login password = do
    catch ( do 
            conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
            [Only user_id] <- query conn "select user_id from users where user_password = crypt(?,user_password) and login = ? " (password,login) :: IO [Only (Maybe Int)]
            close conn
            return user_id)
        $ \e -> do
            let err = show (e :: IOException)
            return Nothing-}

generateToken :: BC.ByteString -> IO (BC.ByteString, UTCTime)
generateToken login = do
    now <- getCurrentTime
    let token = Prelude.filter (`Prelude.notElem` filt ) (show now)
    return (BC.concat [login,BC.pack token],now)
        where filt = " :.-UTC" :: String

authentication :: Handle -> BC.ByteString -> BC.ByteString -> IO (Either LBS.ByteString LBS.ByteString)
authentication hLogger login password = catch (do
                logInfo hLogger $ T.concat ["User with login ", E.decodeUtf8 login, " try logging"]
                conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                (token,now) <- generateToken login
                let q = "WITH loging_user as (select user_id from users where login = ? and user_password = (crypt(?,user_password))) update tokens set token = ?, creation_date = ? where user_id in (select user_id from loging_user)"
                --let q = "insert into tokens (user_id, token ,creation_date) values (?, ? ,?) ON CONFLICT(user_id) DO UPDATE set token = ?, creation_date= ?"
                n <- execute conn q (login,password,token,now)
                close conn
                if n > 0 then do
                    logInfo hLogger $ T.concat ["User with login ", E.decodeUtf8 login, " logged"]
                    return $ Right $ LBS.pack $ B.unpack token
                else do
                    logError hLogger $ T.concat ["User with login ", E.decodeUtf8 login, " cant logged"]
                    return $ Left "Wrong login or password"
                                                             ) $ \e -> do
                                                                let err = E.decodeUtf8 $ sqlErrorMsg e
                                                                logError hLogger err
                                                                return $ Left "Database error in authentication"
                                                        
{-tstAuth' :: IO (Either LBS.ByteString LBS.ByteString)
tstAuth' = authentication (Handle Debug printLog) "dahaku" "qwerty"-}



loadImageToDb :: Handle -> Image''' -> IO (Maybe Int)
loadImageToDb hLogger image = do
    if f_name'' image == "" || content_type'' image == "" || fromBinary (content'' image) == "" || BC.take 5 (content_type'' image) /= "image" then do
        logError hLogger "Bad image file"
        return Nothing
    else catch (do
        logInfo hLogger "Loading image to database"
        conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
        let q = "insert into images (image_name, image_b, content_type) values (?,?,?) returning image_id"
        rows <- returning conn q [image] :: IO [Only Int]
        close conn
        if Prelude.null rows then do
            Prelude.putStrLn "Image not loaded"
            return Nothing
        else do
            Prelude.putStrLn "image loaded"
            return (Just (fromOnly $ Prelude.head rows))) $ \e ->  do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        logError hLogger err
                                        return Nothing

createUserInDb :: Handle -> BC.ByteString  -> BC.ByteString  -> T.Text -> T.Text -> BC.ByteString  -> BC.ByteString  -> LBS.ByteString  -> IO (Either LBS.ByteString LBS.ByteString )
createUserInDb hLogger login password f_name l_name avatar_name avatar_contentType avatar= do
    if avatar_name == "" || avatar_contentType == "" || avatar == "" || BC.take 5 avatar_contentType /= "image" then do
        logError hLogger "Bad image file"
        return $ Left "Bad image file"
        
    else catch (do
        conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
        now <- getCurrentTime
        --let q = "with avatar_id as (insert into images (image_name,image_b,content_type) values (?,?,?) returning image_id) insert into users (first_name, last_name, avatar, login, user_password, creation_date, admin_mark) values (?,?,(select image_id from avatar_id),?,?,?,?)"

        let q = "with avatar_id as (insert into images (image_name,image_b,content_type) values (?,?,?) returning image_id) insert into users (first_name, last_name, avatar, login, user_password, creation_date, admin_mark) values (?,?,(select image_id from avatar_id),?,crypt(?,gen_salt('md5')),?,?)"
        n <- execute conn q (avatar_name,Binary avatar,avatar_contentType,f_name,l_name,login,password,now,False)
        if n > 0 then do
            logInfo hLogger $ T.concat ["User ", E.decodeUtf8 login, " registered"]
            firstToken hLogger login password
        else do
            logError hLogger "Registration failed"
            return $ Left "Registration failed") $ \e ->  do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        logError hLogger err
                                        return $ Left "Database error in registration"

deleteUserFromDb :: Handle -> ByteString -> IO (Either LBS.ByteString LBS.ByteString)
deleteUserFromDb hLogger login = catch (do
    logInfo hLogger $ T.concat ["Trying delete user ", E.decodeUtf8 login]
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    let q = "delete from users where login = ?"
    n <- execute conn q [login]
    close conn
    if n > 0 then do
        logInfo hLogger $ T.concat ["User ", E.decodeUtf8 login, " deleted"]
        return $ Right $ LBS.concat ["User ", LBS.fromStrict login, " deleted"]
    else return $ Left "User not exist") $ \e ->  do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        logError hLogger err
                                        return $ Left "Database error"


firstToken :: Handle -> BC.ByteString -> BC.ByteString ->  IO (Either LBS.ByteString LBS.ByteString)
firstToken hLogger login password = catch (do
                logInfo hLogger $ T.concat ["Generate first token for user ", E.decodeUtf8 login]
                conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                (token,now) <- generateToken login
                let q = "WITH loging_user as (select user_id from users where login = ? and user_password = (crypt(?,user_password))) insert into tokens (user_id,token,creation_date) values ((select user_id from loging_user),?,?)"
                --let q = "insert into tokens (user_id, token ,creation_date) values (?, ? ,?) ON CONFLICT(user_id) DO UPDATE set token = ?, creation_date= ?"
                n <- execute conn q (login,password,token,now)
                close conn
                if n > 0 then do
                    logInfo hLogger $ T.concat ["User with login ", E.decodeUtf8 login, " logged"]
                    return $ Right $ LBS.pack $ B.unpack token
                else do
                    logError hLogger $ T.concat ["User with login ", E.decodeUtf8 login, " cant logged"]
                    return $ Left "Wrong login or password"
                                                             ) $ \e -> do
                                                                let err = E.decodeUtf8 $ sqlErrorMsg e
                                                                logError hLogger err
                                                                return $ Left "Database error"