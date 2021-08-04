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
--import qualified System.Posix.Internals as TIO
--import qualified GHC.TypeLits as T
--import GHC.Stats (RTSStats(mutator_cpu_ns))
--import Network.HTTP.Types (QueryLike(toQuery))



{-getNewsFromDb :: Handle -> ByteString -> Maybe ByteString-> IO (Either LBS.ByteString NewsArray')
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
                                        return $ Left "Database error"-}

getNewsFromDb' :: Handle -> ByteString -> Maybe ByteString-> IO (Either LBS.ByteString NewsArray')
getNewsFromDb' hLogger sortParam pageParam = do
    logInfo hLogger "Someone try get news list"
    let sort' = if sortParam == "" then ""
                else BC.concat [" order by ",sortParam," DESC"]
    let pg = if isNothing pageParam then " limit 10 offset 0"
            else
                BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" pageParam)) - 1)*10 ]
    let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id), news_text  from news join authors using (author_id) join users using (user_id)", sort',pg]
    --rows <- query_ conn "select news_id, short_title, date_creation, author_id, category_id, news_text  from news" :: IO [GetNews']
    catch (do
        conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
        rows <- query_ conn q:: IO [GetNews']
        close conn
        return $ Right (NewsArray' rows)) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        logError hLogger "Database error in getting news list"
                                        return $ Left "Database error"
tstGetNews :: IO (Either LBS.ByteString NewsArray')
tstGetNews = getNewsFromDb' (Handle Debug printLog) "" Nothing

getNewsByIdFromDb :: Handle -> Maybe Int-> IO (Either LBS.ByteString  NewsArray')
getNewsByIdFromDb hLogger Nothing = do
    logError hLogger "Bad news_id parameter"
    return $ Left "Bad news_id parameter"
getNewsByIdFromDb hLogger (Just news_id) = catch ( do
        logInfo hLogger $ T.concat ["Someone try get news №", T.pack $ show news_id]
        conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
        let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id), news_text  from news join authors using (author_id) join users using (user_id) where news_id = ?"]
        rows <- query conn q [news_id]
        close conn
        logInfo hLogger $ T.concat ["Sending news with id = ", T.pack $ show news_id]
        return $ Right (NewsArray' rows) )$ \e -> do
                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                            logError hLogger  $ T.concat["Database error in getting news №", T.pack $ show news_id]
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
checkNews hLogger Nothing = do
    logError hLogger "Bad news id"
    return  (0, "Bad news id")
checkNews hLogger (Just news_id) = catch (do
        conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='arraays'"
        rows <- query conn "select news_id from news where news_id = ?" [news_id] :: IO [Only Int]
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
deleteCommentFromDb hLogger token Nothing = do
    logError hLogger "Bad comment id"
    return $ Left "Bad comment id"

deleteCommentFromDb hLogger token (Just comment_id) = do
        isAdmin <- checkAdmin hLogger token
        case isAdmin of
            (False,e) -> return $ Left e
            (True ,_) -> catch (do
                conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                execute conn "delete from users_comments where comment_id = ?" [comment_id]
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
getNewsFilterByTagInFromDb hLogger Nothing page = do
    logError hLogger "No tag parameter"
    return $ Left "No tag parameter"
getNewsFilterByTagInFromDb hLogger (Just tag_lst) page = do
    logInfo hLogger "Someone try get news list filtered by tag_in parameter"
    case readByteStringListInt tag_lst of
                Nothing -> do
                    logError hLogger "Bad tag parameter"
                    return $ Left "bad tag parameter"
                Just n -> catch (do
                        conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                        let pg = fromMaybe 1 (readByteStringToInt (fromMaybe "" page))
                        rows <- query conn "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id), news_text from news join authors using (author_id) join users using (user_id) join news_tags using (news_id) where tag_id in ? group by news_id,author_name order by 2 DESC " (Only (In n))
                        close conn
                        return (Right $ NewsArray' $ takePage pg rows)) $ \e -> do
                                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                                            logError hLogger err
                                                            return $ Left "Database error"

getNewsFilterByCategoryIdFromDb :: Handle -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByCategoryIdFromDb hLogger Nothing page sortParam = do
    logError hLogger "No category parameter"
    return $ Left "No category parameter"
getNewsFilterByCategoryIdFromDb hLogger (Just cat_id) page sortParam = do
    logInfo hLogger "Someone try get news list filtered by category id"
    case readByteStringToInt cat_id of
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
                        let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id), news_text  from news  join authors using (author_id) join users using (user_id) where category_id = ? ", sort',pg]
                        rows <- query conn q [n]
                        close conn
                        return (Right $ NewsArray' rows)) $ \e -> do
                                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                                            logError hLogger err
                                                            return $ Left "Database error"


getNewsFilterByTitleFromDb  :: Handle -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByTitleFromDb  hLogger Nothing page sortParam = do
    logError hLogger "No title parameter"
    return $ Left "No title parameter"
getNewsFilterByTitleFromDb  hLogger (Just titleName) page sortParam = catch (do
                    logInfo hLogger "Someone try get news list filtered by title"
                    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                    let sort' = if sortParam == "" then ""
                                            else BC.concat [" order by ",sortParam," DESC"]
                    let pg = if isNothing page then " limit 10 offset 0"
                                        else
                                     BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page)) - 1)*10 ]
                    let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id), news_text from news join authors using (author_id) join users using (user_id) where short_title = ? ",sort', pg]
                    rows <- query conn q [titleName]
                    close conn
                    return (Right $ NewsArray' rows)) $ \e -> do
                                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                                            logError hLogger err
                                                            return $ Left "Database error"

getNewsFilterByAuthorNameFromDb  :: Handle -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByAuthorNameFromDb hLogger Nothing page sortParam = do
        logError hLogger "No author_name parameter"
        return $ Left "No author_name parameter"
getNewsFilterByAuthorNameFromDb hLogger (Just authorName) page sortParam = catch (do
                    logInfo hLogger "Someone try get news list filtered by author's name"
                    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                    let sort' = if sortParam == "" then ""
                                            else BC.concat [" order by ",sortParam," DESC"]
                    let pg = if isNothing page then " limit 10 offset 0"
                                        else
                                     BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page)) - 1)*10 ]
                    let q = toQuery $ BC.concat ["select * from (select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id), news_text from news join authors using (author_id) join users using (user_id)) as temp_t where author_name = ? ",sort', pg]
                    rows <- query conn q [authorName]
                    close conn
                    return (Right $ NewsArray' rows)) $ \e -> do
                                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                                            logError hLogger err
                                                            return $ Left "Database error"

getNewsFilterByDateFromDb  :: Handle -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByDateFromDb  hLogger Nothing page sortParam = do
    logError hLogger "No date parameter"
    return $ Left "No date parameter"
getNewsFilterByDateFromDb  hLogger (Just date) page sortParam = do
    logInfo hLogger "Someone try get news list filtered by date"
    let date' = readByteStringToDay date
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
                                let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id), news_text from news join authors using (author_id) join users using (user_id) where date_creation = ? ",sort', pg]
                                rows <- query conn q [day]
                                close conn
                                return (Right $ NewsArray' rows)) $ \e -> do
                                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                                            logError hLogger err
                                                            return $ Left "Database error"

getNewsFilterByTagAllFromDb  :: Handle -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByTagAllFromDb  hLogger Nothing page sortParam = do
    logError hLogger "No tag parameter"
    return $ Left "No tag parameter"
getNewsFilterByTagAllFromDb  hLogger (Just tag_lst) page sortParam = do
    logInfo hLogger "Someone try get news list filtered by tag_all parameter"
    case readByteStringListInt tag_lst of
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
                        let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id), news_text  from news join authors using (author_id) join users using (user_id) join news_tags using (news_id) where tag_id in ? group by news_id,author_name having count(*) > ",cn, " ",sort',pg]
                        rows <- query conn q (Only (In tag_list))
                        close conn
                        return (Right $ NewsArray' rows)) $ \e -> do
                                                                let err = E.decodeUtf8 $ sqlErrorMsg e
                                                                logError hLogger err
                                                                return $ Left "Database error"

getNewsFilterByContentFromDb  :: Handle -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByContentFromDb  hLogger Nothing page sortParam = do
    logError hLogger "No content parameter"
    return $ Left "No content parameter"
getNewsFilterByContentFromDb  hLogger (Just content) page sortParam = catch (do
                logInfo hLogger "Someone try get news list filtered by content"
                conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                let sort' = if sortParam == "" then ""
                                            else BC.concat [" order by ",sortParam," DESC"]
                let pg = if isNothing page then " limit 10 offset 0"
                                        else
                                     BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page)) - 1)*10 ]
                let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id), news_text from news join authors using (author_id) join users using (user_id) where news_text like ? ",sort', pg]
                rows <- query conn q [BC.concat ["%",content,"%"]]
                close conn
                return (Right $ NewsArray' rows)) $ \e -> do
                                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                                        logError hLogger err
                                                        return $ Left "Database error"

getNewsFilterByAfterDateFromDb  :: Handle -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByAfterDateFromDb hLogger Nothing page sortParam = do
    logError hLogger "No date parameter"
    return $ Left "No date parameter"
getNewsFilterByAfterDateFromDb hLogger (Just date) page sortParam = do
    logInfo hLogger "Someone try get news list filtered by after_date parameter"
    let date' = readByteStringToDay date
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
                                let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id), news_text from news join authors using (author_id) join users using (user_id) where date_creation > ? ",sort', pg]
                                rows <- query conn q [day]
                                close conn
                                return (Right $ NewsArray' rows)) $ \e -> do
                                                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                                                        logError hLogger err
                                                                        return $ Left "Database error"

getNewsFilterByBeforeDateFromDb  :: Handle -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByBeforeDateFromDb hLogger Nothing page sortParam = do
    logError hLogger "No date parameter"
    return $ Left "No date parameter"
getNewsFilterByBeforeDateFromDb hLogger (Just date) page sortParam = do
                let date' = readByteStringToDay date
                case date' of
                    Nothing -> do
                        logError hLogger "Bad date parameter"
                        return $ Left "Bad date parameter"
                    Just day -> catch (do
                        logInfo hLogger "Someone try get news list filtered by before_date parameter"
                        conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                        let sort' = if sortParam == "" then ""
                                            else BC.concat [" order by ",sortParam," DESC"]
                        let pg = if isNothing page then " limit 10 offset 0"
                                         else
                                            BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page)) - 1)*10 ]
                        let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id), news_text from news join authors using (author_id) join users using (user_id) where date_creation < ? ",sort', pg]
                        rows <- query conn q [day]
                        close conn
                        return (Right $ NewsArray' rows)) $ \e -> do
                                                                let err = E.decodeUtf8 $ sqlErrorMsg e
                                                                logError hLogger err
                                                                return $ Left "Database error"

getNewsFilterByTagIdFromDb :: Handle -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByTagIdFromDb hLogger Nothing page sortParam = do
    logError hLogger "No tag parameter"
    return $ Left "No tag parameter"
getNewsFilterByTagIdFromDb hLogger (Just tag_id) page sortParam = do
    logInfo hLogger "Someone try get news list filtered by tag"
    case readByteStringToInt  tag_id of
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
                        let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id), news_text  from news  join authors using (author_id) join users using (user_id) join news_tags using (news_id) where tag_id = ?", sort',pg]
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



{-loadImageToDb :: Handle -> Image''' -> IO (Maybe Int)
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
                                        return Nothing-}

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

profileOnDb :: Handle -> T.Text -> IO (Either LBS.ByteString Profile)
profileOnDb hLogger token = catch (do
    logInfo hLogger "Sending profile information"
    let q = "select first_name, last_name, avatar from users join tokens using (user_id) where  token = ? and (now()- tokens.creation_date) < make_interval(secs => ?)"
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    rows <- query conn q (TokenProfile token 86400) :: IO [Profile]
    close conn
    if Prelude.null rows then do
        logError hLogger "Bad token"
        return $ Left "Bad token"
    else do
        logInfo hLogger "Profile information sended"
        return $ Right $ Prelude.head rows) $  \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ", T.pack $ show errStateInt]
                                        case errStateInt of
                                            23505 -> return $ Left "Category already exist"
                                            _ -> return $ Left "Database error"

---------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------Categories block---------------------------------------------------------------------------------

getCategoriesListFromDb :: Handle -> Maybe BC.ByteString -> IO (Either LBS.ByteString ListOfCategories)
getCategoriesListFromDb hLogger pageParam = catch (do
    logInfo hLogger "Someone try get list of categories"
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    let pg = if isNothing pageParam then " limit 10 offset 0"
                else
                    BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" pageParam)) - 1)*10 ]
    let q = toQuery $ BC.concat ["select category_name from categories",pg]
    rows <- query_ conn q :: IO [Category']
    close conn
    return $ Right (ListOfCategories rows)) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        logError hLogger err
                                        return $ Left "Database error"


createCategoryOnDb :: Handle -> Maybe T.Text -> Maybe T.Text -> IO (Either LBS.ByteString LBS.ByteString)
createCategoryOnDb hLogger Nothing _ = do
    logError hLogger "No category_name field"
    return $ Left "No category_name field"

createCategoryOnDb hLogger _ Nothing = do
    logError hLogger "No maternal_category_name field"
    return $ Left "No maternal_category_name field"

createCategoryOnDb hLogger (Just category_name) (Just maternal_name) = catch (do
    logInfo hLogger "Creating new category"
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    let q = "insert into categories (category_name, maternal_category) values (?,?) returning category_id"
    if maternal_name == "" then do
        logInfo hLogger "Maternal category is null"
        --let m = Nothing :: Maybe T.Text
        rows <- returning conn q [(category_name, Nothing :: Maybe T.Text)] :: IO [Only Int]
        close conn
        logInfo hLogger "Category created"
        --let c_idd = LBS.fromStrict $ BC.pack $ show (fromOnly $ Prelude.head rows)
        return $ Right $ LBS.fromStrict $ BC.pack $ show (fromOnly $ Prelude.head rows)
    else do
    --conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
        let check_maternal = "select category_id from categories where category_name = ?"
        c_id <- query conn check_maternal [maternal_name] :: IO [Only Int]
        if Prelude.null c_id then do
            logError hLogger "Maternal category not exist"
            return $ Left "Maternal category not exist"
            else do
                rows <- returning conn q [(category_name, fromOnly $ Prelude.head c_id)] :: IO [Only Int]
                close conn
                logInfo hLogger "Category created"
                return $ Right $ LBS.fromStrict $ BC.pack $ show (fromOnly $ Prelude.head rows)) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger err
                                        case errStateInt of
                                            23505 -> return $ Left "Category already exist"
                                            _ -> return $ Left "Database error"

deleteCategoryFromDb :: Handle -> Maybe T.Text -> IO (Either LBS.ByteString LBS.ByteString)
deleteCategoryFromDb hLogger Nothing = do
    logError hLogger "No category_name parametr"
    return $ Left "No category_name parametr"
deleteCategoryFromDb hLogger (Just categoryName) = catch (do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    n <- execute conn "delete from categories where category_name = ?" [categoryName]
    close conn
    if n > 0 then do
        logInfo hLogger $ T.concat ["Category ", categoryName, " deleted"]
        return $ Right "Category deleted"
    else do
        logError hLogger $ T.concat ["Category ", categoryName, " not exist"]
        return $ Right "Category not exist"
                                ) $  \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        logError hLogger err
                                        return $ Left "Database error"

editCategoryOnDb :: Handle -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> IO (Either LBS.ByteString LBS.ByteString )
editCategoryOnDb hLogger Nothing _ _ = do
    logError hLogger "No old_name parametr"
    return $ Left "No old_name parametr"
editCategoryOnDb hLogger (Just old_name) (Just new_name) (Just "") = catch (do
    logInfo hLogger $ T.concat ["Update category_name parameter on category ", old_name]
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    let q = "update categories set category_name = ? where category_name = ?"
    n <- execute conn q (new_name,old_name)
    close conn
    if n > 0 then do
        logInfo hLogger $ T.concat ["Category ", old_name, " edited"]
        return $ Right "Category edited"
    else do
        logError hLogger $ T.concat ["Category ", old_name, " not exist"]
        return $ Left "Category not exist") $  \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, T.pack $ show errStateInt]
                                        case errStateInt of
                                            23505 -> return $ Left "Category already exist"
                                            _ -> return $ Left "Database error"

editCategoryOnDb hLogger (Just old_name) (Just "") (Just new_maternal) = catch (do
    logInfo hLogger $ T.concat ["Update maternal_category parameter on category ", old_name]
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    let check_maternal = "select category_id from categories where category_name = ?"
    m_id <- query conn check_maternal [new_maternal] :: IO [Only Int]
    if Prelude.null m_id then do
        close conn
        logError hLogger $ T.concat ["Maternal category ", new_maternal, " not exist" ]
        return $ Left "Maternal category not exist"
    else do
        let q = "update categories set maternal_category = ? where category_name = ?"
        n <- execute conn q (fromOnly $ Prelude.head m_id, old_name)
        close conn
        if n > 0 then do
            logInfo hLogger $ T.concat ["Category ", old_name, " edited"]
            return $ Right "Category edited"
        else do
            logError hLogger $ T.concat ["Category ", old_name, " not exist"]
            return $ Left "Category not exist") $  \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        logError hLogger err
                                        return $ Left "Database error"
editCategoryOnDb hLogger (Just old_name) (Just new_name) (Just new_maternal) = catch (do
    logInfo hLogger $ T.concat ["Update all parameters on category ", old_name]
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    let check_maternal = "select category_id from categories where category_name = ?"
    m_id <- query conn check_maternal [new_maternal] :: IO [Only Int]
    if Prelude.null m_id then do
        close conn
        logError hLogger $ T.concat ["Maternal category ", new_maternal, " not exist" ]
        return $ Left "Maternal category not exist"
    else do
        let q = "update categories set category_name = ?, maternal_category = ? where category_name = ?"
        n <- execute conn q (new_name,fromOnly $ Prelude.head m_id, old_name)
        close conn
        if n > 0 then do
            logInfo hLogger $ T.concat ["Category ", old_name, " edited"]
            return $ Right "Category edited"
        else do
            logError hLogger $ T.concat ["Category ", old_name, " not exist"]
            return $ Left "Category not exist") $  \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, T.pack $ show errStateInt]
                                        case errStateInt of
                                            23505 -> return $ Left "Category already exist"
                                            _ -> return $ Left "Database error"
editCategoryOnDb hLogger _ _ _ = do
    logError hLogger "No update parameters"
    return $ Left "No update parameters"



tstCategory :: IO (Either LBS.ByteString LBS.ByteString)
tstCategory = deleteCategoryFromDb (Handle Debug printLog) (Just "селебрети")




--------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------Drafts Block-----------------------------------------------------------------------------------

checkAuthor :: Handle -> Maybe T.Text -> IO (Either LBS.ByteString Int)
checkAuthor hLogger Nothing = do
    logError hLogger "No token parameter"
    return $ Left "No token parameter"
checkAuthor hLogger (Just token) = catch (do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    --TIO.putStrLn token
    rows <- query conn "select author_id from authors join users using (user_id) join tokens using (user_id) where token = ?" [token]:: IO [Only Int]
    if  Prelude.null rows then do
        logError hLogger "User try get drafts without author's rights"
        return $ Left "You not author"
    else
        return $ Right (fromOnly $ Prelude.head rows)) $  \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"

{-getDraftsByAuthorId :: Int -> IO DraftArray 
getDraftsByAuthorId a_id = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    --let q = "select short_title, date_of_changes, category_id, draft_text, main_image from drafts where author_id = ?"
    let q = "select short_title, date_of_changes, category_id, draft_text, main_image, array_agg(image_id) from drafts join drafts_images using (draft_id) where author_id = ? group by draft_id"
    rows <- query conn q [a_id] :: IO [Draft']
    return $ DraftArray rows-}
getDraftsByAuthorToken :: Handle -> T.Text -> IO (Either LBS.ByteString DraftArray)
getDraftsByAuthorToken hLogger token = catch (do
    logInfo hLogger "Someone try get drafts list"
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    --let q = "select short_title, date_of_changes, category_id, draft_text, main_image from drafts where author_id = ?"
    let q = "select short_title, date_of_changes, category_id, draft_text, main_image, array_agg(image_id) from drafts join drafts_images using (draft_id) join authors using (author_id) join tokens using (user_id) where token = ? and (now() - tokens.creation_date) < make_interval(secs => ?) group by draft_id"
    rows <- query conn q  (TokenProfile token 86400) :: IO [Draft']
    close conn
    if Prelude.null rows then do
        logError hLogger "User use bad token or haven't drafts"
        return $ Left "You are not author or don't have drafts "
    else return $ Right $ DraftArray rows) $  \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"


tstDrafts :: IO (Either LBS.ByteString DraftArray)
tstDrafts = getDraftsByAuthorToken (Handle Debug printLog) "dahaku202108020903181300795"



createDraftOnDb' :: Handle -> Maybe BC.ByteString  -> Maybe BC.ByteString -> Maybe BC.ByteString -> Maybe BC.ByteString -> Maybe BC.ByteString -> Maybe Image''' -> Maybe [Image'''] -> IO (Either LBS.ByteString Int)
createDraftOnDb' hLogger Nothing category tags short_title text main_image images_list = do
    logError hLogger "No token param"
    return $ Left "No token param"
createDraftOnDb' hLogger _ Nothing tags short_title text main_image images_list = do
    logError hLogger "No category field"
    return $ Left "No category field"
createDraftOnDb' hLogger _ _ Nothing short_title text main_image images_list = do
    logError hLogger "No tags field"
    return $ Left "No tags field"
createDraftOnDb' hLogger _ _ _ Nothing text main_image images_list = do
    logError hLogger "No short_title field"
    return $ Left "No short_title field"
createDraftOnDb' hLogger _ _ _ _ Nothing main_image images_list = do
    logError hLogger "No text field"
    return $ Left "No text field"
createDraftOnDb' hLogger (Just token) (Just category) (Just tags) (Just short_title) (Just text) main_image images_list = do
    logInfo hLogger "Someone try add new draft"
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    now <- getCurrentTime
    --let q = "with get_a as (select author_id from authors join tokens using (user_id) where token = ?), get_c as (select category_id from categories where category_name = ?) insert into drafts (author_id,short_title,date_of_changes,category_id,draft_text) values ((select author_id from get_a),?,?,(select category_id from get_c),?) returning draft_id"
    let q =  toQuery $ BC.concat ["with get_a as (select author_id from authors join tokens using (user_id) where token = '",
                    token,
                    "'), get_c as (select category_id from categories where category_name = '",
                    category,
                    "') insert into drafts (author_id,short_title,date_of_changes,category_id,draft_text) values ((select author_id from get_a),'",
                    short_title,
                    "',now(),(select category_id from get_c),'",
                    text,
                    "') returning draft_id"]
--"with get_a as (select author_id from authors join tokens using (user_id) where token = 'dahaku202108020903181300795'), get_c as (select category_id from categories where category_name = 'наука') insert into drafts (author_id,short_title,date_of_changes,category_id,draft_text) values ((select author_id from get_a),'tst2',now(),(select category_id from get_c),'tsttext') returning draft_id"
    d_rows <- query_  conn q  :: IO [Only Int]
    if Prelude.null d_rows then do
        close conn
        logError hLogger "Draft not created"
        return $ Left "Draft not created"
    else do
        --loadImagesForDraft'
        result <- loadImagesForDraft' hLogger (fromOnly $ Prelude.head d_rows) main_image images_list
        close conn
        case result of
            Left bs -> do
                logError hLogger "Images not loaded"
                -- delete draft
                return $ Left bs
            Right bs -> do
                logInfo hLogger "Images  loaded"
                -- delete draft
                return $ Right $ fromOnly $ Prelude.head d_rows

        {---close conn
        images_ids <- loadImagesForDraft hLogger main_image images_list
        case images_ids of
            Left bs -> do
                logError hLogger "Images not loaded"
                --deleteDraft draft_id
                return $ Left bs
            Right (0,[]) -> do
                logInfo hLogger "Draft created"
                return $ Right "Draft created"
            Right (0,ns) -> do
                --createConnDraftImage hLogger (fromOnly $ Prelude.head d_rows) ns
            Right-}


    
        {-let draft_id = fromOnly $ Prelude.head d_rows
        logInfo hLogger "Draft created"
        imagesIds <- loadImagesToDb hLogger images_list
        case imagesIds of
            Left bs -> do
                close conn
                return $ Left bs
            Right i_ids -> do 
                let main_id = lookup (E.decodeUtf8 $ f_name'' main_image) i_ids-}
                --return $ Right 0


        --close conn
        --return $ Right draft_id

tstCDrfats :: IO (Either LBS.ByteString Int)
tstCDrfats = createDraftOnDb' (Handle Debug printLog) (Just "dahaku202108020903181300795") (Just "наука") (Just "") (Just "test_serv") (Just "test text") (Just $ Image''' "5" "6" (Binary "7")) (Just [])




{-loadImageToDb :: Handle -> Image''' -> IO (Either LBS.ByteString Int)
loadImageToDb hLogger image = do
    if f_name'' image == "" || content_type'' image == "" || fromBinary (content'' image) == "" || BC.take 5 (content_type'' image) /= "image" then do
        logError hLogger "Bad image file"
        return $ Left "Bad image file"
    else catch (do
        logInfo hLogger "Loading image to database"
        conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
        let q = "insert into images (image_name, image_b, content_type) values (?,?,?) returning image_id"
        rows <- returning conn q [image] :: IO [Only Int]
        close conn
        if Prelude.null rows then do
            logError hLogger "Image not loaded"
            return $ Left "Image not loaded"
        else do
            logInfo hLogger "Image loaded"
            return $ Right $ fromOnly $ Prelude.head rows) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"-}

{-loadImagesToDb :: Handle -> [Image'''] -> IO (Either LBS.ByteString [Int])
loadImagesToDb hLogger [] = do
    logWarning hLogger "No images"
    return $ Right []
loadImagesToDb hLogger images_list = do
    if Prelude.elem "" (f_name'' <$> images_list)  || Prelude.elem "" (content_type'' <$> images_list ) || Prelude.elem "" (fromBinary . content'' <$> images_list) || Prelude.any (/= "image") (BC.take 5 .content_type'' <$> images_list)  then do
        logError hLogger "Bad image file"
        return $ Left "Bad image file"
    else catch (do
        logInfo hLogger "Loading image to database"
        conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
        let q = "insert into images (image_name, image_b, content_type) values (?,?,?) returning image_id"
        rows <- returning conn q images_list :: IO [Only Int]
        close conn
        if Prelude.null rows then do
            logError hLogger "Image not loaded"
            return $ Left "Image not loaded"
        else do
            logInfo hLogger "Image loaded"
            return $ Right  $ fromOnly <$> rows) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"
loadImagesForDraft :: Handle -> Maybe Image''' -> Maybe [Image'''] -> IO (Either LBS.ByteString (Int,[Int]))
loadImagesForDraft hLogger Nothing (Just image) = do
    result <- loadImagesToDb hLogger image
    case result of
      Left bs -> return $ Left bs
      Right ns -> return $ Right (0,ns)
loadImagesForDraft hLogger (Just image) Nothing = do
    result <- loadImagesToDb hLogger [image]
    case result of
      Left bs -> return $ Left bs
      Right ns -> return $ Right (Prelude.head ns,[])
loadImagesForDraft hLogger (Just image) (Just images) = do
    result <- loadImagesToDb hLogger (image:images)
    case result of
      Left bs -> return $ Left bs
      Right ns -> return $ Right (Prelude.head ns,Prelude.tail ns)
loadImagesForDraft hLogger _ _ = return $ Right (0,[])


createConnDraftImage :: Handle ->  Int -> [Int] -> IO (Either LBS.ByteString Int )
createConnDraftImage hLogger draft_id images_ids = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    let q = "insert into draft_images (draft_id,image_id) values (?,?)"
    n <- executeMany conn q (toPairList draft_id images_ids)
    if fromIntegral n /= Prelude.length images_ids then do
        logError hLogger "Connections not added"
        return $ Left "Connections not added"
    else do
        logInfo hLogger "Connections added"
        return $ Right draft_id


toPairList :: a -> [b] -> [(a, b)]
toPairList n  = Prelude.map (\ x -> (n, x))



megaQ :: Handle -> [Image''''] -> IO (Either LBS.ByteString Int)
megaQ hLogger iml = do
    --let im = Image'''' "123" "123" (Binary "123") 15
    --let imm = [] :: [Image'''']
    --let i = Image''' "5" "6" (Binary "7") 
    let q ="with add_image as (insert into images (image_name,content_type, image_b) values (?,?,?) returning image_id) insert into drafts_images (draft_id, image_id) values ( ? , (select image_id from add_image)) returning image_id"
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    n <- returning conn q iml :: IO [Only Int]
    if Prelude.null n then
        return $ Left ""-}
    --else
     --   return $ Right $ fromOnly $ Prelude.head n
loadImageAndCreateDraftConnectionOn :: Handle -> Int-> [Image'''] -> IO (Either LBS.ByteString Int)
loadImageAndCreateDraftConnectionOn hLogger d_id iml = catch (do
    --let im = Image'''' "123" "123" (Binary "123") 15
    --let imm = [] :: [Image'''']
    --let i = Image''' "5" "6" (Binary "7") 
    let q = toQuery $ BC.concat["with add_image as (insert into images (image_name,content_type, image_b) values (?,?,?) returning image_id) insert into drafts_images (draft_id, image_id) values ( ",BC.pack $ show d_id," , (select image_id from add_image)) returning image_id"]
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    n <- returning conn q iml :: IO [Only Int]
    if Prelude.null n then
        return $ Left ""
    else
        return $ Right $ fromOnly $ Prelude.head n) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"

tstMegaQ :: IO (Either LBS.ByteString Int)
tstMegaQ = loadImageAndCreateDraftConnectionOn (Handle Debug printLog) 16 [(Image''' "5" "6" (Binary "7"))]

{-toImage'''' :: Int -> Image''' -> Image''''
toImage'''' d_id image = Image'''' (f_name'' image) (content_type'' image) (content'' image) d_id-}

loadImagesForDraft' :: Handle -> Int -> Maybe Image''' -> Maybe [Image'''] -> IO (Either LBS.ByteString LBS.ByteString)
loadImagesForDraft' hLogger draft_id Nothing (Just image) = catch (do
    result <- loadImageAndCreateDraftConnectionOn hLogger draft_id image
    case result of
      Left bs -> return $ Left bs
      Right n -> return $ Right "Draft created") $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"
loadImagesForDraft' hLogger draft_id Nothing Nothing = do
    return $ Right "Draft created"
loadImagesForDraft' hLogger draft_id (Just i) Nothing = catch (do
    result <- loadImageAndCreateDraftConnectionOn hLogger draft_id [i]
    case result of
      Left bs -> return $ Left bs
      Right n -> do
            conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
            let q = "update drafts set main_image = ? where draft_id = ?"
            k <- execute conn q (n,draft_id)
            if k > 0 then do
                return $ Right "Draft created"
            else return $ Left "Draft not created") $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"
loadImagesForDraft' hLogger draft_id (Just i) (Just ii) = catch (do
    result <- loadImageAndCreateDraftConnectionOn hLogger draft_id (i:ii)
    case result of
      Left bs -> return $ Left bs
      Right n -> do
            conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
            let q = "update  drafts set main_image = ? where draft_id = ?"
            k <- execute conn q (n,draft_id)
            if k > 0 then do
                return $ Right "Draft created"
            else return $ Left "Draft not created") $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"

deleteDraftFromDb :: Handle -> Int -> Maybe ByteString -> IO (Either LBS.ByteString LBS.ByteString)
deleteDraftFromDb hLogger _ Nothing = do
    logError hLogger "No draft_id parameter"
    return $ Left "No draft_id parameter"
deleteDraftFromDb hLogger author_id (Just draft_id) = catch (do
    let draft_id_int = readByteStringToInt draft_id
    case draft_id_int of
        Nothing -> do
            logError hLogger "Bad draft_id parameter"
            return $ Left "Bad draft_id parameter"
        Just n -> do
            conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
            let q = "delete from drafts where draft_id = ? and author_id = ?"
            ne <- execute conn q (n,author_id)
            close conn
            if ne > 0 then do
                logInfo hLogger $ T.concat ["Draft ", T.pack $ show n, " deleted"]
                return $ Right "Draft deleted"
            else do
                logError hLogger "Deleting not existing draft"
                return $ Left "Draft not exist" ) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"


getDraftByIdFromDb :: Handle -> Int -> Int -> IO (Either LBS.ByteString Draft')
getDraftByIdFromDb hLogger author_id draft_id = catch (do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    --let q = "select short_title, date_of_changes, category_id, draft_text, main_image, array_agg(image_id) from drafts join drafts_images using (draft_id) where author_id = ? and draft_id = ? group by draft_id"
    let q = "with image_arr as (select array_agg(image_id) from drafts_images where draft_id = ?) select short_title, date_of_changes, category_id, draft_text, main_image, (select * from image_arr) from drafts where author_id = ? and draft_id = ?"
    rows <- query conn q (draft_id,author_id,draft_id) :: IO [Draft']
    close conn
    if Prelude.null rows then do
            logError hLogger "Wrong draft id or draft not exist"
            return $ Left "Wrong draft id or draft not exist"
    else do
        logInfo hLogger "Sending draft to user"
        return $ Right $ Prelude.head rows) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"
tstGetD :: IO (Either LBS.ByteString Draft')
tstGetD = getDraftByIdFromDb (Handle Debug printLog) 12 22

getDraftIdsByAuthor :: Handle -> Int -> IO (Either LBS.ByteString  [Int])
getDraftIdsByAuthor hLogger author_id = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    let q = "select draft_id from drafts where author_id = ?"
    rows <- query conn q [author_id] :: IO [Only Int]
    if Prelude.null rows then
        return $ Left "No drafts"
    else
        return $ Right (fromOnly <$> rows)


--updateDraftInDb'hLogger token category tags short_title text main_image images_list draft_id
updateDraftInDb' :: Handle -> Maybe BC.ByteString -> Maybe BC.ByteString -> Maybe BC.ByteString -> Maybe BC.ByteString -> Maybe BC.ByteString -> Maybe Image''' -> Maybe [Image'''] -> Int -> Int -> IO (Either LBS.ByteString LBS.ByteString)
updateDraftInDb' hLogger Nothing _ _ _ _ _ _ _ _ = do
    logError hLogger "No token param"
    return $ Left "No token param"
updateDraftInDb' hLogger _ Nothing _ _ _ _ _ _ _ = do
    logError hLogger "No category field"
    return $ Left "No category field"
updateDraftInDb' hLogger _ _ Nothing _ _ _ _ _ _ = do
    logError hLogger "No tags field"
    return $ Left "No tags field"
updateDraftInDb' hLogger _ _ _ Nothing _ _ _ _ _ = do
    logError hLogger "No short_title field"
    return $ Left "No short_title field"
updateDraftInDb' hLogger _ _ _ _ Nothing _ _ _ _ = do
    logError hLogger "No text field"
    return $ Left "No text field"
updateDraftInDb' hLogger (Just token) (Just category) (Just tags) (Just short_title) (Just text) main_image images_list draft_id author_id  = catch (do
    new_rows <- createDraftOnDb' hLogger (Just token) (Just category) (Just tags) (Just short_title) (Just text) main_image images_list
    case new_rows of
        Left bs -> do
            logError hLogger "Draft not updated"
            return $ Left "Draft not updated"
        Right n -> do
            let q_update_id = "with dr_i as (delete from drafts where draft_id = ? and author_id = ? returning draft_id) update drafts set draft_id = (select draft_id from dr_i) where draft_id = ?"
            conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
            nu <- execute conn q_update_id (draft_id,author_id,n)
            close conn
            if nu > 0 then do
                logInfo hLogger $ T.concat ["Draft ",T.pack $ show draft_id, " updated"]
                return $ Right "Draft updated"
            else do
                --c <-deleteDraftFromDb hLogger author_id (Just $ BC.pack $ show n)
                logInfo hLogger $ T.concat ["Draft ",T.pack $ show draft_id, "not updated"]
                return $ Left "Draft not updated") $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"




publicNewsOnDb :: Handle -> Int -> Int -> IO (Either LBS.ByteString Int)
publicNewsOnDb hLogger author_id draft_id = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    let q = toQuery $ BC.concat ["with draft_s as (delete from drafts where draft_id = ? returning short_title,author_id,category_id,draft_text,main_image))",
                                "insert into news (short_title,author_id,category_id,news_text,main_image,date_creation)", 
                                "values ((select short_title from draft_s),",
                                "(select author_id from draft_s),",
                                "(select category_id from draft_s),",
                                "(select draft_text from draft_s),",
                                "(select main_image from draft_s),",
                                "?) returning news_id"]
    now <- getCurrentTime 
    n_id <- query conn q (draft_id,author_id,now) :: IO [Only Int]
    if Prelude.null n_id then do
        close conn
        return $ Left "News not published"
    else do
        check <- createConnectionImagesNews (fromOnly $ Prelude.head n_id) draft_id
        case check of
            Left bs -> do
                logError hLogger "News not published"
                return $ Left "News not published"
            Right bs -> do
                logInfo hLogger "News published"
                return $ Right (fromOnly $ Prelude.head n_id)


createConnectionImagesNews :: Int -> Int -> IO (Either LBS.ByteString LBS.ByteString)
createConnectionImagesNews news_id draft_id = do
    let q = "with d_s as (select image_id from drafts_images where draft_id = ?) insert into news_image (news_id,image_id) values (?, (select image_id from d_s))"
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    n <- execute conn q (draft_id,news_id)
    if n > 0 then do
        return $ Right ""
    else
        return $ Left ""
