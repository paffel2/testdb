{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Databaseoperations where
import Database.PostgreSQL.Simple
--import Database.PostgreSQL.Simple.FromField
--import Control.Monad
--import Control.Applicative
import Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
--import qualified Data.Text.IO as TIO
import Types
    ( Comment(Comment),
      CommentArray(CommentArray),
      Draft',
      Draft'',
      DraftArray(DraftArray),
      GetNews',
      Image''',
      ListOfCategories(ListOfCategories),
      NewsArray'(NewsArray'),
      Profile,
      TagsList(TagsList),
      TokenLifeTime,
      TokenProfile(TokenProfile),
      GetNews'' )
import Data.Time
import Control.Exception
--import Database.PostgreSQL.Simple.Internal
import qualified Data.Text.Encoding as E
import Data.Maybe
--import Text.Read
--import Data.String
import HelpFunction
--import Database.PostgreSQL.Simple.Types
import Logger
import Data.Pool
import PostgreSqlWithPool
--import qualified Data.Text.IO as TIO
--import Data.Aeson.Types (Options(rejectUnknownFields))




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
                                        logError hLogger err --"Database error in getting news list"
                                        return $ Left "Database error"
getNewsFromDb :: Handle -> Pool Connection -> ByteString -> Maybe ByteString-> IO (Either LBS.ByteString NewsArray')
getNewsFromDb hLogger pool sortParam pageParam =
    catch (do
        logInfo hLogger "Someone try get news list"
        rows <- query_WithPool pool q
        return $ Right (NewsArray' rows)) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        logError hLogger err --"Database error in getting news list"
                                        return $ Left "Database error"
    where
        sort' = if sortParam == "" then ""
                else BC.concat [" order by ",sortParam," DESC"]
        pg = if isNothing pageParam then " limit 10 offset 0"
            else
                BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" pageParam)) - 1)*10 ]
        q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id), news_text  from news join authors using (author_id) join users using (user_id)", sort',pg]




{-tstGetNews :: IO (Either LBS.ByteString NewsArray')
tstGetNews = do
    pool <- tstPool 
    getNewsFromDb (Handle Debug printLog) pool "" Nothing-}

tstPool :: IO (Pool Connection)
tstPool = createPool (connectPostgreSQL db_address) close 1 5 10
    where db_address = "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"



{-getNewsByIdFromDb' :: Handle -> Maybe Int-> IO (Either LBS.ByteString  NewsArray')
getNewsByIdFromDb' hLogger Nothing = do
    logError hLogger "Bad news_id parameter"
    return $ Left "Bad news_id parameter"
getNewsByIdFromDb' hLogger (Just news'_id) = catch ( do
        logInfo hLogger $ T.concat ["Someone try get news №", T.pack $ show news'_id]
        conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
        let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id), news_text  from news join authors using (author_id) join users using (user_id) where news_id = ?"]
        rows <- query conn q [news'_id]
        close conn
        if Prelude.null rows then do
            logError hLogger $ T.concat ["News №", T.pack $ show news'_id, " not exist"]
            return $ Left "News not exist"
        else do
            logInfo hLogger $ T.concat ["Sending news with id = ", T.pack $ show news'_id]
            return $ Right (NewsArray' rows) )$ \e -> do
                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                            logError hLogger err 
                                            return $ Left "Database error"-}

{-getNewsByIdFromDb :: Handle -> Pool Connection ->  Maybe Int -> IO (Either LBS.ByteString NewsArray')
getNewsByIdFromDb hLogger _ Nothing = do
    logError hLogger "Bad news_id parameter"
    return $ Left "Bad news_id parameter"
getNewsByIdFromDb hLogger pool (Just news'_id) = catch ( do
        logInfo hLogger $ T.concat ["Someone try get news №", T.pack $ show news'_id]
        rows <- queryWithPool pool q [news'_id]
        if Prelude.null rows then do
            logError hLogger $ T.concat ["News №", T.pack $ show news'_id, " not exist"]
            return $ Left "News not exist"
        else do
            logInfo hLogger $ T.concat ["Sending news with id = ", T.pack $ show news'_id]
            return $ Right (NewsArray' rows) )$ \e -> do
                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                            logError hLogger err
                                            return $ Left "Database error"
        where   q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name,",
                                       " take_categories_list(category_id), news_text ",
                                       "from news join authors using (author_id) join users using (user_id) where news_id = ?"]
                q_n = toQuery $ BC.concat ["with image_arr as (select array_agg(image_id) from news_images where news_id = ",BC.pack $ show news'_id,"), ",
                                  " tags_arr as (select array_agg(tag_name) from news_tags join tags using (tag_id) where news_id = ",BC.pack $ show news'_id,") " ,
                                  "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id), news_text, main_image, ",
                                  "(select * from image_arr) as other_images, (select * from  tags_arr) as tags from news where news_id = ",BC.pack $ show news'_id]-}
getNewsByIdFromDb :: Handle -> Pool Connection ->  Maybe Int -> IO (Either LBS.ByteString GetNews'')
getNewsByIdFromDb hLogger _ Nothing = do
    logError hLogger "Bad news_id parameter"
    return $ Left "Bad news_id parameter"
getNewsByIdFromDb hLogger pool (Just news'_id) = catch ( do
        logInfo hLogger $ T.concat ["Someone try get news №", T.pack $ show news'_id]
        rows <- query_WithPool pool q_n --[news'_id]
        if Prelude.null rows then do
            logError hLogger $ T.concat ["News №", T.pack $ show news'_id, " not exist"]
            return $ Left "News not exist"
        else do
            logInfo hLogger $ T.concat ["Sending news with id = ", T.pack $ show news'_id]
            return $ Right $ Prelude.head rows )$ \e -> do
                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                            logError hLogger err
                                            return $ Left "Database error"
        where
            q_n = toQuery $ BC.concat ["with image_arr as (select array_agg(image_id) from news_images where news_id = ",BC.pack $ show news'_id,"), ",
                                       " tags_arr as (select array_agg(tag_name) from news_tags join tags using (tag_id) where news_id = ",BC.pack $ show news'_id,") " ,
                                       "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id), news_text, main_image, ",
                                       "(select * from image_arr) as other_images, (select * from  tags_arr) as tags from news join authors using (author_id) join users using (user_id) where news_id = ",BC.pack $ show news'_id]

            {-q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name,",
                                       " take_categories_list(category_id), news_text ",
                                       "from news join authors using (author_id) join users using (user_id) where news_id = ?"]-}
{-getDraftByIdFromDb :: Handle -> Pool Connection -> TokenLifeTime -> Maybe T.Text -> Int -> IO (Either LBS.ByteString Draft'')
getDraftByIdFromDb hLogger _ _ Nothing _ = do
    logError hLogger "No token parameter"
    return $ Left "No Token parameter"
getDraftByIdFromDb hLogger pool token_lifetime token' draft_id = do
    ch_author <- checkAuthor' hLogger pool token_lifetime token'
    case ch_author of
        Left bs -> return $ Left bs
        Right author_id -> catch (do
        --let q = "with image_arr as (select array_agg(image_id) from drafts_images where draft_id = ?) select short_title, date_of_changes, category_id, draft_text, main_image, (select * from image_arr) from drafts where author_id = ? and draft_id = ?"
        --rows <- queryWithPool pool q [author_id] :: IO [Draft']
        rows <- queryWithPool pool q [author_id] :: IO [Draft'']
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
    where q = toQuery $ BC.concat ["with image_arr as (select array_agg(image_id) from drafts_images where draft_id = ",BC.pack $ show draft_id,"), ",
                                  " tags_arr as (select array_agg(tag_name) from draft_tags join tags using (tag_id) where draft_id = ",BC.pack $ show draft_id,") " ,
                                  "select short_title, date_of_changes, category_id, draft_text, main_image, ",
                                  "(select * from image_arr) as other_images, (select * from  tags_arr) as tags from drafts where author_id = ? and draft_id = ",BC.pack $ show draft_id]-}

{-tstGetNews :: IO (Either LBS.ByteString NewsArray')
tstGetNews = do
    pool <- tstPool 
    getNewsByIdFromDb (Handle Debug printLog) pool (Just 5)-}


{-getCommentsByNewsIdFromDb :: Handle -> Maybe Int -> Maybe ByteString ->  IO (Either LBS.ByteString CommentArray)
getCommentsByNewsIdFromDb hLogger Nothing _ = do
    logError hLogger "No news parameter"
    return $ Left "No news parameter"
getCommentsByNewsIdFromDb hLogger (Just news_id) page_p =  catch ( do
                            logInfo hLogger $ T.concat ["Someone try get comments from news №", T.pack $ show news_id]
                            let pg = if isNothing page_p then " limit 10 offset 0"
                                        else
                                            BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page_p)) - 1)*10 ]
                            conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                            let q = toQuery $ BC.concat ["select (concat(first_name, ' ', last_name)) as author_name, comment_text, comment_time, comment_id",
                                                        " from users_comments join users using (user_id) where news_id = ? order by comment_time", pg]
                            rows <- query conn q [news_id]
                            close conn
                            return (Right $ CommentArray rows)) $ \e -> do
                                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                                            let errState = sqlState e
                                                            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                                            logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                                            case errStateInt of
                                                                23503 -> return $ Left "News not exist"
                                                                23502 -> return $ Left "Bad token"
                                                                _ -> return $ Left "Database error"-}

getCommentsByNewsIdFromDb :: Handle -> Pool Connection ->   Maybe Int -> Maybe ByteString ->  IO (Either LBS.ByteString CommentArray)
getCommentsByNewsIdFromDb hLogger _ Nothing _ = do
    logError hLogger "No news parameter"
    return $ Left "No news parameter"
getCommentsByNewsIdFromDb hLogger pool (Just news_id) page_p =  catch ( do
                            logInfo hLogger $ T.concat ["Someone try get comments from news №", T.pack $ show news_id]
                            rows <- queryWithPool pool q [news_id]
                            return (Right $ CommentArray rows)) $ \e -> do
                                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                                            let errState = sqlState e
                                                            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                                            logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                                            case errStateInt of
                                                                23503 -> return $ Left "News not exist"
                                                                23502 -> return $ Left "Bad token"
                                                                _ -> return $ Left "Database error"
    where
        pg = if isNothing page_p then " limit 10 offset 0"
                else
                    BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page_p)) - 1)*10 ]
        q = toQuery $ BC.concat ["select (concat(first_name, ' ', last_name)) as author_name, comment_text, comment_time, comment_id",
                                " from users_comments join users using (user_id) where news_id = ? order by comment_time", pg]

{-tstGetComments :: IO (Either LBS.ByteString CommentArray)
tstGetComments = do
    pool <- tstPool 
    getCommentsByNewsIdFromDb (Handle Debug printLog) pool (Just 2) Nothing-}


{-checkNews :: Handle -> Maybe Int -> IO (Int, LBS.ByteString )
checkNews hLogger Nothing = do
    logError hLogger "Bad news id"
    return  (0, "Bad news id")
checkNews hLogger (Just news'_id) = catch (do
        conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
        rows <- query conn "select news_id from news where news_id = ?" [news'_id] :: IO [Only Int]
        close conn
        if Prelude.null rows then do
            logError hLogger "News not exist"
            return (0, "News not exist")
        else return (fromOnly $ Prelude.head rows, "") )$ \e -> do
                                let err = E.decodeUtf8 $ sqlErrorMsg e
                                logError hLogger err
                                return (0,"Database error")

tst :: IO (Int, LBS.ByteString)
tst = checkNews (Handle Debug printLog) (Just 10)-}


{-addCommentToDb :: Handle -> T.Text -> Maybe Int -> Maybe T.Text -> IO (Either LBS.ByteString LBS.ByteString)
addCommentToDb hLogger _ Nothing _ = do
    logError hLogger "No comment id parameter"
    return $ Left "No comment id parameter"
addCommentToDb hLogger _ _ Nothing = do
    logError hLogger "No comment parameter"
    return $ Left "No comment parameter"
addCommentToDb hLogger token' (Just newsId) (Just comment) = catch (do
                            logInfo hLogger $ T.concat ["User №", T.pack $ show token', " try add commentary to news №", T.pack $ show newsId]
                            now <- getCurrentTime
                            let com = Comment token' 86400 comment newsId now
                            conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                            let q = "insert into users_comments (user_id, comment_text,news_id,comment_time) values (check_token(?,?),?,?,?)"
                            n_r <- execute conn q com
                            close conn
                            if n_r > 0 then do
                                logInfo hLogger $ T.concat ["User №", T.pack $ show token', "  add news commentary to news №", T.pack $ show newsId]
                                return $ Right "comment added"
                            else do
                                logError hLogger "Comment not added"
                                return $ Right "Comment not added") $ \e -> do
                                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                                            let errState = sqlState e
                                                            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                                            logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                                            case errStateInt of
                                                                23503 -> return $ Left "News not exist"
                                                                23502 -> return $ Left "Bad token"
                                                                _ -> return $ Left "Database error"-}


addCommentToDb :: Handle -> Pool Connection -> TokenLifeTime  ->T.Text -> Maybe Int -> Maybe T.Text -> IO (Either LBS.ByteString LBS.ByteString)
addCommentToDb hLogger _ _ _ Nothing _ = do
    logError hLogger "No comment id parameter"
    return $ Left "No comment id parameter"
addCommentToDb hLogger _ _ _ _ Nothing = do
    logError hLogger "No comment parameter"
    return $ Left "No comment parameter"
addCommentToDb hLogger pool token_lifetime token' (Just newsId) (Just comment) = catch (do
                            logInfo hLogger $ T.concat ["User №", T.pack $ show token', " try add commentary to news №", T.pack $ show newsId]
                            now <- getCurrentTime
                            let com = Comment token' token_lifetime comment newsId now
                            n_r <- executeWithPool pool q com
                            if n_r > 0 then do
                                logInfo hLogger $ T.concat ["User №", T.pack $ show token', "  add news commentary to news №", T.pack $ show newsId]
                                return $ Right "comment added"
                            else do
                                logError hLogger "Comment not added"
                                return $ Right "Comment not added") $ \e -> do
                                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                                            let errState = sqlState e
                                                            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                                            logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                                            case errStateInt of
                                                                23503 -> return $ Left "News not exist"
                                                                23502 -> return $ Left "Bad token"
                                                                _ -> return $ Left "Database error"
    where
        q = "insert into users_comments (user_id, comment_text,news_id,comment_time) values (check_token(?,?),?,?,?)"




{-deleteCommentFromDb :: Handle -> T.Text -> Maybe Int -> IO (Either LBS.ByteString LBS.ByteString)
--deleteCommentFromDb hLogger token' Nothing = do
deleteCommentFromDb hLogger _ Nothing = do
    logError hLogger "Bad comment id"
    return $ Left "Bad comment id"

{-deleteCommentFromDb hLogger token (Just comment_id) = do
        isAdmin <- checkAdmin hLogger token
        case isAdmin of
            (False,e) -> return $ Left e
            (True ,_) -> catch (do
                conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                execute conn "delete from users_comments where comment_id = ?" [comment_id]
                return $ Right "Comment deleted") $ \e -> do
                                                let err = E.decodeUtf8 $ sqlErrorMsg e
                                                logError hLogger err
                                                return $ Left "Database error"-}
deleteCommentFromDb hLogger token' (Just comment_id) = do
        isAdmin <- checkAdmin hLogger token'
        case isAdmin of
            (False,e) -> return $ Left e
            (True ,_) -> catch (do
                conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                n <- execute conn "delete from users_comments where comment_id = ?" [comment_id]
                if n > 0 then do
                    logInfo hLogger "Comment deleted"
                    return $ Right "Comment deleted"
                else do
                    logError hLogger "Comment not deleted"
                    return $ Left "Comment not deleted") $ \e -> do
                                                let err = E.decodeUtf8 $ sqlErrorMsg e
                                                logError hLogger err
                                                return $ Left "Database error"-}


deleteCommentFromDb :: Handle -> Pool Connection -> TokenLifeTime -> Maybe T.Text -> Maybe Int -> IO (Either LBS.ByteString LBS.ByteString)
deleteCommentFromDb hLogger _ _ _ Nothing = do
    logError hLogger "Bad comment id"
    return $ Left "Bad comment id"
deleteCommentFromDb hLogger pool token_lifetime token' (Just comment_id) = do
    isAdmin <- checkAdmin' hLogger pool token_lifetime token'
    case isAdmin of
            (False,e) -> return $ Left e
            (True ,_) -> catch (do
                n <- executeWithPool pool "delete from users_comments where comment_id = ?" [comment_id]
                if n > 0 then do
                    logInfo hLogger "Comment deleted"
                    return $ Right "Comment deleted"
                else do
                    logError hLogger "Comment not deleted"
                    return $ Left "Comment not deleted") $ \e -> do
                                                let err = E.decodeUtf8 $ sqlErrorMsg e
                                                logError hLogger err
                                                return $ Left "Database error"


checkAdmin :: Handle -> T.Text -> IO (Bool, LBS.ByteString)
checkAdmin hLogger token' = catch (do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    rows <- query conn "select admin_mark from users join tokens using (user_id) where token = ? and (current_timestamp - tokens.creation_date < interval '1 day')" [token'] :: IO [Only Bool]
    close conn
    if Prelude.null rows then do
        logError hLogger "Bad token"
        return (False, "Bad token")
        else do
            let admin'_mark = fromOnly $ Prelude.head rows
            if admin'_mark then
                return (admin'_mark,"")
            else
                return (admin'_mark, "Not admin")) $ \e -> do
                                                let err = E.decodeUtf8 $ sqlErrorMsg e
                                                logError hLogger err
                                                return (False,"Database error")

checkAdmin' :: Handle -> Pool Connection -> TokenLifeTime -> Maybe T.Text -> IO (Bool, LBS.ByteString)
checkAdmin' hLogger _ _ Nothing = do
    logError hLogger "No token parameter"
    return (False, "No token parameter")
checkAdmin' hLogger pool token_liferime (Just token') = catch (do
    --conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    --rows <- query conn "select admin_mark from users join tokens using (user_id) where token = ? and (current_timestamp - tokens.creation_date < interval '1 day')" [token'] :: IO [Only Bool]
    --close conn
    rows <- queryWithPool pool "select admin_mark from users join tokens using (user_id) where token = ? and ((current_timestamp - tokens.creation_date) < make_interval(secs => ?))" (token',token_liferime)
    if Prelude.null rows then do
        logError hLogger "Bad token"
        return (False, "Bad token")
        else do
            let admin'_mark = fromOnly $ Prelude.head rows
            if admin'_mark then
                return (admin'_mark,"")
            else
                return (admin'_mark, "Not admin")) $ \e -> do
                                                let err = E.decodeUtf8 $ sqlErrorMsg e
                                                logError hLogger err
                                                return (False,"Database error")
{-tstCheckAdmin = do
    pool <- tstPool
    checkAdmin' (Handle Debug printLog) pool 886400 "dahaku202108061111442285637"-}


{-getNewsFilterByTagInFromDb :: Handle -> Maybe ByteString -> Maybe ByteString -> IO (Either LBS.ByteString NewsArray')
--getNewsFilterByTagInFromDb hLogger Nothing page = do
getNewsFilterByTagInFromDb hLogger Nothing _ = do
    logError hLogger "No tag parameter"
    return $ Left "No tag parameter"
getNewsFilterByTagInFromDb hLogger (Just tag_lst) page_p = do
    logInfo hLogger "Someone try get news list filtered by tag_in parameter"
    case readByteStringListInt tag_lst of
                Nothing -> do
                    logError hLogger "Bad tag parameter"
                    return $ Left "bad tag parameter"
                Just n -> catch (do
                        conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                        let pg = fromMaybe 1 (readByteStringToInt (fromMaybe "" page_p))
                        rows <- query conn "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id), news_text from news join authors using (author_id) join users using (user_id) join news_tags using (news_id) where tag_id in ? group by news_id,author_name order by 2 DESC " (Only (In n))
                        close conn
                        return (Right $ NewsArray' $ takePage pg rows)) $ \e -> do
                                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                                            logError hLogger err
                                                            return $ Left "Database error"-}

getNewsFilterByTagInFromDb :: Handle -> Pool Connection -> Maybe ByteString -> Maybe ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByTagInFromDb hLogger _ Nothing _ = do
    logError hLogger "No tag parameter"
    return $ Left "No tag parameter"
getNewsFilterByTagInFromDb hLogger pool (Just tag_lst) page_p = do
    logInfo hLogger "Someone try get news list filtered by tag_in parameter"
    case readByteStringListInt tag_lst of
                Nothing -> do
                    logError hLogger "Bad tag parameter"
                    return $ Left "bad tag parameter"
                Just n -> catch (do
                        rows <- queryWithPool pool q (Only (In n))
                        return (Right $ NewsArray' rows)) $ \e -> do
                                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                                            logError hLogger err
                                                            return $ Left "Database error"
        where
            pg = if isNothing page_p then " limit 10 offset 0"
                        else
                            BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page_p)) - 1)*10 ]
            q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, ",
                                     "take_categories_list(category_id), news_text from news join authors using (author_id) ",
                                     "join users using (user_id) join news_tags using (news_id) where tag_id in ? ",
                                     "group by news_id,author_name order by 2 DESC",pg]





{-tstTagin = do
    pool <- tstPool
    getNewsFilterByTagInFromDb' (Handle Debug printLog) pool (Just "[2]") Nothing-}



{-getNewsFilterByCategoryIdFromDb :: Handle -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
--getNewsFilterByCategoryIdFromDb hLogger Nothing page sortParam = do
getNewsFilterByCategoryIdFromDb hLogger Nothing _ _ = do
    logError hLogger "No category parameter"
    return $ Left "No category parameter"
getNewsFilterByCategoryIdFromDb hLogger (Just cat_id) page_p sortParam = do
    logInfo hLogger "Someone try get news list filtered by category id"
    case readByteStringToInt cat_id of
                Nothing -> do
                    logError hLogger "Bad category parameter"
                    return $ Left "Bad category parameter"
                Just n -> catch (do
                        conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                        let sort' = if sortParam == "" then ""
                                                else BC.concat [" order by ",sortParam," DESC"]
                        let pg = if isNothing page_p then " limit 10 offset 0"
                                            else
                                         BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page_p)) - 1)*10 ]
                        let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id), news_text  from news  join authors using (author_id) join users using (user_id) where category_id = ? ", sort',pg]
                        rows <- query conn q [n]
                        close conn
                        return (Right $ NewsArray' rows)) $ \e -> do
                                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                                            logError hLogger err
                                                            return $ Left "Database error"-}

getNewsFilterByCategoryIdFromDb :: Handle -> Pool Connection ->  Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByCategoryIdFromDb hLogger _ Nothing _ _ = do
    logError hLogger "No category parameter"
    return $ Left "No category parameter"
getNewsFilterByCategoryIdFromDb hLogger pool (Just cat_id) page_p sortParam = do
    logInfo hLogger "Someone try get news list filtered by category id"
    case readByteStringToInt cat_id of
                Nothing -> do
                    logError hLogger "Bad category parameter"
                    return $ Left "Bad category parameter"
                Just n -> catch (do
                        rows <- queryWithPool pool q [n]
                        return (Right $ NewsArray' rows)) $ \e -> do
                                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                                            logError hLogger err
                                                            return $ Left "Database error"
    where
        sort' = if sortParam == "" then ""
                    else
                        BC.concat [" order by ",sortParam," DESC"]
        pg = if isNothing page_p then " limit 10 offset 0"
                else
                    BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page_p)) - 1)*10 ]
        q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, ",
                                 "take_categories_list(category_id), news_text  from news  join authors using (author_id) join users using (user_id) ",
                                 "where category_id = ? ", sort',pg]



{-getNewsFilterByTitleFromDb  :: Handle -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
--getNewsFilterByTitleFromDb  hLogger Nothing page sortParam = do
getNewsFilterByTitleFromDb  hLogger Nothing _ _ = do
    logError hLogger "No title parameter"
    return $ Left "No title parameter"
getNewsFilterByTitleFromDb  hLogger (Just titleName) page_p sortParam = catch (do
                    logInfo hLogger "Someone try get news list filtered by title"
                    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                    let sort' = if sortParam == "" then ""
                                            else BC.concat [" order by ",sortParam," DESC"]
                    let pg = if isNothing page_p then " limit 10 offset 0"
                                        else
                                     BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page_p)) - 1)*10 ]
                    let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id), news_text from news join authors using (author_id) join users using (user_id) where short_title = ? ",sort', pg]
                    rows <- query conn q [titleName]
                    close conn
                    return (Right $ NewsArray' rows)) $ \e -> do
                                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                                            logError hLogger err
                                                            return $ Left "Database error"-}

getNewsFilterByTitleFromDb  :: Handle -> Pool Connection ->  Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByTitleFromDb  hLogger _ Nothing _ _ = do
    logError hLogger "No title parameter"
    return $ Left "No title parameter"
getNewsFilterByTitleFromDb hLogger pool (Just titleName) page_p sortParam = catch (do
                    logInfo hLogger "Someone try get news list filtered by title"
                    rows <- queryWithPool pool q [titleName]
                    return (Right $ NewsArray' rows)) $ \e -> do
                                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                                            logError hLogger err
                                                            return $ Left "Database error"
    where
        sort' = if sortParam == "" then ""
                    else
                        BC.concat [" order by ",sortParam," DESC"]
        pg = if isNothing page_p then " limit 10 offset 0"
                else
                    BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page_p)) - 1)*10 ]
        q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, ",
                                 "take_categories_list(category_id), news_text from news join authors using (author_id) join users using (user_id) ",
                                 "where short_title = ? ",sort', pg]


{-getNewsFilterByAuthorNameFromDb  :: Handle -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
--getNewsFilterByAuthorNameFromDb hLogger Nothing page sortParam = do
getNewsFilterByAuthorNameFromDb hLogger Nothing _ _ = do
        logError hLogger "No author_name parameter"
        return $ Left "No author_name parameter"
getNewsFilterByAuthorNameFromDb hLogger (Just authorName) page_p sortParam = catch (do
                    logInfo hLogger "Someone try get news list filtered by author's name"
                    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                    let sort' = if sortParam == "" then ""
                                            else BC.concat [" order by ",sortParam," DESC"]
                    let pg = if isNothing page_p then " limit 10 offset 0"
                                        else
                                     BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page_p)) - 1)*10 ]
                    let q = toQuery $ BC.concat ["select * from (select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id), news_text from news join authors using (author_id) join users using (user_id)) as temp_t where author_name = ? ",sort', pg]
                    rows <- query conn q [authorName]
                    close conn
                    return (Right $ NewsArray' rows)) $ \e -> do
                                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                                            logError hLogger err
                                                            return $ Left "Database error"-}

getNewsFilterByAuthorNameFromDb  :: Handle -> Pool Connection -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByAuthorNameFromDb hLogger _ Nothing _ _ = do
        logError hLogger "No author_name parameter"
        return $ Left "No author_name parameter"
getNewsFilterByAuthorNameFromDb hLogger pool (Just authorName) page_p sortParam = catch (do
                    logInfo hLogger "Someone try get news list filtered by author's name"
                    rows <- queryWithPool pool q [authorName]
                    return (Right $ NewsArray' rows)) $ \e -> do
                                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                                            logError hLogger err
                                                            return $ Left "Database error"
    where
        sort' = if sortParam == "" then ""
            else
                BC.concat [" order by ",sortParam," DESC"]
        pg = if isNothing page_p then " limit 10 offset 0"
            else
                BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page_p)) - 1)*10 ]
        q = toQuery $ BC.concat ["select * from (select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, ",
                                 "take_categories_list(category_id), news_text from news join authors using (author_id) join users using (user_id)) as temp_t ",
                                 "where author_name = ? ",sort', pg]


{-getNewsFilterByDateFromDb  :: Handle -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
--getNewsFilterByDateFromDb  hLogger Nothing page sortParam
getNewsFilterByDateFromDb  hLogger Nothing _ _ = do
    logError hLogger "No date parameter"
    return $ Left "No date parameter"
getNewsFilterByDateFromDb  hLogger (Just date) page_p sortParam = do
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
                                let pg = if isNothing page_p then " limit 10 offset 0"
                                         else
                                            BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page_p)) - 1)*10 ]
                                let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id), news_text from news join authors using (author_id) join users using (user_id) where date_creation = ? ",sort', pg]
                                rows <- query conn q [day]
                                close conn
                                return (Right $ NewsArray' rows)) $ \e -> do
                                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                                            logError hLogger err
                                                            return $ Left "Database error"-}

getNewsFilterByDateFromDb  :: Handle -> Pool Connection ->  Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByDateFromDb hLogger _ Nothing _ _ = do
    logError hLogger "No date parameter"
    return $ Left "No date parameter"
getNewsFilterByDateFromDb hLogger pool (Just date) page_p sortParam = do
    logInfo hLogger "Someone try get news list filtered by date"
    let date' = readByteStringToDay date
    case date' of
        Nothing -> do
            logError hLogger "Bad date parameter"
            return (Left "Bad date parameter")
        Just day -> catch (do
                            rows <- queryWithPool pool q [day]
                            return (Right $ NewsArray' rows)) $ \e -> do
                                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                                            logError hLogger err
                                                            return $ Left "Database error"
    where
        sort' = if sortParam == "" then ""
            else
                BC.concat [" order by ",sortParam," DESC"]
        pg = if isNothing page_p then " limit 10 offset 0"
                else
                    BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page_p)) - 1)*10 ]
        q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, ",
                                 "take_categories_list(category_id), news_text from news join authors using (author_id) join users using (user_id) ",
                                 "where date_creation = ? ",sort', pg]


{-getNewsFilterByTagAllFromDb  :: Handle -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
--getNewsFilterByTagAllFromDb  hLogger Nothing page sortParam = do
getNewsFilterByTagAllFromDb  hLogger Nothing _ _ = do
    logError hLogger "No tag parameter"
    return $ Left "No tag parameter"
getNewsFilterByTagAllFromDb  hLogger (Just tag_lst) page_p sortParam = do
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
                        let pg = if isNothing page_p then " limit 10 offset 0"
                                            else
                                         BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page_p)) - 1)*10 ]
                        let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id), news_text  from news join authors using (author_id) join users using (user_id) join news_tags using (news_id) where tag_id in ? group by news_id,author_name having count(*) > ",cn, " ",sort',pg]
                        rows <- query conn q (Only (In tag_list))
                        close conn
                        return (Right $ NewsArray' rows)) $ \e -> do
                                                                let err = E.decodeUtf8 $ sqlErrorMsg e
                                                                logError hLogger err
                                                                return $ Left "Database error"-}
getNewsFilterByTagAllFromDb  :: Handle -> Pool Connection -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByTagAllFromDb hLogger _ Nothing _ _ = do
    logError hLogger "No tag parameter"
    return $ Left "No tag parameter"
getNewsFilterByTagAllFromDb hLogger pool (Just tag_lst) page_p sortParam = do
    logInfo hLogger "Someone try get news list filtered by tag_all parameter"
    case readByteStringListInt tag_lst of
        Nothing -> do
            logError hLogger "Bad tag parameter"
            return ( Left "bad tag_all parameter")
        Just tag_list -> catch (do
                        let cn = BC.pack $ show $ Prelude.length tag_list - 1
                        let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, ",
                                                     "take_categories_list(category_id), news_text  from news join authors using (author_id) ",
                                                     "join users using (user_id) join news_tags using (news_id) where tag_id in ? ",
                                                     "group by news_id,author_name having count(*) > ",cn, " ",sort',pg]
                        rows <- queryWithPool pool q (Only (In tag_list))
                        return (Right $ NewsArray' rows)) $ \e -> do
                                                                let err = E.decodeUtf8 $ sqlErrorMsg e
                                                                logError hLogger err
                                                                return $ Left "Database error"
    where
        sort' = if sortParam == "" then ""
            else
                BC.concat [" order by ",sortParam," DESC"]
        pg = if isNothing page_p then " limit 10 offset 0"
                else
                    BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page_p)) - 1)*10 ]

{-getNewsFilterByContentFromDb  :: Handle -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
--getNewsFilterByContentFromDb  hLogger Nothing page_p sortParam = do
getNewsFilterByContentFromDb  hLogger Nothing _ _ = do
    logError hLogger "No content parameter"
    return $ Left "No content parameter"
getNewsFilterByContentFromDb  hLogger (Just content_c) page_p sortParam = catch (do
                logInfo hLogger "Someone try get news list filtered by content"
                conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                let sort' = if sortParam == "" then ""
                                            else BC.concat [" order by ",sortParam," DESC"]
                let pg = if isNothing page_p then " limit 10 offset 0"
                                        else
                                     BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page_p)) - 1)*10 ]
                let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id), news_text from news join authors using (author_id) join users using (user_id) where news_text like ? ",sort', pg]
                rows <- query conn q [BC.concat ["%",content_c,"%"]]
                close conn
                return (Right $ NewsArray' rows)) $ \e -> do
                                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                                        logError hLogger err
                                                        return $ Left "Database error"-}


getNewsFilterByContentFromDb :: Handle -> Pool Connection -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')

getNewsFilterByContentFromDb hLogger _ Nothing _ _ = do
    logError hLogger "No content parameter"
    return $ Left "No content parameter"
getNewsFilterByContentFromDb hLogger pool (Just content_c) page_p sortParam = catch (do
                logInfo hLogger "Someone try get news list filtered by content"
                rows <- queryWithPool pool q [BC.concat ["%",content_c,"%"]]
                return (Right $ NewsArray' rows)) $ \e -> do
                                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                                        logError hLogger err
                                                        return $ Left "Database error"
    where
        sort' = if sortParam == "" then ""
                    else
                        BC.concat [" order by ",sortParam," DESC"]
        pg = if isNothing page_p then " limit 10 offset 0"
                else
                    BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page_p)) - 1)*10 ]
        q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, ",
                                 "take_categories_list(category_id), news_text from news join authors using (author_id) join users using (user_id) ",
                                 "where news_text like ? ",sort', pg]


{-getNewsFilterByAfterDateFromDb  :: Handle -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
--getNewsFilterByAfterDateFromDb hLogger Nothing page sortParam = do
getNewsFilterByAfterDateFromDb hLogger Nothing _ _ = do
    logError hLogger "No date parameter"
    return $ Left "No date parameter"
getNewsFilterByAfterDateFromDb hLogger (Just date) page_p sortParam = do
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
                                let pg = if isNothing page_p then " limit 10 offset 0"
                                         else
                                            BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page_p)) - 1)*10 ]
                                let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id), news_text from news join authors using (author_id) join users using (user_id) where date_creation > ? ",sort', pg]
                                rows <- query conn q [day]
                                close conn
                                return (Right $ NewsArray' rows)) $ \e -> do
                                                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                                                        logError hLogger err
                                                                        return $ Left "Database error"-}
getNewsFilterByAfterDateFromDb  :: Handle -> Pool Connection -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByAfterDateFromDb hLogger _ Nothing _ _ = do
    logError hLogger "No date parameter"
    return $ Left "No date parameter"
getNewsFilterByAfterDateFromDb hLogger pool (Just date) page_p sortParam = do
    logInfo hLogger "Someone try get news list filtered by after_date parameter"
    let date' = readByteStringToDay date
    case date' of
        Nothing -> do
            logError hLogger "Bad date parameter"
            return $ Left "Bad date parameter"
        Just day -> catch (do
                            rows <- queryWithPool pool q [day]
                            return (Right $ NewsArray' rows)) $ \e -> do
                                                                    let err = E.decodeUtf8 $ sqlErrorMsg e
                                                                    logError hLogger err
                                                                    return $ Left "Database error"
    where
        sort' = if sortParam == "" then ""
                    else
                        BC.concat [" order by ",sortParam," DESC"]
        pg = if isNothing page_p then " limit 10 offset 0"
                else
                    BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page_p)) - 1)*10 ]
        q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, ",
                                 "take_categories_list(category_id), news_text from news join authors using (author_id) join users using (user_id) ",
                                 "where date_creation > ? ",sort', pg]


{-getNewsFilterByBeforeDateFromDb  :: Handle -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
--getNewsFilterByBeforeDateFromDb hLogger Nothing page sortParam = do
getNewsFilterByBeforeDateFromDb hLogger Nothing _ _ = do
    logError hLogger "No date parameter"
    return $ Left "No date parameter"
getNewsFilterByBeforeDateFromDb hLogger (Just date) page_p sortParam = do
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
                        let pg = if isNothing page_p then " limit 10 offset 0"
                                         else
                                            BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page_p)) - 1)*10 ]
                        let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id), news_text from news join authors using (author_id) join users using (user_id) where date_creation < ? ",sort', pg]
                        rows <- query conn q [day]
                        close conn
                        return (Right $ NewsArray' rows)) $ \e -> do
                                                                let err = E.decodeUtf8 $ sqlErrorMsg e
                                                                logError hLogger err
                                                                return $ Left "Database error"-}

getNewsFilterByBeforeDateFromDb :: Handle -> Pool Connection -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByBeforeDateFromDb hLogger _ Nothing _ _ = do
    logError hLogger "No date parameter"
    return $ Left "No date parameter"
getNewsFilterByBeforeDateFromDb hLogger pool (Just date) page_p sortParam = do
    logInfo hLogger "Someone try get news list filtered by before_date parameter"
    let date' = readByteStringToDay date
    case date' of
        Nothing -> do
            logError hLogger "Bad date parameter"
            return $ Left "Bad date parameter"
        Just day -> catch (do
                rows <- queryWithPool pool q [day]
                return (Right $ NewsArray' rows)) $ \e -> do
                                            let err = E.decodeUtf8 $ sqlErrorMsg e
                                            logError hLogger err
                                            return $ Left "Database error"
    where
        sort' = if sortParam == "" then ""
                    else
                        BC.concat [" order by ",sortParam," DESC"]
        pg = if isNothing page_p then " limit 10 offset 0"
                else
                    BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page_p)) - 1)*10 ]
        q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, ",
                                 "take_categories_list(category_id), news_text from news join authors using (author_id) join users using (user_id) ",
                                 "where date_creation < ? ",sort', pg]


{-getNewsFilterByTagIdFromDb :: Handle -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
--getNewsFilterByTagIdFromDb hLogger Nothing page sortParam = do
getNewsFilterByTagIdFromDb hLogger Nothing _ _ = do
    logError hLogger "No tag parameter"
    return $ Left "No tag parameter"
getNewsFilterByTagIdFromDb hLogger (Just tag_id) page_p' sortParam = do
    logInfo hLogger "Someone try get news list filtered by tag"
    case readByteStringToInt  tag_id of
                Nothing -> do
                    logError hLogger "Bad tag parameter"
                    return $ Left "Bad tag parameter"
                Just n -> catch (do
                        conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                        let sort' = if sortParam == "" then ""
                                                else BC.concat [" order by ",sortParam," DESC"]
                        let pg = if isNothing page_p' then " limit 10 offset 0"
                                            else
                                         BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page_p')) - 1)*10 ]
                        let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id), news_text  from news  join authors using (author_id) join users using (user_id) join news_tags using (news_id) where tag_id = ?", sort',pg]
                        rows <- query conn q [n]
                        close conn
                        return (Right $ NewsArray' rows)) $ \e -> do
                                                                let err = E.decodeUtf8 $ sqlErrorMsg e
                                                                logError hLogger err
                                                                return $ Left "Database error"-}


getNewsFilterByTagIdFromDb :: Handle -> Pool Connection -> Maybe ByteString -> Maybe ByteString -> ByteString -> IO (Either LBS.ByteString NewsArray')
getNewsFilterByTagIdFromDb hLogger _ Nothing _ _ = do
    logError hLogger "No tag parameter"
    return $ Left "No tag parameter"
getNewsFilterByTagIdFromDb hLogger pool (Just tag_id) page_p' sortParam = do
    logInfo hLogger "Someone try get news list filtered by tag"
    case readByteStringToInt  tag_id of
        Nothing -> do
            logError hLogger "Bad tag parameter"
            return $ Left "Bad tag parameter"
        Just n -> catch (do
                        rows <- queryWithPool pool q [n]
                        return (Right $ NewsArray' rows)) $ \e -> do
                                                                let err = E.decodeUtf8 $ sqlErrorMsg e
                                                                logError hLogger err
                                                                return $ Left "Database error"
    where
        sort' = if sortParam == "" then ""
                    else
                        BC.concat [" order by ",sortParam," DESC"]
        pg = if isNothing page_p' then " limit 10 offset 0"
                else
                    BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" page_p')) - 1)*10 ]
        q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, ",
                                 "take_categories_list(category_id), news_text from news join authors using (author_id) join users using (user_id) ",
                                 "join news_tags using (news_id) where tag_id = ?", sort',pg]

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
    let token' = Prelude.filter (`Prelude.notElem` filt ) (show now)
    return (BC.concat [login,BC.pack token'],now)
        where filt = " :.-UTC" :: String

generateToken' :: T.Text -> IO (T.Text, UTCTime)
generateToken' login = do
    now <- getCurrentTime
    let token' = Prelude.filter (`Prelude.notElem` filt ) (show now)
    return (T.concat [login, T.pack token'],now)
        where filt = " :.-UTC" :: String

{-authentication :: Handle -> BC.ByteString -> BC.ByteString -> IO (Either LBS.ByteString LBS.ByteString)
authentication hLogger login password = catch (do
                logInfo hLogger $ T.concat ["User with login ", E.decodeUtf8 login, " try logging"]
                conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                (token',now) <- generateToken login
                let q = "WITH loging_user as (select user_id from users where login = ? and user_password = (crypt(?,user_password))) update tokens set token = ?, creation_date = ? where user_id in (select user_id from loging_user)"
                --let q = "insert into tokens (user_id, token ,creation_date) values (?, ? ,?) ON CONFLICT(user_id) DO UPDATE set token = ?, creation_date= ?"
                n <- execute conn q (login,password,token',now)
                close conn
                if n > 0 then do
                    logInfo hLogger $ T.concat ["User with login ", E.decodeUtf8 login, " logged"]
                    return $ Right $ LBS.pack $ B.unpack token'
                else do
                    logError hLogger $ T.concat ["User with login ", E.decodeUtf8 login, " cant logged"]
                    return $ Left "Wrong login or password"
                                                             ) $ \e -> do
                                                                let err = E.decodeUtf8 $ sqlErrorMsg e
                                                                logError hLogger err
                                                                return $ Left "Database error in authentication"-}

authentication :: Handle -> Pool Connection ->  BC.ByteString -> BC.ByteString -> IO (Either LBS.ByteString LBS.ByteString)
authentication hLogger pool login password = catch (do
    logInfo hLogger $ T.concat ["User with login ", E.decodeUtf8 login, " try logging"]
    (token',now) <- generateToken login
    n <- executeWithPool pool q (login,password,token',now)
    if n > 0 then do
        logInfo hLogger $ T.concat ["User with login ", E.decodeUtf8 login, " logged"]
        return $ Right $ LBS.pack $ B.unpack token'
    else do
        logError hLogger $ T.concat ["User with login ", E.decodeUtf8 login, " cant logged"]
        return $ Left "Wrong login or password") $ \e -> do
                                                let err = E.decodeUtf8 $ sqlErrorMsg e
                                                logError hLogger err
                                                return $ Left "Database error in authentication"
    where
        q = toQuery $ BC.concat ["WITH loging_user as (select user_id from users where login = ? and user_password = (crypt(?,user_password))) ",
                                     "update tokens set token = ?, creation_date = ? where user_id in (select user_id from loging_user)"]



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

{-createUserInDb :: Handle -> BC.ByteString  -> BC.ByteString  -> T.Text -> T.Text -> BC.ByteString  -> BC.ByteString  -> LBS.ByteString  -> IO (Either LBS.ByteString LBS.ByteString )
createUserInDb hLogger login password f'_name l_name avatar_name avatar_contentType avatar_b = do
    if avatar_name == "" || avatar_contentType == "" || avatar_b == "" || BC.take 5 avatar_contentType /= "image" then do
        logError hLogger "Bad image file"
        return $ Left "Bad image file"

    else catch (do
        conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
        now <- getCurrentTime
        --let q = "with avatar_id as (insert into images (image_name,image_b,content_type) values (?,?,?) returning image_id) insert into users (first_name, last_name, avatar, login, user_password, creation_date, admin_mark) values (?,?,(select image_id from avatar_id),?,?,?,?)"

        let q = "with avatar_id as (insert into images (image_name,image_b,content_type) values (?,?,?) returning image_id) insert into users (first_name, last_name, avatar, login, user_password, creation_date, admin_mark) values (?,?,(select image_id from avatar_id),?,crypt(?,gen_salt('md5')),?,?)"
        n <- execute conn q (avatar_name,Binary avatar_b,avatar_contentType,f'_name,l_name,login,password,now,False)
        if n > 0 then do
            logInfo hLogger $ T.concat ["User ", E.decodeUtf8 login, " registered"]
            firstToken hLogger login password
        else do
            logError hLogger "Registration failed"
            return $ Left "Registration failed") $ \e ->  do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        logError hLogger err
                                        return $ Left "Database error in registration"-}
createUserInDb :: Handle -> Pool Connection -> Maybe T.Text  -> Maybe T.Text  -> Maybe T.Text -> Maybe T.Text -> BC.ByteString  -> BC.ByteString  -> LBS.ByteString  -> IO (Either LBS.ByteString LBS.ByteString )
createUserInDb hLogger _ Nothing _ _ _ _ _ _  = do
    logError hLogger "No login parameter"
    return $ Left "No login parameter"
createUserInDb hLogger _ _ Nothing  _ _ _ _ _  = do
    logError hLogger "No password parameter"
    return $ Left "No password parameter"
createUserInDb hLogger _ _ _ Nothing _ _ _ _  = do
    logError hLogger "No first name parameter"
    return $ Left "No first name parameter"
createUserInDb hLogger _ _ _ _ Nothing  _ _ _  = do
    logError hLogger "No second name parameter"
    return $ Left "No second name parameter"
createUserInDb hLogger pool (Just login) (Just password) (Just f'_name) (Just l_name) avatar_name avatar_contentType avatar_b = do
    if avatar_name == "" || avatar_contentType == "" || avatar_b == "" || BC.take 5 avatar_contentType /= "image" then do
        logError hLogger "Bad image file"
        return $ Left "Bad image file"
    else catch (do
        now <- getCurrentTime
        n <- executeWithPool pool q (avatar_name,Binary avatar_b,avatar_contentType,f'_name,l_name,login,password,now,False)
        if n > 0 then do
            logInfo hLogger $ T.concat ["User ", login, " registered"]
            firstToken hLogger pool login password
        else do
            logError hLogger "Registration failed"
            return $ Left "Registration failed") $ \e ->  do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        logError hLogger err
                                        return $ Left "Database error in registration"
    where
        q = toQuery $ BC.concat ["with avatar_id as (insert into images (image_name,image_b,content_type) values (?,?,?) returning image_id) ",
                                 "insert into users (first_name, last_name, avatar, login, user_password, creation_date, admin_mark) ",
                                 "values (?,?,(select image_id from avatar_id),?,crypt(?,gen_salt('md5')),?,?)"]


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
deleteUserFromDb' :: Handle -> Pool Connection -> ByteString -> IO (Either LBS.ByteString LBS.ByteString)
deleteUserFromDb' hLogger pool login = catch (do
    logInfo hLogger $ T.concat ["Trying delete user ", E.decodeUtf8 login]
    --conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    --let q = "delete from users where login = ?"
    --n <- execute conn q [login]
    --close conn
    n <- executeWithPool pool "dlete from users where login = ?" [login]
    if n > 0 then do
        logInfo hLogger $ T.concat ["User ", E.decodeUtf8 login, " deleted"]
        return $ Right $ LBS.concat ["User ", LBS.fromStrict login, " deleted"]
    else return $ Left "User not exist") $ \e ->  do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        logError hLogger err
                                        return $ Left "Database error"


firstToken :: Handle -> Pool Connection -> T.Text -> T.Text ->  IO (Either LBS.ByteString LBS.ByteString)
firstToken hLogger pool login password = catch (do
                logInfo hLogger $ T.concat ["Generate first token for user ", login]
                --conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                (token',now) <- generateToken' login
                --let q = "WITH loging_user as (select user_id from users where login = ? and user_password = (crypt(?,user_password))) insert into tokens (user_id,token,creation_date) values ((select user_id from loging_user),?,?)"
                --let q = "insert into tokens (user_id, token ,creation_date) values (?, ? ,?) ON CONFLICT(user_id) DO UPDATE set token = ?, creation_date= ?"
                n <- executeWithPool pool q (login,password,token',now)
                --close conn
                if n > 0 then do
                    logInfo hLogger $ T.concat ["User with login ", login, " logged"]
                    return $ Right $ LBS.fromStrict $ E.encodeUtf8 token'
                else do
                    logError hLogger $ T.concat ["User with login ",login, " cant logged"]
                    return $ Left "Wrong login or password"
                                                             ) $ \e -> do
                                                                let err = E.decodeUtf8 $ sqlErrorMsg e
                                                                logError hLogger err
                                                                return $ Left "Database error"
                where
                    q = toQuery $ BC.concat ["WITH loging_user as (select user_id from users where login = ? and user_password = (crypt(?,user_password))) ",
                                             "insert into tokens (user_id,token,creation_date) values ((select user_id from loging_user),?,?)"]


{-profileOnDb :: Handle -> T.Text -> IO (Either LBS.ByteString Profile)
profileOnDb hLogger token' = catch (do
    logInfo hLogger "Sending profile information"
    let q = "select first_name, last_name, avatar from users join tokens using (user_id) where  token = ? and (now()- tokens.creation_date) < make_interval(secs => ?)"
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    rows <- query conn q (TokenProfile token' 86400) :: IO [Profile]
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
                                            _ -> return $ Left "Database error"-}

profileOnDb :: Handle -> Pool Connection -> TokenLifeTime ->  Maybe T.Text -> IO (Either LBS.ByteString Profile)
profileOnDb hLogger _ _ Nothing = do
    logError hLogger "No token parameter"
    return $ Left "No token parameter"
profileOnDb hLogger pool token_lifetime (Just token') = catch (do
    logInfo hLogger "Sending profile information"
    --let q = "select first_name, last_name, avatar from users join tokens using (user_id) where token = ? and (now()- tokens.creation_date) < make_interval(secs => ?)"
    --conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    rows <- queryWithPool pool q (TokenProfile token' token_lifetime)
    --close conn
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
    where
        q = "select first_name, last_name, avatar from users join tokens using (user_id) where token = ? and (now()- tokens.creation_date) < make_interval(secs => ?)"


---------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------Categories block---------------------------------------------------------------------------------

{-getCategoriesListFromDb :: Handle -> Maybe BC.ByteString -> IO (Either LBS.ByteString ListOfCategories)
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
                                        return $ Left "Database error"-}

getCategoriesListFromDb :: Handle -> Pool Connection -> Maybe BC.ByteString -> IO (Either LBS.ByteString ListOfCategories)
getCategoriesListFromDb hLogger pool pageParam = catch (do
    logInfo hLogger "Someone try get list of categories"
    rows <- query_WithPool pool q
    return $ Right (ListOfCategories rows)) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        logError hLogger err
                                        return $ Left "Database error"
    where
        pg = if isNothing pageParam then " limit 10 offset 0"
                else
                    BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" pageParam)) - 1)*10 ]
        q = toQuery $ BC.concat ["select category_name from categories",pg]


{-createCategoryOnDb :: Handle -> Maybe T.Text -> Maybe T.Text -> IO (Either LBS.ByteString LBS.ByteString)
createCategoryOnDb hLogger Nothing _ = do
    logError hLogger "No category_name field"
    return $ Left "No category_name field"

createCategoryOnDb hLogger _ Nothing = do
    logError hLogger "No maternal_category_name field"
    return $ Left "No maternal_category_name field"

createCategoryOnDb hLogger (Just category'_name) (Just maternal_name) = catch (do
    logInfo hLogger "Creating new category"
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    let q = "insert into categories (category_name, maternal_category) values (?,?) returning category_id"
    if maternal_name == "" then do
        logInfo hLogger "Maternal category is null"
        --let m = Nothing :: Maybe T.Text
        rows <- returning conn q [(category'_name, Nothing :: Maybe T.Text)] :: IO [Only Int]
        close conn
        logInfo hLogger "Category created"
        --let c_idd = LBS.fromStrict $ BC.pack $ show (fromOnly $ Prelude.head rows)
        return $ Right $ LBS.fromStrict $ BC.pack $ show (fromOnly $ Prelude.head rows)
    else do
    --conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
        let check_maternal = "select category_id from categories where category_name = ?"
        c'_id <- query conn check_maternal [maternal_name] :: IO [Only Int]
        if Prelude.null c'_id then do
            logError hLogger "Maternal category not exist"
            return $ Left "Maternal category not exist"
            else do
                rows <- returning conn q [(category'_name, fromOnly $ Prelude.head c'_id)] :: IO [Only Int]
                close conn
                logInfo hLogger "Category created"
                return $ Right $ LBS.fromStrict $ BC.pack $ show (fromOnly $ Prelude.head rows)) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger err
                                        case errStateInt of
                                            23505 -> return $ Left "Category already exist"
                                            _ -> return $ Left "Database error"-}

createCategoryOnDb :: Handle -> Pool Connection -> Maybe T.Text -> Maybe T.Text -> IO (Either LBS.ByteString LBS.ByteString)
createCategoryOnDb hLogger _ Nothing _ = do
    logError hLogger "No category_name field"
    return $ Left "No category_name field"

createCategoryOnDb hLogger _ _ Nothing = do
    logError hLogger "No maternal_category_name field"
    return $ Left "No maternal_category_name field"

createCategoryOnDb hLogger pool (Just category'_name) (Just maternal_name) = catch (do
    logInfo hLogger "Creating new category"
    if maternal_name == "" then do
        logInfo hLogger "Maternal category is null"
        rows <- returningWithPool pool q [(category'_name, Nothing :: Maybe T.Text)] :: IO [Only Int]
        logInfo hLogger "Category created"
        return $ Right $ LBS.fromStrict $ BC.pack $ show (fromOnly $ Prelude.head rows)
    else do
        c'_id <- queryWithPool pool check_maternal [maternal_name] :: IO [Only Int]
        if Prelude.null c'_id then do
            logError hLogger "Maternal category not exist"
            return $ Left "Maternal category not exist"
            else do
                rows <- returningWithPool pool q [(category'_name, fromOnly $ Prelude.head c'_id)] :: IO [Only Int]
                logInfo hLogger "Category created"
                return $ Right $ LBS.fromStrict $ BC.pack $ show (fromOnly $ Prelude.head rows)) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger err
                                        case errStateInt of
                                            23505 -> return $ Left "Category already exist"
                                            _ -> return $ Left "Database error"
    where
        q = "insert into categories (category_name, maternal_category) values (?,?) returning category_id"
        check_maternal = "select category_id from categories where category_name = ?"


{-deleteCategoryFromDb :: Handle -> Maybe T.Text -> IO (Either LBS.ByteString LBS.ByteString)
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
                                        return $ Left "Database error"-}

deleteCategoryFromDb :: Handle -> Pool Connection -> Maybe T.Text -> IO (Either LBS.ByteString LBS.ByteString)
deleteCategoryFromDb hLogger _ Nothing = do
    logError hLogger "No category_name parametr"
    return $ Left "No category_name parametr"
deleteCategoryFromDb hLogger pool (Just categoryName) = catch (do
    n <- executeWithPool pool "delete from categories where category_name = ?" [categoryName]
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

{-editCategoryOnDb :: Handle -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> IO (Either LBS.ByteString LBS.ByteString )
editCategoryOnDb hLogger Nothing _ _ = do
    logError hLogger "No old_name parametr"
    return $ Left "No old_name parametr"
editCategoryOnDb hLogger (Just old_name) (Just new'_name) (Just "") = catch (do
    logInfo hLogger $ T.concat ["Update category_name parameter on category ", old_name]
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    let q = "update categories set category_name = ? where category_name = ?"
    n <- execute conn q (new'_name,old_name)
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

editCategoryOnDb hLogger (Just old_name) (Just "") (Just new'_maternal) = catch (do
    logInfo hLogger $ T.concat ["Update maternal_category parameter on category ", old_name]
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    let check_maternal = "select category_id from categories where category_name = ?"
    m_id <- query conn check_maternal [new'_maternal] :: IO [Only Int]
    if Prelude.null m_id then do
        close conn
        logError hLogger $ T.concat ["Maternal category ", new'_maternal, " not exist" ]
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
editCategoryOnDb hLogger (Just old_name) (Just new'_name) (Just new'_maternal) = catch (do
    logInfo hLogger $ T.concat ["Update all parameters on category ", old_name]
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    let check_maternal = "select category_id from categories where category_name = ?"
    m_id <- query conn check_maternal [new'_maternal] :: IO [Only Int]
    if Prelude.null m_id then do
        close conn
        logError hLogger $ T.concat ["Maternal category ", new'_maternal, " not exist" ]
        return $ Left "Maternal category not exist"
    else do
        let q = "update categories set category_name = ?, maternal_category = ? where category_name = ?"
        n <- execute conn q (new'_name,fromOnly $ Prelude.head m_id, old_name)
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
    return $ Left "No update parameters"-}



editCategoryOnDb :: Handle -> Pool Connection -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> IO (Either LBS.ByteString LBS.ByteString )
editCategoryOnDb hLogger _ Nothing _ _ = do
    logError hLogger "No old_name parametr"
    return $ Left "No old_name parametr"
editCategoryOnDb hLogger pool (Just old_name) (Just new'_name) (Just "") = catch (do
    logInfo hLogger $ T.concat ["Update category_name parameter on category ", old_name]
    --conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    --let q = "update categories set category_name = ? where category_name = ?"
    --n <- execute conn q (new'_name,old_name)
    --close conn
    n <- executeWithPool pool "update categories set category_name = ? where category_name = ?" (new'_name,old_name)
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

editCategoryOnDb hLogger pool (Just old_name) (Just "") (Just new'_maternal) = catch (do
    logInfo hLogger $ T.concat ["Update maternal_category parameter on category ", old_name]
    --conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    --let check_maternal = "select category_id from categories where category_name = ?"
    --m_id <- query conn check_maternal [new'_maternal] :: IO [Only Int]
    m_id <- queryWithPool pool "select category_id from categories where category_name = ?" [new'_maternal] :: IO [Only Int]
    if Prelude.null m_id then do
        --close conn
        logError hLogger $ T.concat ["Maternal category ", new'_maternal, " not exist" ]
        return $ Left "Maternal category not exist"
    else do
        --let q = "update categories set maternal_category = ? where category_name = ?"
        --n <- execute conn q (fromOnly $ Prelude.head m_id, old_name)
        --close conn
        n <- executeWithPool pool "update categories set maternal_category = ? where category_name = ?" (fromOnly $ Prelude.head m_id, old_name)
        if n > 0 then do
            logInfo hLogger $ T.concat ["Category ", old_name, " edited"]
            return $ Right "Category edited"
        else do
            logError hLogger $ T.concat ["Category ", old_name, " not exist"]
            return $ Left "Category not exist") $  \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        logError hLogger err
                                        return $ Left "Database error"
editCategoryOnDb hLogger pool (Just old_name) (Just new'_name) (Just new'_maternal) = catch (do
    logInfo hLogger $ T.concat ["Update all parameters on category ", old_name]
    --conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    --let check_maternal = "select category_id from categories where category_name = ?"
    m_id <- queryWithPool pool "select category_id from categories where category_name = ?" [new'_maternal] :: IO [Only Int]
    if Prelude.null m_id then do
        --close conn
        logError hLogger $ T.concat ["Maternal category ", new'_maternal, " not exist" ]
        return $ Left "Maternal category not exist"
    else do
        let q = "update categories set category_name = ?, maternal_category = ? where category_name = ?"
        n <- executeWithPool pool q (new'_name,fromOnly $ Prelude.head m_id, old_name)
        --close conn
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
editCategoryOnDb hLogger _ _ _ _ = do
    logError hLogger "No update parameters"
    return $ Left "No update parameters"




{-tstCategory :: IO (Either LBS.ByteString LBS.ByteString)
tstCategory = deleteCategoryFromDb (Handle Debug printLog) (Just "селебрети")-}




--------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------Drafts Block-----------------------------------------------------------------------------------

checkAuthor :: Handle -> Maybe T.Text -> IO (Either LBS.ByteString Int)
checkAuthor hLogger Nothing = do
    logError hLogger "No token parameter"
    return $ Left "No token parameter"
checkAuthor hLogger (Just token') = catch (do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    --TIO.putStrLn token
    rows <- query conn "select author_id from authors join users using (user_id) join tokens using (user_id) where token = ?" [token']:: IO [Only Int]
    if  Prelude.null rows then do
        logError hLogger "User try get drafts without author's rights"
        return $ Left "You are not author"
    else
        return $ Right (fromOnly $ Prelude.head rows)) $  \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"


checkAuthor' :: Handle -> Pool Connection -> TokenLifeTime -> Maybe T.Text -> IO (Either LBS.ByteString Int)
checkAuthor' hLogger _ _ Nothing = do
    logError hLogger "No token parameter"
    return $ Left "No token parameter"
checkAuthor' hLogger pool token_lifetime (Just token') = catch (do
    --conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    --TIO.putStrLn token
    rows <- queryWithPool pool q [token']
    if  Prelude.null rows then do
        --logError hLogger "User try get drafts without author's rights"
        return $ Left "You are not author"
    else
        return $ Right (fromOnly $ Prelude.head rows)) $  \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"
    where q = toQuery $ BC.concat ["select author_id from authors join users using (user_id) join tokens using (user_id) where token = ? ",
                                    "and (now()- tokens.creation_date) < make_interval(secs => ",BC.pack $ show token_lifetime,")"]

{-getDraftsByAuthorId :: Int -> IO DraftArray 
getDraftsByAuthorId a_id = do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    --let q = "select short_title, date_of_changes, category_id, draft_text, main_image from drafts where author_id = ?"
    let q = "select short_title, date_of_changes, category_id, draft_text, main_image, array_agg(image_id) from drafts join drafts_images using (draft_id) where author_id = ? group by draft_id"
    rows <- query conn q [a_id] :: IO [Draft']
    return $ DraftArray rows-}
{-getDraftsByAuthorToken :: Handle -> T.Text -> IO (Either LBS.ByteString DraftArray)
getDraftsByAuthorToken hLogger token' = catch (do
    logInfo hLogger "Someone try get drafts list"
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    --let q = "select short_title, date_of_changes, category_id, draft_text, main_image from drafts where author_id = ?"
    let q = "select short_title, date_of_changes, category_id, draft_text, main_image, array_agg(image_id) from drafts join drafts_images using (draft_id) join authors using (author_id) join tokens using (user_id) where token = ? and (now() - tokens.creation_date) < make_interval(secs => ?) group by draft_id"
    rows <- query conn q  (TokenProfile token' 86400) :: IO [Draft']
    close conn
    if Prelude.null rows then do
        logError hLogger "User use bad token or haven't drafts"
        return $ Left "You are not author or don't have drafts "
    else return $ Right $ DraftArray rows) $  \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"-}
getDraftsByAuthorToken :: Handle -> Pool Connection -> TokenLifeTime -> Maybe T.Text -> IO (Either LBS.ByteString DraftArray)
getDraftsByAuthorToken hLogger _ _ Nothing = do
    logError hLogger "No token parameter"
    return $ Left "No token parameter"
getDraftsByAuthorToken hLogger pool token_lifetime (Just token') = catch (do
    logInfo hLogger "Someone try get drafts list"
    rows <- queryWithPool pool q  (TokenProfile token' token_lifetime) :: IO [Draft']
    if Prelude.null rows then do
        logError hLogger "User use bad token or haven't drafts"
        return $ Left "You are not author or don't have drafts "
    else return $ Right $ DraftArray rows) $  \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"
    where
        q = toQuery $ BC.concat ["select short_title, date_of_changes, category_id, draft_text, main_image, array_agg(image_id) ",
                                 "from drafts join drafts_images using (draft_id) join authors using (author_id) join tokens using (user_id) ",
                                 "where token = ? and (now() - tokens.creation_date) < make_interval(secs => ?) group by draft_id"]





{-tstDrafts :: IO (Either LBS.ByteString DraftArray)
tstDrafts = getDraftsByAuthorToken (Handle Debug printLog) "dahaku202108020903181300795"-}



{-createDraftOnDb' :: Handle -> Maybe BC.ByteString  -> Maybe BC.ByteString -> Maybe BC.ByteString -> Maybe BC.ByteString -> Maybe BC.ByteString -> Maybe Image''' -> Maybe [Image'''] -> IO (Either LBS.ByteString Int)
--createDraftOnDb' hLogger Nothing category tags short_title text main_image images_list = do
createDraftOnDb' hLogger Nothing _ _ _ _ _ _ = do
    logError hLogger "No token param"
    return $ Left "No token param"
--createDraftOnDb' hLogger _ Nothing tags short_title text main_image images_list = do
createDraftOnDb' hLogger _ Nothing _ _ _ _ _ = do
    logError hLogger "No category field"
    return $ Left "No category field"
--createDraftOnDb' hLogger _ _ Nothing short_title text main_image images_list = do
createDraftOnDb' hLogger _ _ Nothing _ _ _ _ = do
    logError hLogger "No tags field"
    return $ Left "No tags field"
--createDraftOnDb' hLogger _ _ _ Nothing text main_image images_list = do
createDraftOnDb' hLogger _ _ _ Nothing _ _ _ = do
    logError hLogger "No short_title field"
    return $ Left "No short_title field"
--createDraftOnDb' hLogger _ _ _ _ Nothing main_image images_list = do
createDraftOnDb' hLogger _ _ _ _ Nothing _ _ = do
    logError hLogger "No text field"
    return $ Left "No text field"
createDraftOnDb' hLogger (Just token') (Just category) (Just tags) (Just short'_title) (Just text) main'_image images_list = do
    logInfo hLogger "Someone try add new draft"
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    --now <- getCurrentTime
    --let q = "with get_a as (select author_id from authors join tokens using (user_id) where token = ?), get_c as (select category_id from categories where category_name = ?) insert into drafts (author_id,short_title,date_of_changes,category_id,draft_text) values ((select author_id from get_a),?,?,(select category_id from get_c),?) returning draft_id"
    let q =  toQuery $ BC.concat ["with get_a as (select author_id from authors join tokens using (user_id) where token = '",
                    token',
                    "'), get_c as (select category_id from categories where category_name = '",
                    category,
                    "') insert into drafts (author_id,short_title,date_of_changes,category_id,draft_text) values ((select author_id from get_a),'",
                    short'_title,
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
        result <- loadImagesForDraft' hLogger (fromOnly $ Prelude.head d_rows) main'_image images_list
        close conn
        case result of
            Left bs -> do
                logError hLogger "Images not loaded"
                -- delete draft
                return $ Left bs
            --Right bs -> do
            Right _ -> do
                logInfo hLogger "Images  loaded"
                -- delete draft
                return $ Right $ fromOnly $ Prelude.head d_rows-}

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

{-tstCDrfats :: IO (Either LBS.ByteString Int)
tstCDrfats = createDraftOnDb' (Handle Debug printLog) (Just "dahaku202108020903181300795") (Just "наука") (Just "") (Just "test_serv") (Just "test text") (Just $ Image''' "5" "6" (Binary "7")) (Just [])-}




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

--ДОБАВИТЬ ДОБАВЛЕНИЕ СПИСКА ТЕГОВ
createDraftOnDb :: Handle -> Pool Connection -> TokenLifeTime -> Maybe BC.ByteString  -> Maybe BC.ByteString -> Maybe BC.ByteString -> Maybe BC.ByteString -> Maybe BC.ByteString -> Maybe Image''' -> Maybe [Image'''] -> IO (Either LBS.ByteString Int)
createDraftOnDb hLogger _ _ Nothing _ _ _ _ _ _ = do
    logError hLogger "No token param"
    return $ Left "No token param"
createDraftOnDb hLogger _ _ _ Nothing _ _ _ _ _ = do
    logError hLogger "No category field"
    return $ Left "No category field"
createDraftOnDb hLogger _ _ _ _ Nothing _ _ _ _ = do
    logError hLogger "No tags field"
    return $ Left "No tags field"
createDraftOnDb hLogger _ _ _ _ _ Nothing _ _ _ = do
    logError hLogger "No short_title field"
    return $ Left "No short_title field"
createDraftOnDb hLogger _ _ _ _ _ _ Nothing _ _ = do
    logError hLogger "No text field"
    return $ Left "No text field"
createDraftOnDb hLogger pool token_lifetime (Just token') (Just category) (Just tags_list) (Just short'_title) (Just text) main'_image images_list = do
    logInfo hLogger "Someone try add new draft"
    --conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    let q =  toQuery $ BC.concat ["with get_a as (select author_id from authors join tokens using (user_id) where token = '",
                    token', "' and (now() - tokens.creation_date) < make_interval(secs => ", BC.pack $ show token_lifetime,
                    ")), get_c as (select category_id from categories where category_name = '",
                    category,
                    "'), d_id as (insert into drafts (author_id,short_title,date_of_changes,category_id,draft_text) values ((select author_id from get_a),'",
                    short'_title,
                    "',now(),(select category_id from get_c),'",
                    text,
                    "') returning draft_id), ",
                    "t_id as (select tag_id from tags where tag_name in (",
                    tagsToQueryTagList tags_list,")), ",
                    "d_t as (select * from d_id cross join t_id) ",
                    "insert into draft_tags (draft_id, tag_id) select * from d_t"]
                    --"d_t_in as (insert into draft_tags (draft_id, tag_id) select * from d_t returning 1) ",
                    --"select * from d_id"]
    {-let q =  toQuery $ BC.concat ["with get_a as (select author_id from authors join tokens using (user_id) where token = '",
                    token', "' and (now() - tokens.creation_date) < make_interval(secs => ", BC.pack $ show token_lifetime,
                    ")), get_c as (select category_id from categories where category_name = '",
                    category,
                    "') insert into drafts (author_id,short_title,date_of_changes,category_id,draft_text) values ((select author_id from get_a),'",
                    short'_title,
                    "',now(),(select category_id from get_c),'",
                    text,
                    "') returning draft_id"]-}
    {-with a_id as (select author_id from authors where user_id = 1),
c_id as (select category_id from categories where category_name = 'музыка'),
d_id as (insert into drafts (author_id,short_title,date_of_changes,category_id,draft_text)
		values ((select * from a_id),'testWith1',now(),(select * from c_id),'tsttext') returning draft_id),
t_ids as (select tag_id from tags where tag_name in ('новости','наука')),
d_t as (select * from d_id cross join t_ids)
insert into draft_tags (draft_id,tag_id) select * from d_t;
select * from drafts-}
    d_rows <- query_WithPool pool q  :: IO [Only Int]
    if Prelude.null d_rows then do
        --close conn
        logError hLogger "Draft not created"
        return $ Left "Draft not created"
    else do
        result <- loadImagesForDraft' hLogger pool (fromOnly $ Prelude.head d_rows) main'_image images_list
        --close conn
        case result of
            Left bs -> do
                logError hLogger "Images not loaded"
                simpleDeleteDraft hLogger pool (fromOnly $ Prelude.head d_rows) "Draft not created"
                return $ Left bs
            Right _ -> do
                logInfo hLogger "Images  loaded"
                return $ Right $ fromOnly $ Prelude.head d_rows

--simpleDeleteDraft :: Pool Connection -> Int 
simpleDeleteDraft :: Handle -> Pool Connection -> Int -> T.Text -> IO ()
simpleDeleteDraft hLogger pool draft_id mes= do
    n <- executeWithPool pool "delete from drafts where draft_id = ?" [draft_id]
    if n > 0 then
        logError hLogger mes
    else
        logError hLogger "draft not deleted"


loadImageAndCreateDraftConnectionOn :: Handle -> Pool Connection -> Int-> [Image'''] -> IO (Either LBS.ByteString Int)
loadImageAndCreateDraftConnectionOn hLogger pool d_id iml = catch (do
    --let im = Image'''' "123" "123" (Binary "123") 15
    --let imm = [] :: [Image'''']
    --let i = Image''' "5" "6" (Binary "7") 
    --let q = toQuery $ BC.concat["with add_image as (insert into images (image_name,content_type, image_b) values (?,?,?) returning image_id) insert into drafts_images (draft_id, image_id) values ( ",BC.pack $ show d_id," , (select image_id from add_image)) returning image_id"]
    --conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    n <- returningWithPool pool q iml :: IO [Only Int]
    if Prelude.null n then
        return $ Left ""
    else
        return $ Right $ fromOnly $ Prelude.head n) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"
    where
        q = toQuery $ BC.concat["with add_image as (insert into images (image_name,content_type, image_b) values (?,?,?) returning image_id) ",
                                "insert into drafts_images (draft_id, image_id) select ",BC.pack $ show d_id," , image_id from add_image ",
                                "returning image_id"]



{-tstMegaQ :: IO (Either LBS.ByteString Int)
tstMegaQ = loadImageAndCreateDraftConnectionOn (Handle Debug printLog) 16 [(Image''' "5" "6" (Binary "7"))]-}

{-toImage'''' :: Int -> Image''' -> Image''''
toImage'''' d_id image = Image'''' (f_name'' image) (content_type'' image) (content'' image) d_id-}

loadImagesForDraft' :: Handle -> Pool Connection -> Int -> Maybe Image''' -> Maybe [Image'''] -> IO (Either LBS.ByteString LBS.ByteString)
loadImagesForDraft' hLogger pool draft_id Nothing (Just image) = catch (do
    result <- loadImageAndCreateDraftConnectionOn hLogger pool draft_id image
    case result of
        Left bs -> return $ Left bs
        --Right n -> do 
        Right _ -> do
            logInfo hLogger "Draft created"
            return $ Right "Draft created") $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"
loadImagesForDraft' hLogger _ _ Nothing Nothing = do
    logInfo hLogger "Draft created"
    return $ Right "Draft created"
loadImagesForDraft' hLogger pool draft_id (Just i) Nothing = catch (do
    result <- loadImageAndCreateDraftConnectionOn hLogger pool draft_id [i]
    case result of
      Left bs -> return $ Left bs
      Right n -> do
            --conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
            --let q = "update drafts set main_image = ? where draft_id = ?"
            k <- executeWithPool pool "update drafts set main_image = ? where draft_id = ?" (n,draft_id)
            if k > 0 then do
                logInfo hLogger "Draft created"
                return $ Right "Draft created"
            else return $ Left "Draft not created") $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"
loadImagesForDraft' hLogger pool draft_id (Just i) (Just ii) = catch (do
    result <- loadImageAndCreateDraftConnectionOn hLogger pool draft_id (i:ii)
    case result of
      Left bs -> return $ Left bs
      Right n -> do
            --conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
            --let q = "update  drafts set main_image = ? where draft_id = ?"
            k <- executeWithPool pool "update  drafts set main_image = ? where draft_id = ?" (n,draft_id)
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
deleteDraftFromDb hLogger author'_id (Just draft_id) = catch (do
    let draft_id_int = readByteStringToInt draft_id
    case draft_id_int of
        Nothing -> do
            logError hLogger "Bad draft_id parameter"
            return $ Left "Bad draft_id parameter"
        Just n -> do
            conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
            let q = "delete from drafts where draft_id = ? and author_id = ?"
            ne <- execute conn q (n,author'_id)
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


{-getDraftByIdFromDb :: Handle -> Int -> Int -> IO (Either LBS.ByteString Draft')
getDraftByIdFromDb hLogger author'_id draft_id = catch (do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    --let q = "select short_title, date_of_changes, category_id, draft_text, main_image, array_agg(image_id) from drafts join drafts_images using (draft_id) where author_id = ? and draft_id = ? group by draft_id"
    let q = "with image_arr as (select array_agg(image_id) from drafts_images where draft_id = ?) select short_title, date_of_changes, category_id, draft_text, main_image, (select * from image_arr) from drafts where author_id = ? and draft_id = ?"
    rows <- query conn q (draft_id,author'_id,draft_id) :: IO [Draft']
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
                                        return $ Left "Database error"-}
getDraftByIdFromDb :: Handle -> Pool Connection -> TokenLifeTime -> Maybe T.Text -> Int -> IO (Either LBS.ByteString Draft'')
getDraftByIdFromDb hLogger _ _ Nothing _ = do
    logError hLogger "No token parameter"
    return $ Left "No Token parameter"
getDraftByIdFromDb hLogger pool token_lifetime token' draft_id = do
    ch_author <- checkAuthor' hLogger pool token_lifetime token'
    case ch_author of
        Left bs -> return $ Left bs
        Right author_id -> catch (do
        --let q = "with image_arr as (select array_agg(image_id) from drafts_images where draft_id = ?) select short_title, date_of_changes, category_id, draft_text, main_image, (select * from image_arr) from drafts where author_id = ? and draft_id = ?"
        --rows <- queryWithPool pool q [author_id] :: IO [Draft']
        rows <- queryWithPool pool q [author_id] :: IO [Draft'']
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
    where q = toQuery $ BC.concat ["with image_arr as (select array_agg(image_id) from drafts_images where draft_id = ",BC.pack $ show draft_id,"), ",
                                  " tags_arr as (select array_agg(tag_name) from draft_tags join tags using (tag_id) where draft_id = ",BC.pack $ show draft_id,") " ,
                                  "select short_title, date_of_changes, category_id, draft_text, main_image, ",
                                  "(select * from image_arr) as other_images, (select * from  tags_arr) as tags from drafts where author_id = ? and draft_id = ",BC.pack $ show draft_id]
{-tstGetD :: IO (Either LBS.ByteString Draft')
tstGetD = getDraftByIdFromDb (Handle Debug printLog) 12 22-}

{-getDraftIdsByAuthor :: Handle -> Int -> IO (Either LBS.ByteString  [Int])
getDraftIdsByAuthor hLogger author'_id = do
    logInfo hLogger "Getting drafts"
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    let q = "select draft_id from drafts where author_id = ?"
    rows <- query conn q [author'_id] :: IO [Only Int]
    if Prelude.null rows then
        return $ Left "No drafts"
    else
        return $ Right (fromOnly <$> rows)-}


{--updateDraftInDb'hLogger token category tags short_title text main_image images_list draft_id
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
updateDraftInDb' hLogger (Just token') (Just category) (Just tags) (Just short'_title) (Just text) main'_image images_list draft_id author'_id  = catch (do
    pool <- tstPool --переработать
    new_rows <- createDraftOnDb hLogger pool 8 (Just token') (Just category) (Just tags) (Just short'_title) (Just text) main'_image images_list -- перерабботать
    case new_rows of
        --Left bs -> do
        Left _ -> do
            logError hLogger "Draft not updated"
            return $ Left "Draft not updated"
        Right n -> do
            let q_update_id = "with dr_i as (delete from drafts where draft_id = ? and author_id = ? returning draft_id) update drafts set draft_id = (select draft_id from dr_i) where draft_id = ?"
            conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
            nu <- execute conn q_update_id (draft_id,author'_id,n)
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
                                        return $ Left "Database error"-}


updateDraftInDb :: Handle -> Pool Connection -> TokenLifeTime -> Maybe BC.ByteString -> Maybe T.Text -> Maybe BC.ByteString -> Maybe T.Text -> Maybe T.Text -> Maybe Image''' -> Maybe [Image'''] -> Int -> IO (Either LBS.ByteString LBS.ByteString)
updateDraftInDb hLogger _ _ Nothing _ _ _ _ _ _ _  = do
    logError hLogger "No token param"
    return $ Left "No token param"
updateDraftInDb hLogger _ _ _ Nothing _ _ _ _ _ _ = do
    logError hLogger "No category field"
    return $ Left "No category field"
updateDraftInDb hLogger _ _ _ _ Nothing _ _ _ _ _  = do
    logError hLogger "No tags field"
    return $ Left "No tags field"
updateDraftInDb hLogger _ _ _ _ _ Nothing _ _ _ _ = do
    logError hLogger "No short_title field"
    return $ Left "No short_title field"
updateDraftInDb hLogger _ _ _ _ _ _ Nothing _ _ _ = do
    logError hLogger "No text field"
    return $ Left "No text field"
updateDraftInDb hLogger pool token_lifetime (Just token') (Just category) (Just tags) (Just short'_title) (Just text) main'_image images_list draft_id = catch (do
    new_rows <- createDraftOnDb' hLogger pool token_lifetime (Just token') (Just category) (Just tags) (Just short'_title) (Just text) main'_image images_list
    case new_rows of
        Left _ -> do
            logError hLogger "Draft not updated"
            return $ Left "Draft not updated"
        Right n -> do
            let q_update = toQuery $ BC.concat ["with a_id as (select author_id from authors join users using(user_id) join tokens using(user_id) ",
                                           "where token = '",token',"' and (now() - tokens.creation_date) < make_interval(secs => ",
                                           BC.pack $ show token_lifetime,")), ",
                                           "with dr_i as (delete from drafts where draft_id = ",BC.pack $ show draft_id,
                                           " and author_id = (select author_id from a_id) returning draft_id) ",
                                           "update drafts set draft_id = (select draft_id from dr_i) where draft_id = ",BC.pack $ show n]
            nu <- execute_WithPool pool q_update
            if nu > 0 then do
                logInfo hLogger $ T.concat ["Draft ",T.pack $ show draft_id, " updated"]
                return $ Right "Draft updated"
            else do
                _ <- simpleDeleteDraft hLogger pool n "update deleted" --simpleDeleteDraft hLogger pool n "update deleted"
                logError hLogger $ T.concat ["Draft ",T.pack $ show draft_id, "not updated"]
                return $ Left "Draft not updated") $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"



{-publicNewsOnDb :: Handle -> Int -> Int -> IO (Either LBS.ByteString Int)
publicNewsOnDb hLogger author'_id draft_id = catch( do
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
    n_id <- query conn q (draft_id,author'_id,now) :: IO [Only Int]
    if Prelude.null n_id then do
        close conn
        return $ Left "News not published"
    else do
        check <- createConnectionImagesNews (fromOnly $ Prelude.head n_id) draft_id
        case check of
            --Left bs -> do
            Left _ -> do
                logError hLogger "News not published"
                return $ Left "News not published"
            --Right bs -> do
            Right _ -> do
                logInfo hLogger "News published"
                return $ Right (fromOnly $ Prelude.head n_id)) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"-}


publicNewsOnDb :: Handle -> Pool Connection -> Int -> Int -> IO (Either LBS.ByteString Int)
publicNewsOnDb hLogger pool author'_id draft_id = catch( do
    let q = toQuery $ BC.concat ["with draft_s as (delete from drafts where draft_id = ? returning short_title,author_id,category_id,draft_text,main_image))",
                                "insert into news (short_title,author_id,category_id,news_text,main_image,date_creation)",
                                "values ((select short_title from draft_s),",
                                "(select author_id from draft_s),",
                                "(select category_id from draft_s),",
                                "(select draft_text from draft_s),",
                                "(select main_image from draft_s),",
                                "?) returning news_id"]
    now <- getCurrentTime
    n_id <- queryWithPool pool q (draft_id,author'_id,now) :: IO [Only Int]
    if Prelude.null n_id then do
        --close conn
        return $ Left "News not published"
    else do
        check <- createConnectionImagesNews pool (fromOnly $ Prelude.head n_id) draft_id
        case check of
            --Left bs -> do
            Left _ -> do
                logError hLogger "News not published"
                deletingNewsById hLogger pool (fromOnly $ Prelude.head n_id)
                return $ Left "News not published"
            --Right bs -> do
            Right _ -> do
                logInfo hLogger "News published"
                return $ Right (fromOnly $ Prelude.head n_id)) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"


createConnectionImagesNews :: Pool Connection -> Int -> Int -> IO (Either LBS.ByteString LBS.ByteString)
createConnectionImagesNews pool news'_id draft_id = do
    let q = "with d_s as (select image_id from drafts_images where draft_id = ?) insert into news_image (news_id,image_id) values (?, (select image_id from d_s))"
    --conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    n <- executeWithPool pool q (draft_id,news'_id)
    if n > 0 then do
        return $ Right ""
    else
        return $ Left ""
deletingNewsById :: Handle -> Pool Connection -> Int -> IO ()
deletingNewsById hLogger pool news_id = do
    let q = "delete from news where news_id = ?"
    _ <- executeWithPool pool q [news_id] --executeWithPool pool q [news_id]
    logInfo hLogger "news proto deleted"










getDraftByIdFromDb' :: Handle -> Int -> Int -> IO (Either LBS.ByteString Draft')
getDraftByIdFromDb' hLogger author'_id draft_id = catches (do
    conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    --let q = "select short_title, date_of_changes, category_id, draft_text, main_image, array_agg(image_id) from drafts join drafts_images using (draft_id) where author_id = ? and draft_id = ? group by draft_id"
    let q = "with image_arr as (select array_agg(image_id) from drafts_images where draft_id = ?) select short_title, date_of_changes, category_id, draft_text, main_image, (select * from image_arr) from drafts where author_id = ? and draft_id = ?"
    rows <- query conn q (draft_id,author'_id,draft_id) :: IO [Draft']
    close conn
    if Prelude.null rows then do
        logError hLogger "Wrong draft id or draft not exist"
        return $ Left "Wrong draft id or draft not exist"
    else do
        logInfo hLogger "Sending draft to user"
        return $ Right $ Prelude.head rows) $ dberrors hLogger --[Handler (\ (e :: SqlError) -> catchSqlError hLogger e)]
                                    {-\e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"-}


getTagsListFromDb :: Handle -> Pool Connection -> Maybe ByteString -> IO (Either LBS.ByteString TagsList)
getTagsListFromDb hLogger pool maybe_page = catch (do
    logInfo hLogger "Sending tags list"
    rows <- query_WithPool pool q
    return $ Right $ TagsList rows) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"
    where
        pg = if isNothing maybe_page then " limit 10 offset 0"
                else
                    BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" maybe_page)) - 1)*10 ]
        q = toQuery $ BC.concat [ "select tag_name from tags order by tag_name",pg]


createTagInDb :: Handle -> Pool Connection -> Maybe T.Text ->  IO (Either LBS.ByteString Int)
createTagInDb hLogger _  Nothing = do
    logError hLogger "No tag_name parameter"
    return $ Left "No tag_name parameter"
createTagInDb hLogger pool (Just tag_name') = catch (do
    logInfo hLogger "Creating new tag"
    rows <- queryWithPool pool q [tag_name'] :: IO [Only Int]
    if Prelude.null rows then do
        logError hLogger "Tag not created"
        return $ Left "Tag not created"
    else do
        logInfo hLogger "Tag created"
        return $ Right $ fromOnly $ Prelude.head rows) $  \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, T.pack $ show errStateInt]
                                        case errStateInt of
                                            23505 -> return $ Left "Tag already exist"
                                            _ -> return $ Left "Database error"
    where
        q = toQuery $ BC.concat ["insert into tags (tag_name) values (?) returning tag_id"]


deleteTagFromDb :: Handle -> Pool Connection -> Maybe T.Text -> IO (Either LBS.ByteString LBS.ByteString)
deleteTagFromDb hLogger _  Nothing = do
    logError hLogger "No tag_name parameter"
    return $ Left "No tag_name parameter"
deleteTagFromDb hLogger pool (Just tag_name') = catch (do
    logInfo hLogger $ T.concat ["Deleting tag ", tag_name']
    n <- executeWithPool pool q [tag_name']
    if n > 0 then do
        logInfo hLogger $ T.concat ["Tag ", tag_name', " deleted"]
        return $ Right "Tag deleted"
    else do
        logError hLogger $ T.concat ["Tag ", tag_name', " not deleted"]
        return $ Right "Tag not deleted") $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"
    where
        q = toQuery $ BC.concat ["delete from tags where tag_name = ?"]



catchSqlError :: Handle -> SqlError -> IO (Either LBS.ByteString a)
catchSqlError hLogger e = do
    let err = E.decodeUtf8 $ sqlErrorMsg e
    let errStateInt = fromMaybe 0 (readByteStringToInt $ sqlState e)
    logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
    return $ Left "Database error"

dberrors :: Handle -> [Handler (Either LBS.ByteString a)]
dberrors hLogger = [Handler (\ (e :: SqlError) -> catchSqlError hLogger e)]

tstNewErr :: IO (Either LBS.ByteString Draft')
tstNewErr = getDraftByIdFromDb' (Handle Debug printLog) 12 155




{-createDraftTagsConnections :: Handle -> Pool Connection -> Int -> BC.ByteString ->  IO (Either LBS.ByteString LBS.ByteString)
createDraftTagsConnections hLogger pool draft_id tags = do
    n <- execute_WithPool pool q
    return $ Left ""
    where q = toQuery $ BC.concat []-}

getTagsIds :: Handle -> Pool Connection -> BC.ByteString -> IO (Either LBS.ByteString [Int])
getTagsIds hLogger pool tags_bs = do
    let n = Prelude.length $ BC.split ' ' tags_bs
    let l = BC.split ' ' tags_bs
    let q = toQuery $ BC.concat [ "select tag_id from tags where tag_name in ?"]
    rows <- queryWithPool pool q (Only (In l))
    if Prelude.length rows < n then do
        --print rows
        logError hLogger "Someone tags not exist"
        return $ Left "Someone tags not exist"
    else do
        return $ Right $ fromOnly <$> rows

{-tstgettags :: IO (Either LBS.ByteString [Int])
tstgettags = do
    pool <- tstPool
    let bs = E.encodeUtf8 "наука новости музыка"
    getTagsIds (Handle Debug printLog) pool bs-}


createDraftOnDb' :: Handle -> Pool Connection -> TokenLifeTime -> Maybe BC.ByteString  -> Maybe T.Text -> Maybe BC.ByteString -> Maybe T.Text -> Maybe T.Text -> Maybe Image''' -> Maybe [Image'''] -> IO (Either LBS.ByteString Int)
createDraftOnDb' hLogger _ _ Nothing _ _ _ _ _ _ = do
    logError hLogger "No token param"
    return $ Left "No token param"
createDraftOnDb' hLogger _ _ _ Nothing _ _ _ _ _ = do
    logError hLogger "No category field"
    return $ Left "No category field"
createDraftOnDb' hLogger _ _ _ _ Nothing _ _ _ _ = do
    logError hLogger "No tags field"
    return $ Left "No tags field"
createDraftOnDb' hLogger _ _ _ _ _ Nothing _ _ _ = do
    logError hLogger "No short_title field"
    return $ Left "No short_title field"
createDraftOnDb' hLogger _ _ _ _ _ _ Nothing _ _ = do
    logError hLogger "No text field"
    return $ Left "No text field"
createDraftOnDb' hLogger pool token_lifetime (Just token') (Just category) (Just tags_list) (Just short'_title) (Just text) main'_image images_list = do
    logInfo hLogger "Someone try add new draft"
    --return $ Left ""
    draft_id <- newDraft
    c <- createTagConnections draft_id
    l <- loadMainImage c main'_image
    loadImages l images_list
    --return ll
    --return $ Left ""
    where
        newDraft = catch (do
            {-let q =  toQuery $ BC.concat ["with get_a as (select author_id from authors join tokens using (user_id) where token = '",
                    token', "' and (now() - tokens.creation_date) < make_interval(secs => ", BC.pack $ show token_lifetime,
                    ")), get_c as (select category_id from categories where category_name = '",
                    category,
                    "') insert into drafts (author_id,short_title,date_of_changes,category_id,draft_text) values ((select author_id from get_a),'",
                    short'_title,
                    "',now(),(select category_id from get_c),'",
                    text,
                    "') returning draft_id"]
            
            rows <- query_WithPool pool q :: IO [Only Int]-}
            let q =  toQuery $ BC.concat ["with get_a as (select author_id from authors join tokens using (user_id) where token = '",
                    token', "' and (now() - tokens.creation_date) < make_interval(secs => ", BC.pack $ show token_lifetime,
                    ")), get_c as (select category_id from categories where category_name = ?) ",
                    "insert into drafts (author_id,short_title,date_of_changes,category_id,draft_text) values ((select author_id from get_a),",
                    " ? ,now(),(select category_id from get_c),",
                    " ?) returning draft_id"]
            rows <- queryWithPool pool q (category, short'_title, text):: IO [Only Int]
            if Prelude.null rows then do
                logError hLogger "Draft not created"
                return $ Left "Draft not created"
            else do
                return $ Right $ fromOnly $ Prelude.head rows) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"

        createTagConnections (Left message) = return $ Left message
        createTagConnections (Right draft_id) = catch (do
            tag_ids <- getTagsIds hLogger pool tags_list
            case tag_ids of
                Left bs -> return $ Left bs
                Right ns -> do
                    let q = toQuery $ BC.concat ["insert into draft_tags (draft_id,tag_id) values (?,?)"]
                    --let a = Prelude.map (\ x -> (draft_id, x)) ns
                    let a = Prelude.map (draft_id,) ns
                    let nt = Prelude.length ns
                    n <- executeManyWithPool pool q a
                    if fromIntegral n < nt then do
                        logError hLogger "Some tags not added"
                        return $ Left "Some tags not added"
                    else
                        return $ Right draft_id) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"

        loadMainImage (Left message) _ = return $ Left message
        loadMainImage (Right draft_id) Nothing = return $ Right draft_id
        loadMainImage (Right draft_id) (Just image) = catch (do
            let q = toQuery $ BC.concat ["with m_id as (insert into images (image_name,content_type, image_b) values (?,?,?) returning image_id) ",
                                         "update drafts set main_image = (select * from m_id) where draft_id = ", BC.pack $ show draft_id ]
            n <- executeWithPool pool q image
            if n < 1 then do
                logError hLogger "Image not loaded"
                return $ Left "Image not loaded"
            else return $ Right draft_id) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"

        loadImages (Left message) _ = return $ Left message
        loadImages (Right draft_id) Nothing = return $ Right draft_id
        loadImages (Right draft_id) (Just images) = catch (do
            let q = toQuery $ BC.concat ["with m_id as (insert into images (image_name,content_type, image_b) values (?,?,?) returning image_id), ",
                                         "d_i as (select ", BC.pack $ show draft_id," as draft_id, image_id from m_id) ",
                                         "insert into drafts_images (draft_id,image_id) select * from d_i"]
            n <- executeManyWithPool pool q images
            if fromIntegral n < Prelude.length images then do
                logError hLogger "Images not loaded"
                return $ Left "Images not loaded"
            else do
                logInfo hLogger "Draft created"
                return $ Right draft_id) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"






    --conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
    {-let q =  toQuery $ BC.concat ["with get_a as (select author_id from authors join tokens using (user_id) where token = '",
                    token', "' and (now() - tokens.creation_date) < make_interval(secs => ", BC.pack $ show token_lifetime,
                    ")), get_c as (select category_id from categories where category_name = '",
                    category,
                    "'), d_id as (insert into drafts (author_id,short_title,date_of_changes,category_id,draft_text) values ((select author_id from get_a),'",
                    short'_title,
                    "',now(),(select category_id from get_c),'",
                    text,
                    "') returning draft_id), ",
                    "t_id as (select tag_id from tags where tag_name in (",
                    tagsToQueryTagList tags_list,")), ",
                    "d_t as (select * from d_id cross join t_id) ",
                    "insert into draft_tags (draft_id, tag_id) select * from d_t"]
    d_rows <- query_WithPool pool q  :: IO [Only Int]
    if Prelude.null d_rows then do
        --close conn
        logError hLogger "Draft not created"
        return $ Left "Draft not created"
    else do
        result <- loadImagesForDraft' hLogger pool (fromOnly $ Prelude.head d_rows) main'_image images_list
        --close conn
        case result of
            Left bs -> do
                logError hLogger "Images not loaded"
                simpleDeleteDraft hLogger pool (fromOnly $ Prelude.head d_rows) "Draft not created"
                return $ Left bs
            Right _ -> do
                logInfo hLogger "Images  loaded"
                return $ Right $ fromOnly $ Prelude.head d_rows-}
--func :: t -> [b] -> [(t, b)]
--func n xs = Prelude.map (\ x -> (n, x)) xs



updateDraftInDb' :: Handle -> Pool Connection -> TokenLifeTime -> Maybe BC.ByteString -> Maybe T.Text -> Maybe BC.ByteString -> Maybe T.Text -> Maybe T.Text  -> Maybe Image''' -> Maybe [Image'''] -> Int -> IO (Either LBS.ByteString LBS.ByteString)
updateDraftInDb' hLogger _ _ Nothing _ _ _ _ _ _ _  = do
    logError hLogger "No token param"
    return $ Left "No token param"
updateDraftInDb' hLogger _ _ _ Nothing _ _ _ _ _ _ = do
    logError hLogger "No category field"
    return $ Left "No category field"
updateDraftInDb' hLogger _ _ _ _ Nothing _ _ _ _ _  = do
    logError hLogger "No tags field"
    return $ Left "No tags field"
updateDraftInDb' hLogger _ _ _ _ _ Nothing _ _ _ _ = do
    logError hLogger "No short_title field"
    return $ Left "No short_title field"
updateDraftInDb' hLogger _ _ _ _ _ _ Nothing _ _ _ = do
    logError hLogger "No text field"
    return $ Left "No text field"
updateDraftInDb' hLogger pool token_lifetime (Just token') (Just category) (Just tags_list) (Just short'_title) (Just text) main'_image images_list draft_id = do
    logInfo hLogger "Someone try update draft"
    --return $ Left ""
    u <- updateDraft
    dt <- deleteTagConnections u
    ct <- createTagConnections dt
    dmi <- deleteMainImage ct main'_image
    l <- loadMainImage dmi main'_image
    di <- deleteOldImages l images_list
    loadImages di images_list
    --categoryUpdate li (Just category)
    ---добавить апдейт категории
    --return ll
    --return $ Left ""
    where
        updateDraft = catch (do
            logDebug hLogger "Update drafts"
            {-let q =  toQuery $ BC.concat ["with get_a as (select author_id from authors join tokens using (user_id) where token = '",
                    token', "' and (now() - tokens.creation_date) < make_interval(secs => ", BC.pack $ show token_lifetime,
                    ")), get_c as (select category_id from categories where category_name = '",
                    category,
                    "') update drafts set short_title = '",short'_title,"', date_of_changes = now(), category_id = (select * from get_c), ",
                    "draft_text = '",text,"' where draft_id = ",BC.pack $ show draft_id] 
            n <- execute_WithPool pool q-}
            let q =  toQuery $ BC.concat ["with get_a as (select author_id from authors join tokens using (user_id) where token = '",
                    token', "' and (now() - tokens.creation_date) < make_interval(secs => ", BC.pack $ show token_lifetime,
                    ")), get_c as (select category_id from categories where category_name = ?) ",
                    "update drafts set short_title = ?, date_of_changes = now(), category_id = (select * from get_c), ",
                    "draft_text = ? where draft_id = ",BC.pack $ show draft_id]
            n <- executeWithPool pool q (category, short'_title,text)
            if n < 1 then do
                logError hLogger "Draft not updated"
                return $ Left "Draft not updated"
            else do
                logDebug hLogger "Update drafts complete"
                return $ Right "Draft updated") $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"
        deleteTagConnections (Left mess) = return $ Left mess
        deleteTagConnections (Right mess) = catch (do
            logDebug hLogger "Deleting old tags"
            let q = toQuery $ BC.concat ["delete from draft_tags where draft_id = ",BC.pack $ show draft_id]
            n <- execute_WithPool pool q
            if n < 1 then do
                logError hLogger "Draft tags not updated"
                return $ Left "Draft tags not updated"
            else do
                logDebug hLogger "Deleting old tags complete"
                return $ Right mess) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"

        createTagConnections (Left message) = return $ Left message
        createTagConnections (Right mess) = catch (do
            tag_ids <- getTagsIds hLogger pool tags_list
            case tag_ids of
                Left bs -> return $ Left bs
                Right ns -> do
                    logDebug hLogger "Add new tags"
                    let q = toQuery $ BC.concat ["insert into draft_tags (draft_id,tag_id) values (?,?)"]
                    --let a = Prelude.map (\ x -> (draft_id, x)) ns
                    let a = Prelude.map (draft_id,) ns
                    let nt = Prelude.length ns
                    n <- executeManyWithPool pool q a
                    if fromIntegral n < nt then do
                        logError hLogger "Some tags not added"
                        return $ Left "Some tags not added"
                    else do
                        logDebug hLogger "Add new tags complete"
                        return $ Right mess) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"
        deleteMainImage (Left mess) _= return $ Left mess
        deleteMainImage (Right mess) Nothing = return $ Right mess
        deleteMainImage (Right mess) (Just _)= catch (do
            logDebug hLogger "Deleting old main image"
            let q = toQuery $ BC.concat ["with m_id as (select main_image from drafts where draft_id = ",BC.pack $ show draft_id,") ",
                                         "delete from images where image_id = (select * from m_id)"]
            _ <- execute_WithPool pool q
            --if n < 1 then do
             --   logError hLogger "Main image not updated"
              --  return $ Left "Main image not updated"
            --else do
            logDebug hLogger "Deleting old main image complete"
            return $ Right mess) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"

        loadMainImage (Left mess) _ = return $ Left mess
        loadMainImage (Right mess) Nothing = return $ Right mess
        loadMainImage (Right mess) (Just image) = catch (do
            logDebug hLogger "Add new main image"
            let q = toQuery $ BC.concat ["with m_id as (insert into images (image_name,content_type, image_b) values (?,?,?) returning image_id) ",
                                         "update drafts set main_image = (select * from m_id) where draft_id = ", BC.pack $ show draft_id ]
            n <- executeWithPool pool q image
            if n < 1 then do
                logError hLogger "Image not loaded"
                return $ Left "Image not loaded"
            else do
                logDebug hLogger "Add new main image"
                return $ Right mess) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"
        deleteOldImages (Left mess) _ = return $ Left mess
        deleteOldImages (Right mess) Nothing = return $ Right mess
        deleteOldImages (Right mess) (Just _) = catch (do
            logDebug hLogger "Deleting old images"
            let q = toQuery $ BC.concat ["delete from images where image_id in (select image_id from drafts_images where draft_id = ",BC.pack $ show draft_id,")"]
            _ <- execute_WithPool pool q
            --if n < 1 then do
            --    logError hLogger "Other images not updated"
            --    return $ Left "Other images not updated"
            --else do
            logDebug hLogger "Deleting old images complete"
            return $ Right mess) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error" 

        loadImages (Left mess) _ = return $ Left mess
        loadImages (Right mess) Nothing = return $ Right mess
        loadImages (Right mess) (Just images) = catch (do
            logDebug hLogger "Add new images"
            let q = toQuery $ BC.concat ["with m_id as (insert into images (image_name,content_type, image_b) values (?,?,?) returning image_id), ",
                                         "d_i as (select ", BC.pack $ show draft_id," as draft_id, image_id from m_id) ",
                                         "insert into drafts_images (draft_id,image_id) select * from d_i"]
            n <- executeManyWithPool pool q images
            if fromIntegral n < Prelude.length images then do
                logError hLogger "Images not loaded"
                return $ Left "Images not loaded"
            else do
                logDebug hLogger "Add new images complete"
                --logInfo hLogger "New images added"
                return $ Right mess) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"
        {-categoryUpdate (Left mess) _ = return $ Left mess
        categoryUpdate (Right mess) Nothing = return $ Right mess
        categoryUpdate (Right mess) (Just "") = return $ Right mess
        categoryUpdate (Right mess) (Just cat) = catch (do 
            logDebug hLogger "Updating category"
            TIO.putStrLn $ E.decodeUtf8 cat
            --let q = toQuery $ BC.concat ["with cat as (select category_id from categories where category_name = '", cat,"') ",
            --                             "update drafts set category_id = (select * from cat) where draft_id = ", BC.pack $ show draft_id]
            let q = toQuery $ BC.concat ["with cat as (select category_id from categories where category_name = '", cat,"') ",
                                         "update drafts set category_id = (select * from cat) where draft_id = ", BC.pack $ show draft_id,
                                         " returning category_id"]
            rows <- query_WithPool pool q :: IO [Only Int]
            if Prelude.null rows then do
                logDebug hLogger "Category not updated"
                return $ Left "Category not updated"
            else do
                print $ fromOnly $ Prelude.head rows
                logDebug hLogger "Category updated"
                return $ Right mess) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"-}
    {-temp_draft_id <- createDraftOnDb' hLogger pool token_lifetime (Just token') (Just category) (Just tags) (Just short'_title) (Just text) main'_image images_list
    updateDraftId temp_draft_id
    --deleteOldDraft n
    
    where 
        updateDraftId (Left mess) = return $ Left mess
        updateDraftId (Right tdi) = catch (do
            let q = toQuery $ BC.concat ["with del_old as (delete from drafts where draft_id = ", BC.pack $ show draft_id,
                        " returning draft_id) update drafts set draft_id = (select * from del_old) where draft_id = ", BC.pack $ show tdi]
            n <- execute_WithPool pool q
            if n < 1 then do
                logError hLogger "Draft not updated"
                return $ Left "Draft not updated"
            else return $ Right "Draft updated") $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"-}

        {-deleteOldDraft (Left mess) = return $ Left mess
        deleteOldDraft (Right tdi) = catch (do
            let q = toQuery $ BC.concat ["delete from drafts where draft_id = ", BC.pack $ show tdi]
            n <- execute_WithPool pool q
            if n < 1 then do
                logError hLogger "Draft updated, but temp draft not cleared"
                return $ Left "Draft updated, but temp draft not cleared"
            else return $ Right "Draft updated") $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"-}

    {-new_rows <- createDraftOnDb' hLogger pool token_lifetime (Just token') (Just category) (Just tags) (Just short'_title) (Just text) main'_image images_list
    case new_rows of
        Left _ -> do
            logError hLogger "Draft not updated"
            return $ Left "Draft not updated"
        Right n -> do
            let q_update = toQuery $ BC.concat ["with a_id as (select author_id from authors join users using(user_id) join tokens using(user_id) ",
                                           "where token = '",token',"' and (now() - tokens.creation_date) < make_interval(secs => ",
                                           BC.pack $ show token_lifetime,")), ",
                                           "with dr_i as (delete from drafts where draft_id = ",BC.pack $ show draft_id,
                                           " and author_id = (select author_id from a_id) returning draft_id) ",
                                           "update drafts set draft_id = (select draft_id from dr_i) where draft_id = ",BC.pack $ show n]
            nu <- execute_WithPool pool q_update
            if nu > 0 then do
                logInfo hLogger $ T.concat ["Draft ",T.pack $ show draft_id, " updated"]
                return $ Right "Draft updated"
            else do
                _ <- simpleDeleteDraft hLogger pool n "update deleted" --simpleDeleteDraft hLogger pool n "update deleted"
                logError hLogger $ T.concat ["Draft ",T.pack $ show draft_id, "not updated"]
                return $ Left "Draft not updated") $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error" -}
{-publicNews :: Handle ->Pool Connection -> TokenLifeTime -> Int -> Request -> IO Response
publicNews hLogger pool token_lifetime draft_id req = do
    let token' = E.decodeUtf8 <$> takeToken req
    ca <- checkAuthor' hLogger pool token_lifetime token'
    case ca of
        Left bs -> return $ responseBadRequest bs
        Right author'_id -> do
                    result <- publicNewsOnDb hLogger pool author'_id draft_id
                    case result of
                      Left bs -> return $ responseBadRequest bs
                      Right n -> return $ responseOk $ LBS.fromStrict $ BC.pack $ show n-}


publicNewsOnDb' :: Handle -> Pool Connection -> Int -> Int -> IO (Either LBS.ByteString Int)
publicNewsOnDb' hLogger pool author'_id draft_id = do
    n <- insertNews
    t <- insertTags n
    insertImages t
    where   insertNews = catch (do
                logDebug hLogger "Adding news to db"
                {-let q = toQuery $ BC.concat ["with ds as (select short_title,author_id, category_id,draft_text,main_image from drafts where author_id = ",
                                            BC.pack $ show author'_id," and draft_id = ", BC.pack $ show draft_id," ) ",
                                            "insert into news (short_title, date_creation, author_id, category_id, news_text, main_image) values ",
                                            " ((select short_title from ds), now(), ",
                                            "(select author_id from ds), ",
                                            "(select category_id from ds), ",
                                            "(select draft_text from ds), ",
                                            "(select main_image from ds)) returning news_id"]
                rows <- query_WithPool pool q :: IO [Only Int]-}
                let q = toQuery $ BC.concat ["with dus as (select short_title,author_id, category_id,draft_text,main_image from drafts where author_id = ?",
                                            " and draft_id = ?) ",
                                            "insert into news (short_title, date_creation, author_id, category_id, news_text, main_image) values ",
                                            "((select short_title from dus), now(), ",
                                            "(select author_id from dus), ",
                                            "(select category_id from dus), ",
                                            "(select draft_text from dus), ",
                                            "(select main_image from dus)) returning news_id"]
                rows <- queryWithPool pool q (author'_id,draft_id)
                if Prelude.null rows then do
                    logError hLogger "News not published"
                    return $ Left "News not published"
                else do
                    logDebug hLogger "Adding news to db complete"
                    return $ Right $ fromOnly $ Prelude.head rows) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"
            insertTags (Left mess) = return $ Left mess
            insertTags (Right news_id) = catch (do
                logDebug hLogger "Adding news tags to db"
                let q = toQuery $ BC.concat ["with tags_ids as (select tag_id from draft_tags where draft_id = ", BC.pack $ show draft_id," ), ",
                                           "tag_n as (select ",BC.pack $ show news_id, " as news_id, tag_id from tags_ids) ",
                                           "insert into news_tags (news_id, tag_id) select * from tag_n"]
                _ <- execute_WithPool pool q
                logDebug hLogger "Adding news tags to db complete"
                return $ Right news_id) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"
            insertImages (Left mess) = return $ Left mess
            insertImages (Right news_id) = catch (do
                logDebug hLogger "Adding news images to db"
                let q = toQuery $ BC.concat ["with images_ids as (select image_id from drafts_images where draft_id = ",BC.pack $ show draft_id, "), ",
                                             "image_n as (select ", BC.pack $ show news_id, " as news_id, image_id from images_ids) ",
                                             "insert into news_images (news_id, image_id) select * from image_n"]
                _ <- execute_WithPool pool q
                logDebug hLogger "Adding news images to db complete"
                return $ Right news_id) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"

    --catch( do
    {-let q = toQuery $ BC.concat ["with draft_s as (delete from drafts where draft_id = ? returning short_title,author_id,category_id,draft_text,main_image))",
                                "insert into news (short_title,author_id,category_id,news_text,main_image,date_creation)",
                                "values ((select short_title from draft_s),",
                                "(select author_id from draft_s),",
                                "(select category_id from draft_s),",
                                "(select draft_text from draft_s),",
                                "(select main_image from draft_s),",
                                "?) returning news_id"]
    now <- getCurrentTime
    n_id <- queryWithPool pool q (draft_id,author'_id,now) :: IO [Only Int]
    if Prelude.null n_id then do
        --close conn
        return $ Left "News not published"
    else do
        check <- createConnectionImagesNews pool (fromOnly $ Prelude.head n_id) draft_id
        case check of
            --Left bs -> do
            Left _ -> do
                logError hLogger "News not published"
                deletingNewsById hLogger pool (fromOnly $ Prelude.head n_id)
                return $ Left "News not published"
            --Right bs -> do
            Right _ -> do
                logInfo hLogger "News published"
                return $ Right (fromOnly $ Prelude.head n_id)) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        let errState = sqlState e
                                        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                                        logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
                                        return $ Left "Database error"-}