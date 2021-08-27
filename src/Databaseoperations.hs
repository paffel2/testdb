{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Databaseoperations where

import Control.Exception (catch)
import Data.ByteString as B (ByteString, unpack)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe, isNothing)
import Data.Pool (Pool)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Time (UTCTime, getCurrentTime)
import Database.PostgreSQL.Simple
    ( Binary(Binary)
    , Connection
    , In(In)
    , Only(..)
    , SqlError(sqlErrorMsg, sqlState)
    , close
    , connectPostgreSQL
    , execute
    , fromBinary
    , query
    )
import HelpFunction
    ( readByteStringListInt
    , readByteStringToDay
    , readByteStringToInt
    , toQuery
    )
import Logger (Handle, logDebug, logError, logInfo)
import PostgreSqlWithPool
    ( executeManyWithPool
    , executeWithPool
    , execute_WithPool
    , queryWithPool
    , query_WithPool
    , returningWithPool
    )
import Types
    {-( Comment(Comment)
    , CommentArray(CommentArray)
    , Draft
    , DraftArray(DraftArray)
    , GetNews
    , Image
    , ListOfCategories(ListOfCategories)
    , NewsArray(NewsArray)
    , Profile
    , TagsList(TagsList)
    , TokenLifeTime
    , TokenProfile(TokenProfile)
    , ImageArray
    , ElemImageArray
    )-}

getNewsFromDb ::
       Handle
    -> Pool Connection
    -> ByteString
    -> Maybe ByteString
    -> IO (Either LBS.ByteString NewsArray)
getNewsFromDb hLogger pool sortParam pageParam =
    catch
        (do logInfo hLogger "Someone try get news list"
            rows <- query_WithPool pool q
            return $ Right (NewsArray rows)) $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        logError hLogger err
        return $ Left "Database error"
  where
    sort' =
        if sortParam == ""
            then ""
            else BC.concat [" order by ", sortParam, " DESC"]
    pg =
        if isNothing pageParam
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $
                       show $
                       (fromMaybe
                            1
                            (readByteStringToInt (fromMaybe "" pageParam)) -
                        1) *
                       10
                     ]
    q =
        toQuery $
        BC.concat
            [ "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id), news_text  from news join authors using (author_id) join users using (user_id)"
            , sort'
            , pg
            ]

getNewsByIdFromDb ::
       Handle
    -> Pool Connection
    -> Maybe Int
    -> IO (Either LBS.ByteString GetNews)
getNewsByIdFromDb hLogger _ Nothing = do
    logError hLogger "Bad news_id parameter"
    return $ Left "Bad news_id parameter"
getNewsByIdFromDb hLogger pool (Just news'_id) =
    catch
        (do logInfo hLogger $
                T.concat ["Someone try get news №", T.pack $ show news'_id]
            rows <- query_WithPool pool q
            if Prelude.null rows
                then do
                    logError hLogger $
                        T.concat
                            ["News №", T.pack $ show news'_id, " not exist"]
                    return $ Left "News not exist"
                else do
                    logInfo hLogger $
                        T.concat
                            ["Sending news with id = ", T.pack $ show news'_id]
                    return $ Right $ Prelude.head rows) $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        logError hLogger err
        return $ Left "Database error"
  where
    q =
        toQuery $
        BC.concat
            [ "with image_arr as (select array_agg(image_id) from news_images where news_id = "
            , BC.pack $ show news'_id
            , "), "
            , " tags_arr as (select array_agg(tag_name) from news_tags join tags using (tag_id) where news_id = "
            , BC.pack $ show news'_id
            , ") "
            , "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id), news_text, main_image, "
            , "(select * from image_arr) as other_images, (select * from  tags_arr) as tags from news join authors using (author_id) join users using (user_id) where news_id = "
            , BC.pack $ show news'_id
            ]

getCommentsByNewsIdFromDb ::
       Handle
    -> Pool Connection
    -> Maybe Int
    -> Maybe ByteString
    -> IO (Either LBS.ByteString CommentArray)
getCommentsByNewsIdFromDb hLogger _ Nothing _ = do
    logError hLogger "No news parameter"
    return $ Left "No news parameter"
getCommentsByNewsIdFromDb hLogger pool (Just news_id) page_p =
    catch
        (do logInfo hLogger $
                T.concat
                    [ "Someone try get comments from news №"
                    , T.pack $ show news_id
                    ]
            rows <- queryWithPool pool q [news_id]
            return (Right $ CommentArray rows)) $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ T.concat [err, " ", T.pack $ show errStateInt]
        case errStateInt of
            23503 -> return $ Left "News not exist"
            23502 -> return $ Left "Bad token"
            _ -> return $ Left "Database error"
  where
    pg =
        if isNothing page_p
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $
                       show $
                       (fromMaybe 1 (readByteStringToInt (fromMaybe "" page_p)) -
                        1) *
                       10
                     ]
    q =
        toQuery $
        BC.concat
            [ "select (concat(first_name, ' ', last_name)) as author_name, comment_text, comment_time, comment_id"
            , " from users_comments join users using (user_id) where news_id = ? order by comment_time"
            , pg
            ]

addCommentToDb ::
       Handle
    -> Pool Connection
    -> TokenLifeTime
    -> T.Text
    -> Maybe Int
    -> Maybe T.Text
    -> IO (Either LBS.ByteString LBS.ByteString)
addCommentToDb hLogger _ _ _ Nothing _ = do
    logError hLogger "No comment id parameter"
    return $ Left "No comment id parameter"
addCommentToDb hLogger _ _ _ _ Nothing = do
    logError hLogger "No comment parameter"
    return $ Left "No comment parameter"
addCommentToDb hLogger pool token_lifetime token' (Just newsId) (Just comment) =
    catch
        (do logInfo hLogger $
                T.concat
                    [ "Someone try add commentary to news "
                    , T.pack $ show newsId
                    ]
            now <- getCurrentTime
            let com = Comment token' token_lifetime comment newsId now
            n_r <- executeWithPool pool q com
            if n_r > 0
                then do
                    logInfo hLogger $
                        T.concat
                            [ "New commetary to news "
                            , T.pack $ show newsId
                            , " added"
                            ]
                    return $ Right "Comment added"
                else do
                    logError hLogger "Comment not added"
                    return $ Right "Comment not added") $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ T.concat [err, " ", T.pack $ show errStateInt]
        case errStateInt of
            23503 -> return $ Left "News not exist"
            23502 -> return $ Left "Bad token"
            _ -> return $ Left "Database error"
  where
    q =
        "insert into users_comments (user_id, comment_text,news_id,comment_time) values (check_token(?,?),?,?,?)"

deleteCommentFromDb ::
       Handle
    -> Pool Connection
    -> TokenLifeTime
    -> Maybe T.Text
    -> Maybe Int
    -> IO (Either LBS.ByteString LBS.ByteString)
deleteCommentFromDb hLogger _ _ _ Nothing = do
    logError hLogger "Bad comment id"
    return $ Left "Bad comment id"
deleteCommentFromDb hLogger pool token_lifetime token' (Just comment_id) = do
    isAdmin <- checkAdmin' hLogger pool token_lifetime token'
    case isAdmin of
        (False, e) -> return $ Left e
        (True, _) ->
            catch
                (do n <-
                        executeWithPool
                            pool
                            "delete from users_comments where comment_id = ?"
                            [comment_id]
                    if n > 0
                        then do
                            logInfo hLogger "Comment deleted"
                            return $ Right "Comment deleted"
                        else do
                            logError hLogger "Comment not deleted"
                            return $ Left "Comment not deleted") $ \e -> do
                let err = E.decodeUtf8 $ sqlErrorMsg e
                logError hLogger err
                return $ Left "Database error"

checkAdmin' ::
       Handle
    -> Pool Connection
    -> TokenLifeTime
    -> Maybe T.Text
    -> IO (Bool, LBS.ByteString)
checkAdmin' hLogger _ _ Nothing = do
    logError hLogger "No token parameter"
    return (False, "No token parameter")
checkAdmin' hLogger pool token_liferime (Just token') =
    catch
        (do rows <-
                queryWithPool
                    pool
                    "select admin_mark from users join tokens using (user_id) where token = ? and ((current_timestamp - tokens.creation_date) < make_interval(secs => ?))"
                    (token', token_liferime)
            if Prelude.null rows
                then do
                    logError hLogger "Bad token"
                    return (False, "Bad token")
                else do
                    let admin'_mark = fromOnly $ Prelude.head rows
                    if admin'_mark
                        then return (admin'_mark, "")
                        else return (admin'_mark, "Not admin")) $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        logError hLogger err
        return (False, "Database error")

getNewsFilterByTagInFromDb ::
       Handle
    -> Pool Connection
    -> Maybe ByteString
    -> Maybe ByteString
    -> IO (Either LBS.ByteString NewsArray)
getNewsFilterByTagInFromDb hLogger _ Nothing _ = do
    logError hLogger "No tag parameter"
    return $ Left "No tag parameter"
getNewsFilterByTagInFromDb hLogger pool (Just tag_lst) page_p = do
    logInfo hLogger "Someone try get news list filtered by tag_in parameter"
    case readByteStringListInt tag_lst of
        Nothing -> do
            logError hLogger "Bad tag parameter"
            return $ Left "bad tag parameter"
        Just n ->
            catch
                (do rows <- queryWithPool pool q (Only (In n))
                    return (Right $ NewsArray rows)) $ \e -> do
                let err = E.decodeUtf8 $ sqlErrorMsg e
                logError hLogger err
                return $ Left "Database error"
  where
    pg =
        if isNothing page_p
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $
                       show $
                       (fromMaybe 1 (readByteStringToInt (fromMaybe "" page_p)) -
                        1) *
                       10
                     ]
    q =
        toQuery $
        BC.concat
            [ "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, "
            , "take_categories_list(category_id), news_text from news join authors using (author_id) "
            , "join users using (user_id) join news_tags using (news_id) where tag_id in ? "
            , "group by news_id,author_name order by 2 DESC"
            , pg
            ]

getNewsFilterByCategoryIdFromDb ::
       Handle
    -> Pool Connection
    -> Maybe ByteString
    -> Maybe ByteString
    -> ByteString
    -> IO (Either LBS.ByteString NewsArray)
getNewsFilterByCategoryIdFromDb hLogger _ Nothing _ _ = do
    logError hLogger "No category parameter"
    return $ Left "No category parameter"
getNewsFilterByCategoryIdFromDb hLogger pool (Just cat_id) page_p sortParam = do
    logInfo hLogger "Someone try get news list filtered by category id"
    case readByteStringToInt cat_id of
        Nothing -> do
            logError hLogger "Bad category parameter"
            return $ Left "Bad category parameter"
        Just n ->
            catch
                (do rows <- queryWithPool pool q [n]
                    return (Right $ NewsArray rows)) $ \e -> do
                let err = E.decodeUtf8 $ sqlErrorMsg e
                logError hLogger err
                return $ Left "Database error"
  where
    sort' =
        if sortParam == ""
            then ""
            else BC.concat [" order by ", sortParam, " DESC"]
    pg =
        if isNothing page_p
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $
                       show $
                       (fromMaybe 1 (readByteStringToInt (fromMaybe "" page_p)) -
                        1) *
                       10
                     ]
    q =
        toQuery $
        BC.concat
            [ "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, "
            , "take_categories_list(category_id), news_text  from news  join authors using (author_id) join users using (user_id) "
            , "where category_id = ? "
            , sort'
            , pg
            ]

getNewsFilterByTitleFromDb ::
       Handle
    -> Pool Connection
    -> Maybe ByteString
    -> Maybe ByteString
    -> ByteString
    -> IO (Either LBS.ByteString NewsArray)
getNewsFilterByTitleFromDb hLogger _ Nothing _ _ = do
    logError hLogger "No title parameter"
    return $ Left "No title parameter"
getNewsFilterByTitleFromDb hLogger pool (Just titleName) page_p sortParam =
    catch
        (do logInfo hLogger "Someone try get news list filtered by title"
            rows <- queryWithPool pool q [titleName]
            return (Right $ NewsArray rows)) $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        logError hLogger err
        return $ Left "Database error"
  where
    sort' =
        if sortParam == ""
            then ""
            else BC.concat [" order by ", sortParam, " DESC"]
    pg =
        if isNothing page_p
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $
                       show $
                       (fromMaybe 1 (readByteStringToInt (fromMaybe "" page_p)) -
                        1) *
                       10
                     ]
    q =
        toQuery $
        BC.concat
            [ "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, "
            , "take_categories_list(category_id), news_text from news join authors using (author_id) join users using (user_id) "
            , "where short_title = ? "
            , sort'
            , pg
            ]

getNewsFilterByAuthorNameFromDb ::
       Handle
    -> Pool Connection
    -> Maybe ByteString
    -> Maybe ByteString
    -> ByteString
    -> IO (Either LBS.ByteString NewsArray)
getNewsFilterByAuthorNameFromDb hLogger _ Nothing _ _ = do
    logError hLogger "No author_name parameter"
    return $ Left "No author_name parameter"
getNewsFilterByAuthorNameFromDb hLogger pool (Just authorName) page_p sortParam =
    catch
        (do logInfo
                hLogger
                "Someone try get news list filtered by author's name"
            rows <- queryWithPool pool q [authorName]
            return (Right $ NewsArray rows)) $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        logError hLogger err
        return $ Left "Database error"
  where
    sort' =
        if sortParam == ""
            then ""
            else BC.concat [" order by ", sortParam, " DESC"]
    pg =
        if isNothing page_p
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $
                       show $
                       (fromMaybe 1 (readByteStringToInt (fromMaybe "" page_p)) -
                        1) *
                       10
                     ]
    q =
        toQuery $
        BC.concat
            [ "select * from (select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, "
            , "take_categories_list(category_id), news_text from news join authors using (author_id) join users using (user_id)) as temp_t "
            , "where author_name = ? "
            , sort'
            , pg
            ]

getNewsFilterByDateFromDb ::
       Handle
    -> Pool Connection
    -> Maybe ByteString
    -> Maybe ByteString
    -> ByteString
    -> IO (Either LBS.ByteString NewsArray)
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
        Just day ->
            catch
                (do rows <- queryWithPool pool q [day]
                    return (Right $ NewsArray rows)) $ \e -> do
                let err = E.decodeUtf8 $ sqlErrorMsg e
                logError hLogger err
                return $ Left "Database error"
  where
    sort' =
        if sortParam == ""
            then ""
            else BC.concat [" order by ", sortParam, " DESC"]
    pg =
        if isNothing page_p
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $
                       show $
                       (fromMaybe 1 (readByteStringToInt (fromMaybe "" page_p)) -
                        1) *
                       10
                     ]
    q =
        toQuery $
        BC.concat
            [ "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, "
            , "take_categories_list(category_id), news_text from news join authors using (author_id) join users using (user_id) "
            , "where date_creation = ? "
            , sort'
            , pg
            ]

getNewsFilterByTagAllFromDb ::
       Handle
    -> Pool Connection
    -> Maybe ByteString
    -> Maybe ByteString
    -> ByteString
    -> IO (Either LBS.ByteString NewsArray)
getNewsFilterByTagAllFromDb hLogger _ Nothing _ _ = do
    logError hLogger "No tag parameter"
    return $ Left "No tag parameter"
getNewsFilterByTagAllFromDb hLogger pool (Just tag_lst) page_p sortParam = do
    logInfo hLogger "Someone try get news list filtered by tag_all parameter"
    case readByteStringListInt tag_lst of
        Nothing -> do
            logError hLogger "Bad tag parameter"
            return (Left "bad tag_all parameter")
        Just tag_list ->
            catch
                (do let cn = BC.pack $ show $ Prelude.length tag_list - 1
                    let q =
                            toQuery $
                            BC.concat
                                [ "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, "
                                , "take_categories_list(category_id), news_text  from news join authors using (author_id) "
                                , "join users using (user_id) join news_tags using (news_id) where tag_id in ? "
                                , "group by news_id,author_name having count(*) > "
                                , cn
                                , " "
                                , sort'
                                , pg
                                ]
                    rows <- queryWithPool pool q (Only (In tag_list))
                    return (Right $ NewsArray rows)) $ \e -> do
                let err = E.decodeUtf8 $ sqlErrorMsg e
                logError hLogger err
                return $ Left "Database error"
  where
    sort' =
        if sortParam == ""
            then ""
            else BC.concat [" order by ", sortParam, " DESC"]
    pg =
        if isNothing page_p
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $
                       show $
                       (fromMaybe 1 (readByteStringToInt (fromMaybe "" page_p)) -
                        1) *
                       10
                     ]

getNewsFilterByContentFromDb ::
       Handle
    -> Pool Connection
    -> Maybe ByteString
    -> Maybe ByteString
    -> ByteString
    -> IO (Either LBS.ByteString NewsArray)
getNewsFilterByContentFromDb hLogger _ Nothing _ _ = do
    logError hLogger "No content parameter"
    return $ Left "No content parameter"
getNewsFilterByContentFromDb hLogger pool (Just content_c) page_p sortParam =
    catch
        (do logInfo hLogger "Someone try get news list filtered by content"
            rows <- queryWithPool pool q [BC.concat ["%", content_c, "%"]]
            return (Right $ NewsArray rows)) $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        logError hLogger err
        return $ Left "Database error"
  where
    sort' =
        if sortParam == ""
            then ""
            else BC.concat [" order by ", sortParam, " DESC"]
    pg =
        if isNothing page_p
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $
                       show $
                       (fromMaybe 1 (readByteStringToInt (fromMaybe "" page_p)) -
                        1) *
                       10
                     ]
    q =
        toQuery $
        BC.concat
            [ "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, "
            , "take_categories_list(category_id), news_text from news join authors using (author_id) join users using (user_id) "
            , "where news_text like ? "
            , sort'
            , pg
            ]

getNewsFilterByAfterDateFromDb ::
       Handle
    -> Pool Connection
    -> Maybe ByteString
    -> Maybe ByteString
    -> ByteString
    -> IO (Either LBS.ByteString NewsArray)
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
        Just day ->
            catch
                (do rows <- queryWithPool pool q [day]
                    return (Right $ NewsArray rows)) $ \e -> do
                let err = E.decodeUtf8 $ sqlErrorMsg e
                logError hLogger err
                return $ Left "Database error"
  where
    sort' =
        if sortParam == ""
            then ""
            else BC.concat [" order by ", sortParam, " DESC"]
    pg =
        if isNothing page_p
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $
                       show $
                       (fromMaybe 1 (readByteStringToInt (fromMaybe "" page_p)) -
                        1) *
                       10
                     ]
    q =
        toQuery $
        BC.concat
            [ "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, "
            , "take_categories_list(category_id), news_text from news join authors using (author_id) join users using (user_id) "
            , "where date_creation > ? "
            , sort'
            , pg
            ]

getNewsFilterByBeforeDateFromDb ::
       Handle
    -> Pool Connection
    -> Maybe ByteString
    -> Maybe ByteString
    -> ByteString
    -> IO (Either LBS.ByteString NewsArray)
getNewsFilterByBeforeDateFromDb hLogger _ Nothing _ _ = do
    logError hLogger "No date parameter"
    return $ Left "No date parameter"
getNewsFilterByBeforeDateFromDb hLogger pool (Just date) page_p sortParam = do
    logInfo
        hLogger
        "Someone try get news list filtered by before_date parameter"
    let date' = readByteStringToDay date
    case date' of
        Nothing -> do
            logError hLogger "Bad date parameter"
            return $ Left "Bad date parameter"
        Just day ->
            catch
                (do rows <- queryWithPool pool q [day]
                    return (Right $ NewsArray rows)) $ \e -> do
                let err = E.decodeUtf8 $ sqlErrorMsg e
                logError hLogger err
                return $ Left "Database error"
  where
    sort' =
        if sortParam == ""
            then ""
            else BC.concat [" order by ", sortParam, " DESC"]
    pg =
        if isNothing page_p
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $
                       show $
                       (fromMaybe 1 (readByteStringToInt (fromMaybe "" page_p)) -
                        1) *
                       10
                     ]
    q =
        toQuery $
        BC.concat
            [ "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, "
            , "take_categories_list(category_id), news_text from news join authors using (author_id) join users using (user_id) "
            , "where date_creation < ? "
            , sort'
            , pg
            ]

getNewsFilterByTagIdFromDb ::
       Handle
    -> Pool Connection
    -> Maybe ByteString
    -> Maybe ByteString
    -> ByteString
    -> IO (Either LBS.ByteString NewsArray)
getNewsFilterByTagIdFromDb hLogger _ Nothing _ _ = do
    logError hLogger "No tag parameter"
    return $ Left "No tag parameter"
getNewsFilterByTagIdFromDb hLogger pool (Just tag_id) page_p' sortParam = do
    logInfo hLogger "Someone try get news list filtered by tag"
    case readByteStringToInt tag_id of
        Nothing -> do
            logError hLogger "Bad tag parameter"
            return $ Left "Bad tag parameter"
        Just n ->
            catch
                (do rows <- queryWithPool pool q [n]
                    return (Right $ NewsArray rows)) $ \e -> do
                let err = E.decodeUtf8 $ sqlErrorMsg e
                logError hLogger err
                return $ Left "Database error"
  where
    sort' =
        if sortParam == ""
            then ""
            else BC.concat [" order by ", sortParam, " DESC"]
    pg =
        if isNothing page_p'
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $
                       show $
                       (fromMaybe 1 (readByteStringToInt (fromMaybe "" page_p')) -
                        1) *
                       10
                     ]
    q =
        toQuery $
        BC.concat
            [ "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, "
            , "take_categories_list(category_id), news_text from news join authors using (author_id) join users using (user_id) "
            , "join news_tags using (news_id) where tag_id = ?"
            , sort'
            , pg
            ]

-------------------------------------------------------------------------------------------
----------------Users database block-------------------------------------------------------
generateToken :: BC.ByteString -> IO (BC.ByteString, UTCTime)
generateToken login = do
    now <- getCurrentTime
    let token' = Prelude.filter (`Prelude.notElem` filt) (show now)
    return (BC.concat [login, BC.pack token'], now)
  where
    filt = " :.-UTC" :: String

generateToken' :: T.Text -> IO (T.Text, UTCTime)
generateToken' login = do
    now <- getCurrentTime
    let token' = Prelude.filter (`Prelude.notElem` filt) (show now)
    return (T.concat [login, T.pack token'], now)
  where
    filt = " :.-UTC" :: String

authentication ::
       Handle
    -> Pool Connection
    -> BC.ByteString
    -> BC.ByteString
    -> IO (Either LBS.ByteString LBS.ByteString)
authentication hLogger pool login password =
    catch
        (do logInfo hLogger $
                T.concat
                    ["User with login ", E.decodeUtf8 login, " try logging"]
            (token', now) <- generateToken login
            n <- executeWithPool pool q (login, password, token', now)
            if n > 0
                then do
                    logInfo hLogger $
                        T.concat
                            ["User with login ", E.decodeUtf8 login, " logged"]
                    return $ Right $ LBS.pack $ B.unpack token'
                else do
                    logError hLogger $
                        T.concat
                            [ "User with login "
                            , E.decodeUtf8 login
                            , " cant logged"
                            ]
                    return $ Left "Wrong login or password") $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        logError hLogger err
        return $ Left "Database error in authentication"
  where
    q =
        toQuery $
        BC.concat
            [ "WITH loging_user as (select user_id from users where login = ? and user_password = (crypt(?,user_password))) "
            , "update tokens set token = ?, creation_date = ? where user_id in (select user_id from loging_user)"
            ]

createUserInDb ::
       Handle
    -> Pool Connection
    -> Maybe T.Text
    -> Maybe T.Text
    -> Maybe T.Text
    -> Maybe T.Text
    -> BC.ByteString
    -> BC.ByteString
    -> LBS.ByteString
    -> IO (Either LBS.ByteString LBS.ByteString)
createUserInDb hLogger _ Nothing _ _ _ _ _ _ = do
    logError hLogger "No login parameter"
    return $ Left "No login parameter"
createUserInDb hLogger _ _ Nothing _ _ _ _ _ = do
    logError hLogger "No password parameter"
    return $ Left "No password parameter"
createUserInDb hLogger _ _ _ Nothing _ _ _ _ = do
    logError hLogger "No first name parameter"
    return $ Left "No first name parameter"
createUserInDb hLogger _ _ _ _ Nothing _ _ _ = do
    logError hLogger "No second name parameter"
    return $ Left "No second name parameter"
createUserInDb hLogger pool (Just login) (Just password) (Just f'_name) (Just l_name) avatar_name avatar_contentType avatar_b = do
    if avatar_name == "" ||
       avatar_contentType == "" ||
       avatar_b == "" || BC.take 5 avatar_contentType /= "image"
        then do
            logError hLogger "Bad image file"
            return $ Left "Bad image file"
        else catch
                 (do now <- getCurrentTime
                     n <-
                         executeWithPool
                             pool
                             q
                             ( avatar_name
                             , Binary avatar_b
                             , avatar_contentType
                             , f'_name
                             , l_name
                             , login
                             , password
                             , now
                             , False)
                     if n > 0
                         then do
                             logInfo hLogger $
                                 T.concat ["User ", login, " registered"]
                             firstToken hLogger pool login password
                         else do
                             logError hLogger "Registration failed"
                             return $ Left "Registration failed") $ \e -> do
                 let err = E.decodeUtf8 $ sqlErrorMsg e
                 logError hLogger err
                 return $ Left "Database error in registration"
  where
    q =
        toQuery $
        BC.concat
            [ "with avatar_id as (insert into images (image_name,image_b,content_type) values (?,?,?) returning image_id) "
            , "insert into users (first_name, last_name, avatar, login, user_password, creation_date, admin_mark) "
            , "values (?,?,(select image_id from avatar_id),?,crypt(?,gen_salt('md5')),?,?)"
            ]

deleteUserFromDb ::
       Handle -> ByteString -> IO (Either LBS.ByteString LBS.ByteString)
deleteUserFromDb hLogger login =
    catch
        (do logInfo hLogger $
                T.concat ["Trying delete user ", E.decodeUtf8 login]
            conn <-
                connectPostgreSQL
                    "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
            let q = "delete from users where login = ?"
            n <- execute conn q [login]
            close conn
            if n > 0
                then do
                    logInfo hLogger $
                        T.concat ["User ", E.decodeUtf8 login, " deleted"]
                    return $
                        Right $
                        LBS.concat ["User ", LBS.fromStrict login, " deleted"]
                else return $ Left "User not exist") $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        logError hLogger err
        return $ Left "Database error"

deleteUserFromDb' ::
       Handle
    -> Pool Connection
    -> ByteString
    -> IO (Either LBS.ByteString LBS.ByteString)
deleteUserFromDb' hLogger pool login =
    catch
        (do logInfo hLogger $
                T.concat ["Trying delete user ", E.decodeUtf8 login]
            n <- executeWithPool pool "dlete from users where login = ?" [login]
            if n > 0
                then do
                    logInfo hLogger $
                        T.concat ["User ", E.decodeUtf8 login, " deleted"]
                    return $
                        Right $
                        LBS.concat ["User ", LBS.fromStrict login, " deleted"]
                else return $ Left "User not exist") $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        logError hLogger err
        return $ Left "Database error"

firstToken ::
       Handle
    -> Pool Connection
    -> T.Text
    -> T.Text
    -> IO (Either LBS.ByteString LBS.ByteString)
firstToken hLogger pool login password =
    catch
        (do logInfo hLogger $ T.concat ["Generate first token for user ", login]
            (token', now) <- generateToken' login
            n <- executeWithPool pool q (login, password, token', now)
            if n > 0
                then do
                    logInfo hLogger $
                        T.concat ["User with login ", login, " logged"]
                    return $ Right $ LBS.fromStrict $ E.encodeUtf8 token'
                else do
                    logError hLogger $
                        T.concat ["User with login ", login, " cant logged"]
                    return $ Left "Wrong login or password") $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        logError hLogger err
        return $ Left "Database error"
  where
    q =
        toQuery $
        BC.concat
            [ "WITH loging_user as (select user_id from users where login = ? and user_password = (crypt(?,user_password))) "
            , "insert into tokens (user_id,token,creation_date) values ((select user_id from loging_user),?,?)"
            ]

profileOnDb ::
       Handle
    -> Pool Connection
    -> TokenLifeTime
    -> Maybe T.Text
    -> IO (Either LBS.ByteString Profile)
profileOnDb hLogger _ _ Nothing = do
    logError hLogger "No token parameter"
    return $ Left "No token parameter"
profileOnDb hLogger pool token_lifetime (Just token') =
    catch
        (do logInfo hLogger "Sending profile information"
            rows <- queryWithPool pool q (TokenProfile token' token_lifetime)
            if Prelude.null rows
                then do
                    logError hLogger "Bad token"
                    return $ Left "Bad token"
                else do
                    logInfo hLogger "Profile information sended"
                    return $ Right $ Prelude.head rows) $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ T.concat [err, " ", T.pack $ show errStateInt]
        case errStateInt of
            23505 -> return $ Left "Category already exist"
            _ -> return $ Left "Database error"
  where
    q =
        "select first_name, last_name, avatar from users join tokens using (user_id) where token = ? and (now()- tokens.creation_date) < make_interval(secs => ?)"

---------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------Categories block---------------------------------------------------------------------------------
getCategoriesListFromDb ::
       Handle
    -> Pool Connection
    -> Maybe BC.ByteString
    -> IO (Either LBS.ByteString ListOfCategories)
getCategoriesListFromDb hLogger pool pageParam =
    catch
        (do logInfo hLogger "Someone try get list of categories"
            rows <- query_WithPool pool q
            return $ Right (ListOfCategories rows)) $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        logError hLogger err
        return $ Left "Database error"
  where
    pg =
        if isNothing pageParam
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $
                       show $
                       (fromMaybe
                            1
                            (readByteStringToInt (fromMaybe "" pageParam)) -
                        1) *
                       10
                     ]
    q = toQuery $ BC.concat ["select category_name from categories", pg]

createCategoryOnDb ::
       Handle
    -> Pool Connection
    -> Maybe T.Text
    -> Maybe T.Text
    -> IO (Either LBS.ByteString LBS.ByteString)
createCategoryOnDb hLogger _ Nothing _ = do
    logError hLogger "No category_name field"
    return $ Left "No category_name field"
createCategoryOnDb hLogger _ _ Nothing = do
    logError hLogger "No maternal_category_name field"
    return $ Left "No maternal_category_name field"
createCategoryOnDb hLogger pool (Just category'_name) (Just maternal_name) =
    catch
        (do logInfo hLogger "Creating new category"
            if maternal_name == ""
                then do
                    logInfo hLogger "Maternal category is null"
                    rows <-
                        returningWithPool
                            pool
                            q
                            [(category'_name, Nothing :: Maybe T.Text)] :: IO [Only Int]
                    logInfo hLogger "Category created"
                    return $
                        Right $
                        LBS.fromStrict $
                        BC.pack $ show (fromOnly $ Prelude.head rows)
                else do
                    c'_id <-
                        queryWithPool pool check_maternal [maternal_name] :: IO [Only Int]
                    if Prelude.null c'_id
                        then do
                            logError hLogger "Maternal category not exist"
                            return $ Left "Maternal category not exist"
                        else do
                            rows <-
                                returningWithPool
                                    pool
                                    q
                                    [ ( category'_name
                                      , fromOnly $ Prelude.head c'_id)
                                    ] :: IO [Only Int]
                            logInfo hLogger "Category created"
                            return $
                                Right $
                                LBS.fromStrict $
                                BC.pack $ show (fromOnly $ Prelude.head rows)) $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger err
        case errStateInt of
            23505 -> return $ Left "Category already exist"
            _ -> return $ Left "Database error"
  where
    q =
        "insert into categories (category_name, maternal_category) values (?,?) returning category_id"
    check_maternal =
        "select category_id from categories where category_name = ?"

deleteCategoryFromDb ::
       Handle
    -> Pool Connection
    -> Maybe T.Text
    -> IO (Either LBS.ByteString LBS.ByteString)
deleteCategoryFromDb hLogger _ Nothing = do
    logError hLogger "No category_name parametr"
    return $ Left "No category_name parametr"
deleteCategoryFromDb hLogger pool (Just categoryName) =
    catch
        (do n <-
                executeWithPool
                    pool
                    "delete from categories where category_name = ?"
                    [categoryName]
            if n > 0
                then do
                    logInfo hLogger $
                        T.concat ["Category ", categoryName, " deleted"]
                    return $ Right "Category deleted"
                else do
                    logError hLogger $
                        T.concat ["Category ", categoryName, " not exist"]
                    return $ Right "Category not exist") $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        logError hLogger err
        return $ Left "Database error"

editCategoryOnDb ::
       Handle
    -> Pool Connection
    -> Maybe T.Text
    -> Maybe T.Text
    -> Maybe T.Text
    -> IO (Either LBS.ByteString LBS.ByteString)
editCategoryOnDb hLogger _ Nothing _ _ = do
    logError hLogger "No old_name parametr"
    return $ Left "No old_name parametr"
editCategoryOnDb hLogger pool (Just old_name) (Just new'_name) (Just "") =
    catch
        (do logInfo hLogger $
                T.concat
                    ["Update category_name parameter on category ", old_name]
            n <-
                executeWithPool
                    pool
                    "update categories set category_name = ? where category_name = ?"
                    (new'_name, old_name)
            if n > 0
                then do
                    logInfo hLogger $
                        T.concat ["Category ", old_name, " edited"]
                    return $ Right "Category edited"
                else do
                    logError hLogger $
                        T.concat ["Category ", old_name, " not exist"]
                    return $ Left "Category not exist") $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ T.concat [err, T.pack $ show errStateInt]
        case errStateInt of
            23505 -> return $ Left "Category already exist"
            _ -> return $ Left "Database error"
editCategoryOnDb hLogger pool (Just old_name) (Just "") (Just new'_maternal) =
    catch
        (do logInfo hLogger $
                T.concat
                    [ "Update maternal_category parameter on category "
                    , old_name
                    ]
            m_id <-
                queryWithPool
                    pool
                    "select category_id from categories where category_name = ?"
                    [new'_maternal] :: IO [Only Int]
            if Prelude.null m_id
                then do
                    logError hLogger $
                        T.concat
                            ["Maternal category ", new'_maternal, " not exist"]
                    return $ Left "Maternal category not exist"
                else do
                    n <-
                        executeWithPool
                            pool
                            "update categories set maternal_category = ? where category_name = ?"
                            (fromOnly $ Prelude.head m_id, old_name)
                    if n > 0
                        then do
                            logInfo hLogger $
                                T.concat ["Category ", old_name, " edited"]
                            return $ Right "Category edited"
                        else do
                            logError hLogger $
                                T.concat ["Category ", old_name, " not exist"]
                            return $ Left "Category not exist") $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        logError hLogger err
        return $ Left "Database error"
editCategoryOnDb hLogger pool (Just old_name) (Just new'_name) (Just new'_maternal) =
    catch
        (do logInfo hLogger $
                T.concat ["Update all parameters on category ", old_name]
            m_id <-
                queryWithPool
                    pool
                    "select category_id from categories where category_name = ?"
                    [new'_maternal] :: IO [Only Int]
            if Prelude.null m_id
                then do
                    logError hLogger $
                        T.concat
                            ["Maternal category ", new'_maternal, " not exist"]
                    return $ Left "Maternal category not exist"
                else do
                    let q =
                            "update categories set category_name = ?, maternal_category = ? where category_name = ?"
                    n <-
                        executeWithPool
                            pool
                            q
                            (new'_name, fromOnly $ Prelude.head m_id, old_name)
                    if n > 0
                        then do
                            logInfo hLogger $
                                T.concat ["Category ", old_name, " edited"]
                            return $ Right "Category edited"
                        else do
                            logError hLogger $
                                T.concat ["Category ", old_name, " not exist"]
                            return $ Left "Category not exist") $ \e -> do
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

--------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------Drafts Block-----------------------------------------------------------------------------------
checkAuthor :: Handle -> Maybe T.Text -> IO (Either LBS.ByteString Int)
checkAuthor hLogger Nothing = do
    logError hLogger "No token parameter"
    return $ Left "No token parameter"
checkAuthor hLogger (Just token') =
    catch
        (do conn <-
                connectPostgreSQL
                    "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
            rows <-
                query
                    conn
                    "select author_id from authors join users using (user_id) join tokens using (user_id) where token = ?"
                    [token'] :: IO [Only Int]
            if Prelude.null rows
                then do
                    logError
                        hLogger
                        "User try get drafts without author's rights"
                    return $ Left "You are not author"
                else return $ Right (fromOnly $ Prelude.head rows)) $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ T.concat [err, " ", T.pack $ show errStateInt]
        return $ Left "Database error"

checkAuthor' ::
       Handle
    -> Pool Connection
    -> TokenLifeTime
    -> Maybe T.Text
    -> IO (Either LBS.ByteString Int)
checkAuthor' hLogger _ _ Nothing = do
    logError hLogger "No token parameter"
    return $ Left "No token parameter"
checkAuthor' hLogger pool token_lifetime (Just token') =
    catch
        (do rows <- queryWithPool pool q [token']
            if Prelude.null rows
                then do
                    return $ Left "You are not author"
                else return $ Right (fromOnly $ Prelude.head rows)) $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ T.concat [err, " ", T.pack $ show errStateInt]
        return $ Left "Database error"
  where
    q =
        toQuery $
        BC.concat
            [ "select author_id from authors join users using (user_id) join tokens using (user_id) where token = ? "
            , "and (now()- tokens.creation_date) < make_interval(secs => "
            , BC.pack $ show token_lifetime
            , ")"
            ]

getDraftsByAuthorToken ::
       Handle
    -> Pool Connection
    -> TokenLifeTime
    -> Maybe T.Text
    -> IO (Either LBS.ByteString DraftArray)
getDraftsByAuthorToken hLogger _ _ Nothing = do
    logError hLogger "No token parameter"
    return $ Left "No token parameter"
getDraftsByAuthorToken hLogger pool token_lifetime (Just token') =
    catch
        (do logInfo hLogger "Someone try get drafts list"
            rows <- queryWithPool pool q (TokenProfile token' token_lifetime)
            if Prelude.null rows
                then do
                    logError hLogger "User use bad token or haven't drafts"
                    return $ Left "You are not author or don't have drafts "
                else return $ Right $ DraftArray rows) $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ T.concat [err, " ", T.pack $ show errStateInt]
        return $ Left "Database error"
  where
    q =
        toQuery $
        BC.concat
            [ "select short_title, date_of_changes, category_id, draft_text, main_image, array_agg(image_id) "
            , "from drafts join drafts_images using (draft_id) join authors using (author_id) join tokens using (user_id) "
            , "where token = ? and (now() - tokens.creation_date) < make_interval(secs => ?) group by draft_id"
            ]

deleteDraftFromDb ::
       Handle
    -> Int
    -> Maybe ByteString
    -> IO (Either LBS.ByteString LBS.ByteString)
deleteDraftFromDb hLogger _ Nothing = do
    logError hLogger "No draft_id parameter"
    return $ Left "No draft_id parameter"
deleteDraftFromDb hLogger author'_id (Just draft_id) =
    catch
        (do let draft_id_int = readByteStringToInt draft_id
            case draft_id_int of
                Nothing -> do
                    logError hLogger "Bad draft_id parameter"
                    return $ Left "Bad draft_id parameter"
                Just n -> do
                    conn <-
                        connectPostgreSQL
                            "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
                    let q =
                            "delete from drafts where draft_id = ? and author_id = ?"
                    ne <- execute conn q (n, author'_id)
                    close conn
                    if ne > 0
                        then do
                            logInfo hLogger $
                                T.concat ["Draft ", T.pack $ show n, " deleted"]
                            return $ Right "Draft deleted"
                        else do
                            logError hLogger "Deleting not existing draft"
                            return $ Left "Draft not exist") $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ T.concat [err, " ", T.pack $ show errStateInt]
        return $ Left "Database error"

getDraftByIdFromDb ::
       Handle
    -> Pool Connection
    -> TokenLifeTime
    -> Maybe T.Text
    -> Int
    -> IO (Either LBS.ByteString Draft)
getDraftByIdFromDb hLogger _ _ Nothing _ = do
    logError hLogger "No token parameter"
    return $ Left "No Token parameter"
getDraftByIdFromDb hLogger pool token_lifetime token' draft_id = do
    ch_author <- checkAuthor' hLogger pool token_lifetime token'
    case ch_author of
        Left bs -> return $ Left bs
        Right author_id ->
            catch
                (do rows <- queryWithPool pool q [author_id]
                    if Prelude.null rows
                        then do
                            logError hLogger "Wrong draft id or draft not exist"
                            return $ Left "Wrong draft id or draft not exist"
                        else do
                            logInfo hLogger "Sending draft to user"
                            return $ Right $ Prelude.head rows) $ \e -> do
                let err = E.decodeUtf8 $ sqlErrorMsg e
                let errState = sqlState e
                let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                logError hLogger $
                    T.concat [err, " ", T.pack $ show errStateInt]
                return $ Left "Database error"
  where
    q =
        toQuery $
        BC.concat
            [ "with image_arr as (select array_agg(image_id) from drafts_images where draft_id = "
            , BC.pack $ show draft_id
            , "), "
            , " tags_arr as (select array_agg(tag_name) from draft_tags join tags using (tag_id) where draft_id = "
            , BC.pack $ show draft_id
            , ") "
            , "select short_title, date_of_changes, category_id, draft_text, main_image, "
            , "(select * from image_arr) as other_images, (select * from  tags_arr) as tags from drafts where author_id = ? and draft_id = "
            , BC.pack $ show draft_id
            ]

getTagsListFromDb ::
       Handle
    -> Pool Connection
    -> Maybe ByteString
    -> IO (Either LBS.ByteString TagsList)
getTagsListFromDb hLogger pool maybe_page =
    catch
        (do logInfo hLogger "Sending tags list"
            rows <- query_WithPool pool q
            return $ Right $ TagsList rows) $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ T.concat [err, " ", T.pack $ show errStateInt]
        return $ Left "Database error"
  where
    pg =
        if isNothing maybe_page
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $
                       show $
                       (fromMaybe
                            1
                            (readByteStringToInt (fromMaybe "" maybe_page)) -
                        1) *
                       10
                     ]
    q = toQuery $ BC.concat ["select tag_name from tags order by tag_name", pg]

createTagInDb ::
       Handle
    -> Pool Connection
    -> Maybe T.Text
    -> IO (Either LBS.ByteString Int)
createTagInDb hLogger _ Nothing = do
    logError hLogger "No tag_name parameter"
    return $ Left "No tag_name parameter"
createTagInDb hLogger pool (Just tag_name') =
    catch
        (do logInfo hLogger "Creating new tag"
            rows <- queryWithPool pool q [tag_name'] :: IO [Only Int]
            if Prelude.null rows
                then do
                    logError hLogger "Tag not created"
                    return $ Left "Tag not created"
                else do
                    logInfo hLogger "Tag created"
                    return $ Right $ fromOnly $ Prelude.head rows) $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ T.concat [err, T.pack $ show errStateInt]
        case errStateInt of
            23505 -> return $ Left "Tag already exist"
            _ -> return $ Left "Database error"
  where
    q =
        toQuery $
        BC.concat ["insert into tags (tag_name) values (?) returning tag_id"]

deleteTagFromDb ::
       Handle
    -> Pool Connection
    -> Maybe T.Text
    -> IO (Either LBS.ByteString LBS.ByteString)
deleteTagFromDb hLogger _ Nothing = do
    logError hLogger "No tag_name parameter"
    return $ Left "No tag_name parameter"
deleteTagFromDb hLogger pool (Just tag_name') =
    catch
        (do logInfo hLogger $ T.concat ["Deleting tag ", tag_name']
            n <- executeWithPool pool q [tag_name']
            if n > 0
                then do
                    logInfo hLogger $ T.concat ["Tag ", tag_name', " deleted"]
                    return $ Right "Tag deleted"
                else do
                    logError hLogger $
                        T.concat ["Tag ", tag_name', " not deleted"]
                    return $ Right "Tag not deleted") $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ T.concat [err, " ", T.pack $ show errStateInt]
        return $ Left "Database error"
  where
    q = toQuery $ BC.concat ["delete from tags where tag_name = ?"]

{-catchSqlError :: Handle -> SqlError -> IO (Either LBS.ByteString a)
catchSqlError hLogger e = do
    let err = E.decodeUtf8 $ sqlErrorMsg e
    let errStateInt = fromMaybe 0 (readByteStringToInt $ sqlState e)
    logError hLogger $ T.concat [err, " ",T.pack $ show errStateInt]
    return $ Left "Database error"

dberrors :: Handle -> [Handler (Either LBS.ByteString a)]
dberrors hLogger = [Handler (\ (e :: SqlError) -> catchSqlError hLogger e)]

tstNewErr :: IO (Either LBS.ByteString Draft')
tstNewErr = getDraftByIdFromDb' (Handle Debug printLog) 12 155
-}
getTagsIds ::
       Handle
    -> Pool Connection
    -> BC.ByteString
    -> IO (Either LBS.ByteString [Int])
getTagsIds hLogger pool tags_bs = do
    let n = Prelude.length $ BC.split ' ' tags_bs
    let l = BC.split ' ' tags_bs
    let q = toQuery $ BC.concat ["select tag_id from tags where tag_name in ?"]
    rows <- queryWithPool pool q (Only (In l))
    if Prelude.length rows < n
        then do
            logError hLogger "Someone tags not exist"
            return $ Left "Someone tags not exist"
        else do
            return $ Right $ fromOnly <$> rows

createDraftOnDb' ::
       Handle
    -> Pool Connection
    -> TokenLifeTime
    -> Maybe BC.ByteString
    -> Maybe T.Text
    -> Maybe BC.ByteString
    -> Maybe T.Text
    -> Maybe T.Text
    -> Maybe Image
    -> Maybe [Image]
    -> IO (Either LBS.ByteString Int)
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
    draft_id <- newDraft
    c <- createTagConnections draft_id
    l <- loadMainImage c main'_image
    loadImages l images_list
  where
    newDraft =
        catch
            (do let q =
                        toQuery $
                        BC.concat
                            [ "with get_a as (select author_id from authors join tokens using (user_id) where token = '"
                            , token'
                            , "' and (now() - tokens.creation_date) < make_interval(secs => "
                            , BC.pack $ show token_lifetime
                            , ")), get_c as (select category_id from categories where category_name = ?) "
                            , "insert into drafts (author_id,short_title,date_of_changes,category_id,draft_text) values ((select author_id from get_a),"
                            , " ? ,now(),(select category_id from get_c),"
                            , " ?) returning draft_id"
                            ]
                rows <-
                    queryWithPool pool q (category, short'_title, text) :: IO [Only Int]
                if Prelude.null rows
                    then do
                        logError hLogger "Draft not created"
                        return $ Left "Draft not created"
                    else do
                        return $ Right $ fromOnly $ Prelude.head rows) $ \e -> do
            let err = E.decodeUtf8 $ sqlErrorMsg e
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ T.concat [err, " ", T.pack $ show errStateInt]
            return $ Left "Database error"
    createTagConnections (Left message) = return $ Left message
    createTagConnections (Right draft_id) =
        catch
            (do tag_ids <- getTagsIds hLogger pool tags_list
                case tag_ids of
                    Left bs -> return $ Left bs
                    Right ns -> do
                        let q =
                                toQuery $
                                BC.concat
                                    [ "insert into draft_tags (draft_id,tag_id) values (?,?)"
                                    ]
                        let a = Prelude.map (draft_id, ) ns
                        let nt = Prelude.length ns
                        n <- executeManyWithPool pool q a
                        if fromIntegral n < nt
                            then do
                                logError hLogger "Some tags not added"
                                return $ Left "Some tags not added"
                            else return $ Right draft_id) $ \e -> do
            let err = E.decodeUtf8 $ sqlErrorMsg e
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ T.concat [err, " ", T.pack $ show errStateInt]
            return $ Left "Database error"
    loadMainImage (Left message) _ = return $ Left message
    loadMainImage (Right draft_id) Nothing = return $ Right draft_id
    loadMainImage (Right draft_id) (Just image) =
        catch
            (do let q =
                        toQuery $
                        BC.concat
                            [ "with m_id as (insert into images (image_name,content_type, image_b) values (?,?,?) returning image_id) "
                            , "update drafts set main_image = (select * from m_id) where draft_id = "
                            , BC.pack $ show draft_id
                            ]
                n <- executeWithPool pool q image
                if n < 1
                    then do
                        logError hLogger "Image not loaded"
                        return $ Left "Image not loaded"
                    else return $ Right draft_id) $ \e -> do
            let err = E.decodeUtf8 $ sqlErrorMsg e
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ T.concat [err, " ", T.pack $ show errStateInt]
            return $ Left "Database error"
    loadImages (Left message) _ = return $ Left message
    loadImages (Right draft_id) Nothing = return $ Right draft_id
    loadImages (Right draft_id) (Just images) =
        catch
            (do let q =
                        toQuery $
                        BC.concat
                            [ "with m_id as (insert into images (image_name,content_type, image_b) values (?,?,?) returning image_id), "
                            , "d_i as (select "
                            , BC.pack $ show draft_id
                            , " as draft_id, image_id from m_id) "
                            , "insert into drafts_images (draft_id,image_id) select * from d_i"
                            ]
                n <- executeManyWithPool pool q images
                if fromIntegral n < Prelude.length images
                    then do
                        logError hLogger "Images not loaded"
                        return $ Left "Images not loaded"
                    else do
                        logInfo hLogger "Draft created"
                        return $ Right draft_id) $ \e -> do
            let err = E.decodeUtf8 $ sqlErrorMsg e
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ T.concat [err, " ", T.pack $ show errStateInt]
            return $ Left "Database error"

updateDraftInDb' ::
       Handle
    -> Pool Connection
    -> TokenLifeTime
    -> Maybe BC.ByteString
    -> Maybe T.Text
    -> Maybe BC.ByteString
    -> Maybe T.Text
    -> Maybe T.Text
    -> Maybe Image
    -> Maybe [Image]
    -> Int
    -> IO (Either LBS.ByteString LBS.ByteString)
updateDraftInDb' hLogger _ _ Nothing _ _ _ _ _ _ _ = do
    logError hLogger "No token param"
    return $ Left "No token param"
updateDraftInDb' hLogger _ _ _ Nothing _ _ _ _ _ _ = do
    logError hLogger "No category field"
    return $ Left "No category field"
updateDraftInDb' hLogger _ _ _ _ Nothing _ _ _ _ _ = do
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
    u <- updateDraft
    dt <- deleteTagConnections u
    ct <- createTagConnections dt
    dmi <- deleteMainImage ct main'_image
    l <- loadMainImage dmi main'_image
    di <- deleteOldImages l images_list
    loadImages di images_list
  where
    updateDraft =
        catch
            (do logDebug hLogger "Update drafts"
                let q =
                        toQuery $
                        BC.concat
                            [ "with get_a as (select author_id from authors join tokens using (user_id) where token = '"
                            , token'
                            , "' and (now() - tokens.creation_date) < make_interval(secs => "
                            , BC.pack $ show token_lifetime
                            , ")), get_c as (select category_id from categories where category_name = ?) "
                            , "update drafts set short_title = ?, date_of_changes = now(), category_id = (select * from get_c), "
                            , "draft_text = ? where draft_id = "
                            , BC.pack $ show draft_id
                            ]
                n <- executeWithPool pool q (category, short'_title, text)
                if n < 1
                    then do
                        logError hLogger "Draft not updated"
                        return $ Left "Draft not updated"
                    else do
                        logDebug hLogger "Update drafts complete"
                        return $ Right "Draft updated") $ \e -> do
            let err = E.decodeUtf8 $ sqlErrorMsg e
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ T.concat [err, " ", T.pack $ show errStateInt]
            return $ Left "Database error"
    deleteTagConnections (Left mess) = return $ Left mess
    deleteTagConnections (Right mess) =
        catch
            (do logDebug hLogger "Deleting old tags"
                let q =
                        toQuery $
                        BC.concat
                            [ "delete from draft_tags where draft_id = "
                            , BC.pack $ show draft_id
                            ]
                n <- execute_WithPool pool q
                if n < 1
                    then do
                        logError hLogger "Draft tags not updated"
                        return $ Left "Draft tags not updated"
                    else do
                        logDebug hLogger "Deleting old tags complete"
                        return $ Right mess) $ \e -> do
            let err = E.decodeUtf8 $ sqlErrorMsg e
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ T.concat [err, " ", T.pack $ show errStateInt]
            return $ Left "Database error"
    createTagConnections (Left message) = return $ Left message
    createTagConnections (Right mess) =
        catch
            (do tag_ids <- getTagsIds hLogger pool tags_list
                case tag_ids of
                    Left bs -> return $ Left bs
                    Right ns -> do
                        logDebug hLogger "Add new tags"
                        let q =
                                toQuery $
                                BC.concat
                                    [ "insert into draft_tags (draft_id,tag_id) values (?,?)"
                                    ]
                        let a = Prelude.map (draft_id, ) ns
                        let nt = Prelude.length ns
                        n <- executeManyWithPool pool q a
                        if fromIntegral n < nt
                            then do
                                logError hLogger "Some tags not added"
                                return $ Left "Some tags not added"
                            else do
                                logDebug hLogger "Add new tags complete"
                                return $ Right mess) $ \e -> do
            let err = E.decodeUtf8 $ sqlErrorMsg e
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ T.concat [err, " ", T.pack $ show errStateInt]
            return $ Left "Database error"
    deleteMainImage (Left mess) _ = return $ Left mess
    deleteMainImage (Right mess) Nothing = return $ Right mess
    deleteMainImage (Right mess) (Just _) =
        catch
            (do logDebug hLogger "Deleting old main image"
                let q =
                        toQuery $
                        BC.concat
                            [ "with m_id as (select main_image from drafts where draft_id = "
                            , BC.pack $ show draft_id
                            , ") "
                            , "delete from images where image_id = (select * from m_id)"
                            ]
                _ <- execute_WithPool pool q
                logDebug hLogger "Deleting old main image complete"
                return $ Right mess) $ \e -> do
            let err = E.decodeUtf8 $ sqlErrorMsg e
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ T.concat [err, " ", T.pack $ show errStateInt]
            return $ Left "Database error"
    loadMainImage (Left mess) _ = return $ Left mess
    loadMainImage (Right mess) Nothing = return $ Right mess
    loadMainImage (Right mess) (Just image) =
        catch
            (do logDebug hLogger "Add new main image"
                let q =
                        toQuery $
                        BC.concat
                            [ "with m_id as (insert into images (image_name,content_type, image_b) values (?,?,?) returning image_id) "
                            , "update drafts set main_image = (select * from m_id) where draft_id = "
                            , BC.pack $ show draft_id
                            ]
                n <- executeWithPool pool q image
                if n < 1
                    then do
                        logError hLogger "Image not loaded"
                        return $ Left "Image not loaded"
                    else do
                        logDebug hLogger "Add new main image"
                        return $ Right mess) $ \e -> do
            let err = E.decodeUtf8 $ sqlErrorMsg e
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ T.concat [err, " ", T.pack $ show errStateInt]
            return $ Left "Database error"
    deleteOldImages (Left mess) _ = return $ Left mess
    deleteOldImages (Right mess) Nothing = return $ Right mess
    deleteOldImages (Right mess) (Just _) =
        catch
            (do logDebug hLogger "Deleting old images"
                let q =
                        toQuery $
                        BC.concat
                            [ "delete from images where image_id in (select image_id from drafts_images where draft_id = "
                            , BC.pack $ show draft_id
                            , ")"
                            ]
                _ <- execute_WithPool pool q
                logDebug hLogger "Deleting old images complete"
                return $ Right mess) $ \e -> do
            let err = E.decodeUtf8 $ sqlErrorMsg e
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ T.concat [err, " ", T.pack $ show errStateInt]
            return $ Left "Database error"
    loadImages (Left mess) _ = return $ Left mess
    loadImages (Right mess) Nothing = return $ Right mess
    loadImages (Right mess) (Just images) =
        catch
            (do logDebug hLogger "Add new images"
                let q =
                        toQuery $
                        BC.concat
                            [ "with m_id as (insert into images (image_name,content_type, image_b) values (?,?,?) returning image_id), "
                            , "d_i as (select "
                            , BC.pack $ show draft_id
                            , " as draft_id, image_id from m_id) "
                            , "insert into drafts_images (draft_id,image_id) select * from d_i"
                            ]
                n <- executeManyWithPool pool q images
                if fromIntegral n < Prelude.length images
                    then do
                        logError hLogger "Images not loaded"
                        return $ Left "Images not loaded"
                    else do
                        logDebug hLogger "Add new images complete"
                        return $ Right mess) $ \e -> do
            let err = E.decodeUtf8 $ sqlErrorMsg e
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ T.concat [err, " ", T.pack $ show errStateInt]
            return $ Left "Database error"

publicNewsOnDb' ::
       Handle -> Pool Connection -> Int -> Int -> IO (Either LBS.ByteString Int)
publicNewsOnDb' hLogger pool author'_id draft_id = do
    n <- insertNews
    t <- insertTags n
    insertImages t
  where
    insertNews =
        catch
            (do logDebug hLogger "Adding news to db"
                let q =
                        toQuery $
                        BC.concat
                            [ "with dus as (select short_title,author_id, category_id,draft_text,main_image from drafts where author_id = ?"
                            , " and draft_id = ?) "
                            , "insert into news (short_title, date_creation, author_id, category_id, news_text, main_image) values "
                            , "((select short_title from dus), now(), "
                            , "(select author_id from dus), "
                            , "(select category_id from dus), "
                            , "(select draft_text from dus), "
                            , "(select main_image from dus)) returning news_id"
                            ]
                rows <- queryWithPool pool q (author'_id, draft_id)
                if Prelude.null rows
                    then do
                        logError hLogger "News not published"
                        return $ Left "News not published"
                    else do
                        logDebug hLogger "Adding news to db complete"
                        return $ Right $ fromOnly $ Prelude.head rows) $ \e -> do
            let err = E.decodeUtf8 $ sqlErrorMsg e
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ T.concat [err, " ", T.pack $ show errStateInt]
            return $ Left "Database error"
    insertTags (Left mess) = return $ Left mess
    insertTags (Right news_id) =
        catch
            (do logDebug hLogger "Adding news tags to db"
                let q =
                        toQuery $
                        BC.concat
                            [ "with tags_ids as (select tag_id from draft_tags where draft_id = "
                            , BC.pack $ show draft_id
                            , " ), "
                            , "tag_n as (select "
                            , BC.pack $ show news_id
                            , " as news_id, tag_id from tags_ids) "
                            , "insert into news_tags (news_id, tag_id) select * from tag_n"
                            ]
                _ <- execute_WithPool pool q
                logDebug hLogger "Adding news tags to db complete"
                return $ Right news_id) $ \e -> do
            let err = E.decodeUtf8 $ sqlErrorMsg e
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ T.concat [err, " ", T.pack $ show errStateInt]
            return $ Left "Database error"
    insertImages (Left mess) = return $ Left mess
    insertImages (Right news_id) =
        catch
            (do logDebug hLogger "Adding news images to db"
                let q =
                        toQuery $
                        BC.concat
                            [ "with images_ids as (select image_id from drafts_images where draft_id = "
                            , BC.pack $ show draft_id
                            , "), "
                            , "image_n as (select "
                            , BC.pack $ show news_id
                            , " as news_id, image_id from images_ids) "
                            , "insert into news_images (news_id, image_id) select * from image_n"
                            ]
                _ <- execute_WithPool pool q
                logDebug hLogger "Adding news images to db complete"
                return $ Right news_id) $ \e -> do
            let err = E.decodeUtf8 $ sqlErrorMsg e
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ T.concat [err, " ", T.pack $ show errStateInt]
            return $ Left "Database error"

getPhoto ::
       Handle -> Pool Connection -> Int -> IO (Either LBS.ByteString ImageB)
getPhoto hLogger pool image_id =
    catch
        (do logDebug hLogger "Getting images from db"
            let q =
                    toQuery $
                    BC.concat
                        [ "select image_b, content_type from images where image_id = ?"
                        ]
            rows <- queryWithPool pool q [image_id] :: IO [ImageB]
            if Prelude.null rows
                then return $ Left "Image not exist"
                else return $ Right $ Prelude.head rows) $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ T.concat [err, " ", T.pack $ show errStateInt]
        return $ Left "Database error"

getPhotoList ::
       Handle
    -> Pool Connection
    -> Maybe ByteString
    -> IO (Either LBS.ByteString ImageArray)
getPhotoList hLogger pool pageParam =
    catch
        (do logInfo hLogger "Someone try get photo list"
            rows <- query_WithPool pool q :: IO [ElemImageArray]
            return $ Right (ImageArray rows)) $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ T.concat [err, " ", T.pack $ show errStateInt]
        return $ Left "Database error"
  where
    pg =
        if isNothing pageParam
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $
                       show $
                       (fromMaybe
                            1
                            (readByteStringToInt (fromMaybe "" pageParam)) -
                        1) *
                       10
                     ]
    q =
        toQuery $
        BC.concat
            ["select image_id, image_name from images order by image_id", pg]
