{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.NewsAndComments where

import Control.Exception (catch)

--import Data.ByteString as B (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe, isNothing)
import Data.Pool (Pool)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple
    ( Connection
    , In(In)
    , Only(Only)
    , SqlError(sqlErrorMsg, sqlState)
    )
import Databaseoperations.CheckAdmin (checkAdmin)
import HelpFunction
    ( readByteStringListInt
    , readByteStringToDay
    , readByteStringToInt
    , toQuery
    )
import Logger (Handle, logError, logInfo)
import PostgreSqlWithPool (executeWithPool, queryWithPool, query_WithPool)
import Types
    ( Comment(Comment)
    , CommentArray(CommentArray)
    , GetNews
    , NewsArray(NewsArray)
    , TokenLifeTime
    )

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
                    return $ Right "Comment not added") $ \e
        --let err = E.decodeUtf8 $ sqlErrorMsg e
     -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
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
    isAdmin <- checkAdmin hLogger pool token_lifetime token'
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
                            return $ Left "Comment not deleted") $ \e
                --let err = E.decodeUtf8 $ sqlErrorMsg e
                --logError hLogger err
             -> do
                let errState = sqlState e
                let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                logError hLogger $
                    T.concat ["Database error ", T.pack $ show errStateInt]
                return $ Left "Database error"

getCommentsByNewsIdFromDb ::
       Handle
    -> Pool Connection
    -> Maybe Int
    -> Maybe BC.ByteString --ByteString
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
            return (Right $ CommentArray rows)) $ \e
        --let err = E.decodeUtf8 $ sqlErrorMsg e
     -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
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
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
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

getNewsFilterByTagInFromDb ::
       Handle
    -> Pool Connection
    -> Maybe BC.ByteString --ByteString
    -> Maybe BC.ByteString --ByteString
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
                let errState = sqlState e
                let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                logError hLogger $
                    T.concat ["Database error ", T.pack $ show errStateInt]
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
    -> Maybe BC.ByteString --ByteString
    -> Maybe BC.ByteString --ByteString
    -> BC.ByteString --ByteString
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
                let errState = sqlState e
                let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                logError hLogger $
                    T.concat ["Database error ", T.pack $ show errStateInt]
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
    -> Maybe BC.ByteString --ByteString
    -> Maybe BC.ByteString --ByteString
    -> BC.ByteString --ByteString
    -> IO (Either LBS.ByteString NewsArray)
getNewsFilterByTitleFromDb hLogger _ Nothing _ _ = do
    logError hLogger "No title parameter"
    return $ Left "No title parameter"
getNewsFilterByTitleFromDb hLogger pool (Just titleName) page_p sortParam =
    catch
        (do logInfo hLogger "Someone try get news list filtered by title"
            rows <- queryWithPool pool q [titleName]
            return (Right $ NewsArray rows)) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
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
    -> Maybe BC.ByteString --ByteString
    -> Maybe BC.ByteString --ByteString
    -> BC.ByteString --ByteString
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
    -> Maybe BC.ByteString --ByteString
    -> Maybe BC.ByteString --ByteString
    -> BC.ByteString --ByteString
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
                let errState = sqlState e
                let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                logError hLogger $
                    T.concat ["Database error ", T.pack $ show errStateInt]
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
    -> Maybe BC.ByteString --ByteString
    -> Maybe BC.ByteString --ByteString
    -> BC.ByteString --ByteString
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
                let errState = sqlState e
                let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                logError hLogger $
                    T.concat ["Database error ", T.pack $ show errStateInt]
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
    -> Maybe BC.ByteString --ByteString
    -> Maybe BC.ByteString --ByteString
    -> BC.ByteString --ByteString
    -> IO (Either LBS.ByteString NewsArray)
getNewsFilterByContentFromDb hLogger _ Nothing _ _ = do
    logError hLogger "No content parameter"
    return $ Left "No content parameter"
getNewsFilterByContentFromDb hLogger pool (Just content_c) page_p sortParam =
    catch
        (do logInfo hLogger "Someone try get news list filtered by content"
            rows <- queryWithPool pool q [BC.concat ["%", content_c, "%"]]
            return (Right $ NewsArray rows)) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
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
    -> Maybe BC.ByteString --ByteString
    -> Maybe BC.ByteString --ByteString
    -> BC.ByteString --ByteString
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
                let errState = sqlState e
                let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                logError hLogger $
                    T.concat ["Database error ", T.pack $ show errStateInt]
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
    -> Maybe BC.ByteString --ByteString
    -> Maybe BC.ByteString --ByteString
    -> BC.ByteString --ByteString
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
                let errState = sqlState e
                let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                logError hLogger $
                    T.concat ["Database error ", T.pack $ show errStateInt]
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
    -> Maybe BC.ByteString --ByteString
    -> Maybe BC.ByteString --ByteString
    -> BC.ByteString --ByteString
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
                let errState = sqlState e
                let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                logError hLogger $
                    T.concat ["Database error ", T.pack $ show errStateInt]
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

getNewsFromDb ::
       Handle
    -> Pool Connection
    -> BC.ByteString --ByteString
    -> Maybe BC.ByteString --ByteString
    -> IO (Either LBS.ByteString NewsArray)
getNewsFromDb hLogger pool sortParam pageParam =
    catch
        (do logInfo hLogger "Someone try get news list"
            rows <- query_WithPool pool q
            return $ Right (NewsArray rows)) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
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
