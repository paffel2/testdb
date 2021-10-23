{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.NewsAndComments where

import           Control.Exception             (catch)
import qualified Data.ByteString.Char8         as BC
import           Data.Maybe                    (fromMaybe, isNothing)
import           Data.Pool                     (Pool)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import           Database.PostgreSQL.Simple    (Connection, In (In),
                                                Only (Only),
                                                SqlError (sqlErrorMsg, sqlState))
import           Databaseoperations.CheckAdmin (checkAdmin)
import           HelpFunction                  (readByteStringToInt, toQuery)

import           Logger                        (Handle, logError, logInfo)
import           PostgreSqlWithPool            (executeWithPool, queryWithPool,
                                                query_WithPool)
import           Types.NewsAndComments         (AfterDateFilterParam,
                                                AuthorFilterParam (from_author_fp),
                                                BeforeDateFilterParam,
                                                CategoryFilterParam (from_category_fp),
                                                Comment (Comment),
                                                CommentArray (CommentArray),
                                                ContentFilterParam (from_content_fp),
                                                DateFilterParam, GetNews,
                                                NewsArray (NewsArray),
                                                Sort (from_sort),
                                                TagAllFilterParam (from_tag_all_fp),
                                                TagFilterParam,
                                                TagInFilterParam (from_tag_in_fp),
                                                TitleFilterParam (from_title_fp))
import           Types.Other                   (ErrorMessage, Id (from_id),
                                                Page (from_page),
                                                SuccessMessage, Token,
                                                TokenLifeTime)

addCommentToDb ::
       Pool Connection
    -> Handle IO
    -> Comment
    -> IO (Either ErrorMessage SuccessMessage)
addCommentToDb _ hLogger (Comment _ _ Nothing _) = do
    logError hLogger "No news id parameter"
    return $ Left "No news id parameter"
addCommentToDb _ hLogger (Comment _ _ _ Nothing) = do
    logError hLogger "No comment parameter"
    return $ Left "No comment parameter"
addCommentToDb _ hLogger (Comment Nothing _ _ _) = do
    logError hLogger "No token parameter"
    return $ Left "No token parameter"
addCommentToDb pool hLogger com =
    catch
        (do n_r <- executeWithPool pool q com
            if n_r > 0
                then do
                    return $ Right "Comment added"
                else do
                    return $ Right "Comment not added") $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        case errStateInt of
            23503 -> return $ Left "News not exist"
            23502 -> return $ Left "Bad token"
            _     -> return $ Left "Database error"
  where
    q =
        "insert into users_comments (user_id, comment_text,news_id,comment_time) values (check_token(?,?),?,?,now())"

deleteCommentFromDb ::
       Pool Connection
    -> Handle IO
    -> TokenLifeTime
    -> Maybe Token
    -> Maybe Id
    -> IO (Either ErrorMessage SuccessMessage)
deleteCommentFromDb _ hLogger _ _ Nothing = do
    logError hLogger "Bad comment id"
    return $ Left "Bad comment id"
deleteCommentFromDb pool hLogger token_lifetime token' (Just comment_id) = do
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
                            return $ Right "Comment deleted"
                        else do
                            return $ Left "Comment not deleted") $ \e -> do
                let errState = sqlState e
                let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                logError hLogger $
                    T.concat ["Database error ", T.pack $ show errStateInt]
                return $ Left "Database error"

getCommentsByNewsIdFromDb ::
       Pool Connection
    -> Handle IO
    -> Maybe Id
    -> Maybe Page
    -> IO (Either ErrorMessage CommentArray)
getCommentsByNewsIdFromDb _ hLogger Nothing _ = do
    logError hLogger "No news parameter"
    return $ Left "No news parameter"
getCommentsByNewsIdFromDb pool hLogger (Just news_id) page_p =
    catch
        (do rows <- queryWithPool pool q [news_id]
            return (Right $ CommentArray rows)) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        case errStateInt of
            23503 -> return $ Left "News not exist"
            23502 -> return $ Left "Bad token"
            _     -> return $ Left "Database error"
  where
    pg =
        if isNothing page_p
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $ show $ (maybe 1 from_page page_p - 1) * 10
                     ]
    q =
        toQuery $
        BC.concat
            [ "select (concat(first_name, ' ', last_name)) as author_name, comment_text, comment_time, comment_id"
            , " from users_comments join users using (user_id) where news_id = ? order by comment_time"
            , pg
            ]

getNewsByIdFromDb ::
       Pool Connection
    -> Handle IO
    -> Maybe Id
    -> IO (Either ErrorMessage GetNews)
getNewsByIdFromDb _ hLogger Nothing = do
    logError hLogger "Bad news_id parameter"
    return $ Left "Bad news_id parameter"
getNewsByIdFromDb pool hLogger (Just news'_id) =
    catch
        (do rows <- query_WithPool pool q
            if Prelude.null rows
                then do
                    return $ Left "News not exist"
                else do
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
            , BC.pack . show . from_id $ news'_id
            , "), "
            , " tags_arr as (select array_agg(tag_name) from news_tags join tags using (tag_id) where news_id = "
            , BC.pack . show . from_id $ news'_id
            , ") "
            , "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id) as category_name, news_text, main_image, "
            , "(select * from image_arr) as other_images, (select * from  tags_arr) as tags from news join authors using (author_id) join users using (user_id) where news_id = "
            , BC.pack . show . from_id $ news'_id
            ]

getNewsFilterByTagInFromDb ::
       Pool Connection
    -> Handle IO
    -> Maybe TagInFilterParam
    -> Maybe Page
    -> IO (Either ErrorMessage NewsArray)
getNewsFilterByTagInFromDb _ hLogger Nothing _ = do
    logError hLogger "No tag parameter"
    return $ Left "No tag parameter"
getNewsFilterByTagInFromDb pool hLogger (Just tag_lst) page_p = do
    logInfo hLogger "Someone try get news list filtered by tag_in parameter"
    catch
        (do rows <- queryWithPool pool q (Only (In (from_tag_in_fp tag_lst)))
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
                     , BC.pack $ show $ (maybe 1 from_page page_p - 1) * 10
                     ]
    q =
        toQuery $
        BC.concat
            [ "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, "
            , "take_categories_list(category_id) as category_name, news_text from news join authors using (author_id) "
            , "join users using (user_id) join news_tags using (news_id) where tag_id in ? "
            , "group by news_id,author_name order by 2 DESC"
            , pg
            ]

getNewsFilterByCategoryIdFromDb ::
       Pool Connection
    -> Handle IO
    -> Maybe CategoryFilterParam
    -> Maybe Page
    -> Sort
    -> IO (Either ErrorMessage NewsArray)
getNewsFilterByCategoryIdFromDb _ hLogger Nothing _ _ = do
    logError hLogger "No category parameter"
    return $ Left "No category parameter"
getNewsFilterByCategoryIdFromDb pool hLogger (Just cat_id) page_p sortParam = do
    logInfo hLogger "Someone try get news list filtered by category id"
    catch
        (do rows <- queryWithPool pool q [from_category_fp cat_id]
            return (Right $ NewsArray rows)) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left "Database error"
  where
    sort' =
        if from_sort sortParam == ""
            then ""
            else BC.concat [" order by ", from_sort sortParam, " DESC"]
    pg =
        if isNothing page_p
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $ show $ (maybe 1 from_page page_p - 1) * 10
                     ]
    q =
        toQuery $
        BC.concat
            [ "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, "
            , "take_categories_list(category_id) as category_name, news_text  from news  join authors using (author_id) join users using (user_id) "
            , "where category_id = ? "
            , sort'
            , pg
            ]

getNewsFilterByTitleFromDb ::
       Pool Connection
    -> Handle IO
    -> Maybe TitleFilterParam
    -> Maybe Page
    -> Sort
    -> IO (Either ErrorMessage NewsArray)
getNewsFilterByTitleFromDb _ hLogger Nothing _ _ = do
    logError hLogger "No title parameter"
    return $ Left "No title parameter"
getNewsFilterByTitleFromDb pool hLogger (Just titleName) page_p sortParam =
    catch
        (do logInfo hLogger "Someone try get news list filtered by title"
            rows <- queryWithPool pool q [from_title_fp titleName]
            return (Right $ NewsArray rows)) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left "Database error"
  where
    sort' =
        if from_sort sortParam == ""
            then ""
            else BC.concat [" order by ", from_sort sortParam, " DESC"]
    pg =
        if isNothing page_p
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $ show $ (maybe 1 from_page page_p - 1) * 10
                     ]
    q =
        toQuery $
        BC.concat
            [ "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, "
            , "take_categories_list(category_id) as category_name, news_text from news join authors using (author_id) join users using (user_id) "
            , "where short_title = ? "
            , sort'
            , pg
            ]

getNewsFilterByAuthorNameFromDb ::
       Pool Connection
    -> Handle IO
    -> Maybe AuthorFilterParam
    -> Maybe Page
    -> Sort
    -> IO (Either ErrorMessage NewsArray)
getNewsFilterByAuthorNameFromDb _ hLogger Nothing _ _ = do
    logError hLogger "No author_name parameter"
    return $ Left "No author_name parameter"
getNewsFilterByAuthorNameFromDb pool hLogger (Just authorName) page_p sortParam =
    catch
        (do logInfo
                hLogger
                "Someone try get news list filtered by author's name"
            rows <- queryWithPool pool q [from_author_fp authorName]
            return (Right $ NewsArray rows)) $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        logError hLogger err
        return $ Left "Database error"
  where
    sort' =
        if from_sort sortParam == ""
            then ""
            else BC.concat [" order by ", from_sort sortParam, " DESC"]
    pg =
        if isNothing page_p
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $ show $ (maybe 1 from_page page_p - 1) * 10
                     ]
    q =
        toQuery $
        BC.concat
            [ "select * from (select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, "
            , "take_categories_list(category_id) as category_name, news_text from news join authors using (author_id) join users using (user_id)) as temp_t "
            , "where author_name = ? "
            , sort'
            , pg
            ]

getNewsFilterByDateFromDb ::
       Pool Connection
    -> Handle IO
    -> Maybe DateFilterParam
    -> Maybe Page
    -> Sort
    -> IO (Either ErrorMessage NewsArray)
getNewsFilterByDateFromDb _ hLogger Nothing _ _ = do
    logError hLogger "No date parameter"
    return $ Left "No date parameter"
getNewsFilterByDateFromDb pool hLogger (Just date) page_p sortParam = do
    logInfo hLogger "Someone try get news list filtered by date"
    catch
        (do rows <- queryWithPool pool q [date]
            return (Right $ NewsArray rows)) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left "Database error"
  where
    sort' =
        if from_sort sortParam == ""
            then ""
            else BC.concat [" order by ", from_sort sortParam, " DESC"]
    pg =
        if isNothing page_p
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $ show $ (maybe 1 from_page page_p - 1) * 10
                     ]
    q =
        toQuery $
        BC.concat
            [ "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, "
            , "take_categories_list(category_id) as category_name, news_text from news join authors using (author_id) join users using (user_id) "
            , "where date_creation = ? "
            , sort'
            , pg
            ]

getNewsFilterByTagAllFromDb ::
       Pool Connection
    -> Handle IO
    -> Maybe TagAllFilterParam
    -> Maybe Page
    -> Sort
    -> IO (Either ErrorMessage NewsArray)
getNewsFilterByTagAllFromDb _ hLogger Nothing _ _ = do
    logError hLogger "No tag parameter"
    return $ Left "No tag parameter"
getNewsFilterByTagAllFromDb pool hLogger (Just tag_lst) page_p sortParam = do
    logInfo hLogger "Someone try get news list filtered by tag_all parameter"
    catch
        (do let tag_list = from_tag_all_fp tag_lst
            let cn = BC.pack $ show $ Prelude.length tag_list - 1
            let q =
                    toQuery $
                    BC.concat
                        [ "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, "
                        , "take_categories_list(category_id) as category_name, news_text  from news join authors using (author_id) "
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
        if from_sort sortParam == ""
            then ""
            else BC.concat [" order by ", from_sort sortParam, " DESC"]
    pg =
        if isNothing page_p
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $ show $ (maybe 1 from_page page_p - 1) * 10
                     ]

getNewsFilterByContentFromDb ::
       Pool Connection
    -> Handle IO
    -> Maybe ContentFilterParam
    -> Maybe Page
    -> Sort
    -> IO (Either ErrorMessage NewsArray)
getNewsFilterByContentFromDb _ hLogger Nothing _ _ = do
    logError hLogger "No content parameter"
    return $ Left "No content parameter"
getNewsFilterByContentFromDb pool hLogger (Just content_c) page_p sortParam =
    catch
        (do logInfo hLogger "Someone try get news list filtered by content"
            rows <-
                queryWithPool
                    pool
                    q
                    [T.concat ["%", from_content_fp content_c, "%"]]
            return (Right $ NewsArray rows)) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left "Database error"
  where
    sort' =
        if from_sort sortParam == ""
            then ""
            else BC.concat [" order by ", from_sort sortParam, " DESC"]
    pg =
        if isNothing page_p
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $ show $ (maybe 1 from_page page_p - 1) * 10
                     ]
    q =
        toQuery $
        BC.concat
            [ "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, "
            , "take_categories_list(category_id) as category_name, news_text from news join authors using (author_id) join users using (user_id) "
            , "where news_text like ? "
            , sort'
            , pg
            ]

getNewsFilterByAfterDateFromDb ::
       Pool Connection
    -> Handle IO
    -> Maybe AfterDateFilterParam
    -> Maybe Page
    -> Sort
    -> IO (Either ErrorMessage NewsArray)
getNewsFilterByAfterDateFromDb _ hLogger Nothing _ _ = do
    logError hLogger "No date parameter"
    return $ Left "No date parameter"
getNewsFilterByAfterDateFromDb pool hLogger (Just date) page_p sortParam = do
    logInfo hLogger "Someone try get news list filtered by after_date parameter"
    catch
        (do rows <- queryWithPool pool q [date]
            return (Right $ NewsArray rows)) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left "Database error"
  where
    sort' =
        if from_sort sortParam == ""
            then ""
            else BC.concat [" order by ", from_sort sortParam, " DESC"]
    pg =
        if isNothing page_p
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $ show $ (maybe 1 from_page page_p - 1) * 10
                     ]
    q =
        toQuery $
        BC.concat
            [ "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, "
            , "take_categories_list(category_id) as category_name, news_text from news join authors using (author_id) join users using (user_id) "
            , "where date_creation > ? "
            , sort'
            , pg
            ]

getNewsFilterByBeforeDateFromDb ::
       Pool Connection
    -> Handle IO
    -> Maybe BeforeDateFilterParam
    -> Maybe Page
    -> Sort
    -> IO (Either ErrorMessage NewsArray)
getNewsFilterByBeforeDateFromDb _ hLogger Nothing _ _ = do
    logError hLogger "No date parameter"
    return $ Left "No date parameter"
getNewsFilterByBeforeDateFromDb pool hLogger (Just date) page_p sortParam = do
    logInfo
        hLogger
        "Someone try get news list filtered by before_date parameter"
    catch
        (do rows <- queryWithPool pool q [date]
            return (Right $ NewsArray rows)) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left "Database error"
  where
    sort' =
        if from_sort sortParam == ""
            then ""
            else BC.concat [" order by ", from_sort sortParam, " DESC"]
    pg =
        if isNothing page_p
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $ show $ (maybe 1 from_page page_p - 1) * 10
                     ]
    q =
        toQuery $
        BC.concat
            [ "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, "
            , "take_categories_list(category_id) as category_name, news_text from news join authors using (author_id) join users using (user_id) "
            , "where date_creation < ? "
            , sort'
            , pg
            ]

getNewsFilterByTagIdFromDb ::
       Pool Connection
    -> Handle IO
    -> Maybe TagFilterParam
    -> Maybe Page
    -> Sort
    -> IO (Either ErrorMessage NewsArray)
getNewsFilterByTagIdFromDb _ hLogger Nothing _ _ = do
    logError hLogger "No tag parameter"
    return $ Left "No tag parameter"
getNewsFilterByTagIdFromDb pool hLogger (Just tag_id) page_p' sortParam = do
    logInfo hLogger "Someone try get news list filtered by tag"
    catch
        (do rows <- queryWithPool pool q [tag_id]
            return (Right $ NewsArray rows)) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left "Database error"
  where
    sort' =
        if from_sort sortParam == ""
            then ""
            else BC.concat [" order by ", from_sort sortParam, " DESC"]
    pg =
        if isNothing page_p'
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $ show $ (maybe 1 from_page page_p' - 1) * 10
                     ]
    q =
        toQuery $
        BC.concat
            [ "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, "
            , "take_categories_list(category_id) as category_name, news_text from news join authors using (author_id) join users using (user_id) "
            , "join news_tags using (news_id) where tag_id = ?"
            , sort'
            , pg
            ]

getNewsFromDb ::
       Pool Connection
    -> Handle IO
    -> Sort
    -> Maybe Page
    -> IO (Either ErrorMessage NewsArray)
getNewsFromDb pool hLogger sortParam pageParam =
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
        if from_sort sortParam == ""
            then ""
            else BC.concat [" order by ", from_sort sortParam, " DESC"]
    pg =
        if isNothing pageParam
            then " limit 10 offset 0"
            else BC.concat
                     [ " limit 10 offset "
                     , BC.pack $ show $ (maybe 1 from_page pageParam - 1) * 10
                     ]
    q =
        toQuery $
        BC.concat
            [ "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id) as category_name, news_text  from news join authors using (author_id) join users using (user_id)"
            , sort'
            , pg
            ]
