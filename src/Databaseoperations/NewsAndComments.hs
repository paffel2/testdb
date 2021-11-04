{-# LANGUAGE NamedFieldPuns    #-}
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

import           Logger                        (LoggerHandle, logError, logInfo)
import           PostgreSqlWithPool            (executeWithPool, queryWithPool,
                                                query_WithPool)
import           Types.NewsAndComments         (AfterDateFilterParam,
                                                AuthorFilterParam (getAuthorFp),
                                                BeforeDateFilterParam,
                                                CategoryFilterParam (getCategoryFp),
                                                CommentArray (CommentArray),
                                                CommentWithoutTokenLifeTime (..),
                                                ContentFilterParam (getContentFp),
                                                DateFilterParam, GetNews,
                                                NewsArray (NewsArray),
                                                Sort (getSort),
                                                TagAllFilterParam (getTagAllFp),
                                                TagFilterParam,
                                                TagInFilterParam (getTagInFp),
                                                TitleFilterParam (getTitleFp),
                                                toComment)
import           Types.Other                   (Id (getId), Page (getPage),
                                                SomeError (BadToken, DatabaseError, OtherError),
                                                Token, TokenLifeTime)

addCommentToDb ::
       Pool Connection
    -> TokenLifeTime
    -> LoggerHandle IO
    -> CommentWithoutTokenLifeTime
    -> IO (Either SomeError ())
addCommentToDb _ _ hLogger CommentWithoutTokenLifeTime {commentWTLNewsId = Nothing} = do
    logError hLogger "Commentary not added. No news id parameter"
    return . Left . OtherError $ "Commentary not added. No news id parameter"
addCommentToDb _ _ hLogger CommentWithoutTokenLifeTime {commentWTLText = Nothing} = do
    logError hLogger "Commentary not added. No comment parameter"
    return . Left . OtherError $ "Commentary not added. No comment parameter"
addCommentToDb _ _ hLogger CommentWithoutTokenLifeTime {commentWTLToken = Nothing} = do
    logError hLogger "Commentary not added. No token parameter"
    return . Left . OtherError $ "Commentary not added. No token parameter"
addCommentToDb pool tokenLifeTime hLogger com =
    catch
        (do result <- executeWithPool pool q (toComment tokenLifeTime com)
            if result > 0
                then do
                    logInfo hLogger "Commentary added."
                    return $ Right ()
                else do
                    return . Left . OtherError $ "Commentary not added") $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ "Database error " <> T.pack (show errStateInt)
        case errStateInt of
            23503 -> return . Left . OtherError $ "News not exist"
            23502 -> return $ Left BadToken
            _     -> return $ Left DatabaseError
  where
    q =
        "insert into users_comments (user_id, comment_text,news_id,comment_time) values (check_token(?,?),?,?,now())"

deleteCommentFromDb ::
       Pool Connection
    -> TokenLifeTime
    -> LoggerHandle IO
    -> Maybe Token
    -> Maybe Id
    -> IO (Either SomeError ())
deleteCommentFromDb _ _ hLogger _ Nothing = do
    logError hLogger "Commentary not deleted. Bad comment id"
    return . Left . OtherError $ "Commentary not deleted. Bad comment id"
deleteCommentFromDb pool tokenLifeTime hLogger token (Just commentId) = do
    isAdmin <- checkAdmin hLogger pool tokenLifeTime token
    case isAdmin of
        (False, e) -> return $ Left e
        (True, _) ->
            catch
                (do n <-
                        executeWithPool
                            pool
                            "delete from users_comments where comment_id = ?"
                            [commentId]
                    if n > 0
                        then do
                            logInfo hLogger "Commentary deleted"
                            return $ Right ()
                        else do
                            logError hLogger "Commentary not deleted"
                            return . Left . OtherError $
                                "Commentary not deleted") $ \e -> do
                let errState = sqlState e
                let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                logError hLogger $
                    "Database error " <> T.pack (show errStateInt)
                return $ Left DatabaseError

getCommentsByNewsIdFromDb ::
       Pool Connection
    -> LoggerHandle IO
    -> Maybe Id
    -> Maybe Page
    -> IO (Either SomeError CommentArray)
getCommentsByNewsIdFromDb _ hLogger Nothing _ = do
    logError hLogger "List of commentaries sended not. No news parameter"
    return . Left . OtherError $ "No news parameter"
getCommentsByNewsIdFromDb pool hLogger (Just newsId) page =
    catch
        (do rows <- queryWithPool pool q [newsId]
            logInfo hLogger "List of commentaries sended"
            return (Right $ CommentArray rows)) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        case errStateInt of
            23503 -> return . Left . OtherError $ "News not exist"
            23502 -> return $ Left BadToken
            _     -> return $ Left DatabaseError
  where
    pg =
        if isNothing page
            then " limit 10 offset 0"
            else " limit 10 offset " <>
                 BC.pack (show $ (maybe 1 getPage page - 1) * 10)
    q =
        toQuery $
        "select (concat(first_name, ' ', last_name)) as author_name, comment_text, comment_time, comment_id \
        \from users_comments join users using (user_id) where news_id = ? order by comment_time" <>
        pg

getNewsByIdFromDb ::
       Pool Connection
    -> LoggerHandle IO
    -> Maybe Id
    -> IO (Either SomeError GetNews)
getNewsByIdFromDb _ hLogger Nothing = do
    logError hLogger "Bad news_id parameter"
    return . Left . OtherError $ "Bad news_id parameter"
getNewsByIdFromDb pool hLogger (Just newsId) =
    catch
        (do rows <- query_WithPool pool q
            if Prelude.null rows
                then do
                    return . Left . OtherError $ "News not exist"
                else do
                    logInfo hLogger "News sended"
                    return $ Right $ Prelude.head rows) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ "Database error " <> T.pack (show errStateInt)
        return $ Left DatabaseError
  where
    q =
        toQuery $
        "with image_arr as (select array_agg(image_id) from news_images where news_id = " <>
        (BC.pack . show . getId $ newsId) <>
        "), " <>
        " tags_arr as (select array_agg(tag_name) from news_tags join tags using (tag_id) where news_id = " <>
        (BC.pack . show . getId $ newsId) <>
        ") " <>
        "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id) as category_name, news_text, main_image, " <>
        "(select * from image_arr) as other_images, (select * from  tags_arr) as tags from news join authors using (author_id) join users using (user_id) where news_id = " <>
        (BC.pack . show . getId $ newsId)

getNewsFilterByTagInFromDb ::
       Pool Connection
    -> LoggerHandle IO
    -> Maybe TagInFilterParam
    -> Maybe Page
    -> IO (Either SomeError NewsArray)
getNewsFilterByTagInFromDb _ hLogger Nothing _ = do
    logError hLogger "No tag parameter"
    return . Left . OtherError $ "No tag parameter"
getNewsFilterByTagInFromDb pool hLogger (Just tagList) page = do
    logInfo hLogger "Someone try get news list filtered by tag_in parameter"
    catch
        (do rows <- queryWithPool pool q (Only (In (getTagInFp tagList)))
            logInfo hLogger "News sended"
            return (Right $ NewsArray rows)) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left DatabaseError
  where
    pg =
        if isNothing page
            then " limit 10 offset 0"
            else " limit 10 offset " <>
                 BC.pack (show $ (maybe 1 getPage page - 1) * 10)
    q =
        toQuery $
        "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, \
            \take_categories_list(category_id) as category_name, news_text from news join authors using (author_id) \
            \join users using (user_id) join news_tags using (news_id) where tag_id in ? \
            \group by news_id,author_name order by 2 DESC" <>
        pg

getNewsFilterByCategoryIdFromDb ::
       Pool Connection
    -> LoggerHandle IO
    -> Maybe CategoryFilterParam
    -> Maybe Page
    -> Sort
    -> IO (Either SomeError NewsArray)
getNewsFilterByCategoryIdFromDb _ hLogger Nothing _ _ = do
    logError hLogger "No category parameter"
    return . Left . OtherError $ "No category parameter"
getNewsFilterByCategoryIdFromDb pool hLogger (Just categoruId) page sortParam = do
    logInfo hLogger "Someone try get news list filtered by category id"
    catch
        (do rows <- queryWithPool pool q [getCategoryFp categoruId]
            logInfo hLogger "News sended"
            return (Right $ NewsArray rows)) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left DatabaseError
  where
    sort =
        if getSort sortParam == ""
            then ""
            else BC.concat [" order by ", getSort sortParam, " DESC"]
    pg =
        if isNothing page
            then " limit 10 offset 0"
            else " limit 10 offset " <>
                 BC.pack (show $ (maybe 1 getPage page - 1) * 10)
    q =
        toQuery $
        "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, \
            \take_categories_list(category_id) as category_name, news_text  from news  join authors using (author_id) join users using (user_id) \
            \where category_id = ? " <>
        sort <> pg

getNewsFilterByTitleFromDb ::
       Pool Connection
    -> LoggerHandle IO
    -> Maybe TitleFilterParam
    -> Maybe Page
    -> Sort
    -> IO (Either SomeError NewsArray)
getNewsFilterByTitleFromDb _ hLogger Nothing _ _ = do
    logError hLogger "No title parameter"
    return . Left . OtherError $ "No title parameter"
getNewsFilterByTitleFromDb pool hLogger (Just titleName) page sortParam =
    catch
        (do logInfo hLogger "Someone try get news list filtered by title"
            logInfo hLogger "News sended"
            rows <- queryWithPool pool q [getTitleFp titleName]
            return (Right $ NewsArray rows)) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left DatabaseError
  where
    sort =
        if getSort sortParam == ""
            then ""
            else " order by " <> getSort sortParam <> " DESC"
    pg =
        if isNothing page
            then " limit 10 offset 0"
            else " limit 10 offset " <>
                 BC.pack (show $ (maybe 1 getPage page - 1) * 10)
    q =
        toQuery $
        "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, \
            \take_categories_list(category_id) as category_name, news_text from news join authors using (author_id) join users using (user_id) \
            \where short_title = ? " <>
        sort <> pg

getNewsFilterByAuthorNameFromDb ::
       Pool Connection
    -> LoggerHandle IO
    -> Maybe AuthorFilterParam
    -> Maybe Page
    -> Sort
    -> IO (Either SomeError NewsArray)
getNewsFilterByAuthorNameFromDb _ hLogger Nothing _ _ = do
    logError hLogger "No author_name parameter"
    return . Left . OtherError $ "No author_name parameter"
getNewsFilterByAuthorNameFromDb pool hLogger (Just authorName) page sortParam =
    catch
        (do logInfo
                hLogger
                "Someone try get news list filtered by author's name"
            rows <- queryWithPool pool q [getAuthorFp authorName]
            logInfo hLogger "News sended"
            return (Right $ NewsArray rows)) $ \e -> do
        let err = E.decodeUtf8 $ sqlErrorMsg e
        logError hLogger err
        return $ Left DatabaseError
  where
    sort =
        if getSort sortParam == ""
            then ""
            else " order by " <> getSort sortParam <> " DESC"
    pg =
        if isNothing page
            then " limit 10 offset 0"
            else " limit 10 offset " <>
                 BC.pack (show $ (maybe 1 getPage page - 1) * 10)
    q =
        toQuery $
        "select * from (select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, \
            \take_categories_list(category_id) as category_name, news_text from news join authors using (author_id) join users using (user_id)) as temp_t \
            \where author_name = ? " <>
        sort <> pg

getNewsFilterByDateFromDb ::
       Pool Connection
    -> LoggerHandle IO
    -> Maybe DateFilterParam
    -> Maybe Page
    -> Sort
    -> IO (Either SomeError NewsArray)
getNewsFilterByDateFromDb _ hLogger Nothing _ _ = do
    logError hLogger "No date parameter"
    return . Left . OtherError $ "No date parameter"
getNewsFilterByDateFromDb pool hLogger (Just date) page sortParam = do
    logInfo hLogger "Someone try get news list filtered by date"
    catch
        (do rows <- queryWithPool pool q [date]
            logInfo hLogger "News sended"
            return (Right $ NewsArray rows)) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left DatabaseError
  where
    sort =
        if getSort sortParam == ""
            then ""
            else " order by " <> getSort sortParam <> " DESC"
    pg =
        if isNothing page
            then " limit 10 offset 0"
            else " limit 10 offset " <>
                 BC.pack (show $ (maybe 1 getPage page - 1) * 10)
    q =
        toQuery $
        "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, \
            \take_categories_list(category_id) as category_name, news_text from news join authors using (author_id) join users using (user_id) \
            \where date_creation = ? " <>
        sort <> pg

getNewsFilterByTagAllFromDb ::
       Pool Connection
    -> LoggerHandle IO
    -> Maybe TagAllFilterParam
    -> Maybe Page
    -> Sort
    -> IO (Either SomeError NewsArray)
getNewsFilterByTagAllFromDb _ hLogger Nothing _ _ = do
    logError hLogger "No tag parameter"
    return . Left . OtherError $ "No tag parameter"
getNewsFilterByTagAllFromDb pool hLogger (Just tagList) page sortParam = do
    logInfo hLogger "Someone try get news list filtered by tag_all parameter"
    catch
        (do let tagIdsList = getTagAllFp tagList
            let countNum = BC.pack $ show $ Prelude.length tagIdsList - 1
            let q =
                    toQuery $
                    "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, \
                        \take_categories_list(category_id) as category_name, news_text  from news join authors using (author_id) \
                        \join users using (user_id) join news_tags using (news_id) where tag_id in ? \
                        \group by news_id,author_name having count(*) > " <>
                    countNum <> " " <> sort <> pg
            rows <- queryWithPool pool q (Only (In tagIdsList))
            logInfo hLogger "News sended"
            return (Right $ NewsArray rows)) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left DatabaseError
  where
    sort =
        if getSort sortParam == ""
            then ""
            else " order by " <> getSort sortParam <> " DESC"
    pg =
        if isNothing page
            then " limit 10 offset 0"
            else " limit 10 offset " <>
                 BC.pack (show $ (maybe 1 getPage page - 1) * 10)

getNewsFilterByContentFromDb ::
       Pool Connection
    -> LoggerHandle IO
    -> Maybe ContentFilterParam
    -> Maybe Page
    -> Sort
    -> IO (Either SomeError NewsArray)
getNewsFilterByContentFromDb _ hLogger Nothing _ _ = do
    logError hLogger "No content parameter"
    return . Left . OtherError $ "No content parameter"
getNewsFilterByContentFromDb pool hLogger (Just contentFilter) page sortParam =
    catch
        (do logInfo hLogger "Someone try get news list filtered by content"
            rows <-
                queryWithPool
                    pool
                    q
                    [T.concat ["%", getContentFp contentFilter, "%"]]
            logInfo hLogger "News sended"
            return (Right $ NewsArray rows)) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left DatabaseError
  where
    sort =
        if getSort sortParam == ""
            then ""
            else " order by " <> getSort sortParam <> " DESC"
    pg =
        if isNothing page
            then " limit 10 offset 0"
            else " limit 10 offset " <>
                 BC.pack (show $ (maybe 1 getPage page - 1) * 10)
    q =
        toQuery $
        "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, \
            \take_categories_list(category_id) as category_name, news_text from news join authors using (author_id) join users using (user_id) \
            \where news_text like ? " <>
        sort <> pg

getNewsFilterByAfterDateFromDb ::
       Pool Connection
    -> LoggerHandle IO
    -> Maybe AfterDateFilterParam
    -> Maybe Page
    -> Sort
    -> IO (Either SomeError NewsArray)
getNewsFilterByAfterDateFromDb _ hLogger Nothing _ _ = do
    logError hLogger "No date parameter"
    return . Left . OtherError $ "No date parameter"
getNewsFilterByAfterDateFromDb pool hLogger (Just date) page sortParam = do
    logInfo hLogger "Someone try get news list filtered by after_date parameter"
    catch
        (do rows <- queryWithPool pool q [date]
            logInfo hLogger "News sended"
            return (Right $ NewsArray rows)) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left DatabaseError
  where
    sort =
        if getSort sortParam == ""
            then ""
            else " order by " <> getSort sortParam <> " DESC"
    pg =
        if isNothing page
            then " limit 10 offset 0"
            else " limit 10 offset " <>
                 BC.pack (show $ (maybe 1 getPage page - 1) * 10)
    q =
        toQuery $
        "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, \
            \take_categories_list(category_id) as category_name, news_text from news join authors using (author_id) join users using (user_id) \
            \where date_creation > ? " <>
        sort <> pg

getNewsFilterByBeforeDateFromDb ::
       Pool Connection
    -> LoggerHandle IO
    -> Maybe BeforeDateFilterParam
    -> Maybe Page
    -> Sort
    -> IO (Either SomeError NewsArray)
getNewsFilterByBeforeDateFromDb _ hLogger Nothing _ _ = do
    logError hLogger "No date parameter"
    return . Left . OtherError $ "No date parameter"
getNewsFilterByBeforeDateFromDb pool hLogger (Just date) page sortParam = do
    logInfo
        hLogger
        "Someone try get news list filtered by before_date parameter"
    catch
        (do rows <- queryWithPool pool q [date]
            logInfo hLogger "News sended"
            return (Right $ NewsArray rows)) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left DatabaseError
  where
    sort =
        if getSort sortParam == ""
            then ""
            else " order by " <> getSort sortParam <> " DESC"
    pg =
        if isNothing page
            then " limit 10 offset 0"
            else " limit 10 offset " <>
                 BC.pack (show $ (maybe 1 getPage page - 1) * 10)
    q =
        toQuery $
        "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, \
            \take_categories_list(category_id) as category_name, news_text from news join authors using (author_id) join users using (user_id) \
            \where date_creation < ? " <>
        sort <> pg

getNewsFilterByTagIdFromDb ::
       Pool Connection
    -> LoggerHandle IO
    -> Maybe TagFilterParam
    -> Maybe Page
    -> Sort
    -> IO (Either SomeError NewsArray)
getNewsFilterByTagIdFromDb _ hLogger Nothing _ _ = do
    logError hLogger "No tag parameter"
    return . Left . OtherError $ "No tag parameter"
getNewsFilterByTagIdFromDb pool hLogger (Just tagId) page sortParam = do
    logInfo hLogger "Someone try get news list filtered by tag"
    catch
        (do rows <- queryWithPool pool q [tagId]
            logInfo hLogger "News sended"
            return (Right $ NewsArray rows)) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left DatabaseError
  where
    sort =
        if getSort sortParam == ""
            then ""
            else " order by " <> getSort sortParam <> " DESC"
    pg =
        if isNothing page
            then " limit 10 offset 0"
            else " limit 10 offset " <>
                 BC.pack (show $ (maybe 1 getPage page - 1) * 10)
    q =
        toQuery $
        "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, \
            \take_categories_list(category_id) as category_name, news_text from news join authors using (author_id) join users using (user_id) \
            \join news_tags using (news_id) where tag_id = ?" <>
        sort <> pg

getNewsFromDb ::
       Pool Connection
    -> LoggerHandle IO
    -> Sort
    -> Maybe Page
    -> IO (Either SomeError NewsArray)
getNewsFromDb pool hLogger sortParam pageParam =
    catch
        (do logInfo hLogger "Someone try get news list"
            rows <- query_WithPool pool q
            logInfo hLogger "News sended"
            return $ Right (NewsArray rows)) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $
            T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left DatabaseError
  where
    sort =
        if getSort sortParam == ""
            then ""
            else " order by " <> getSort sortParam <> " DESC"
    pg =
        if isNothing pageParam
            then " limit 10 offset 0"
            else " limit 10 offset " <>
                 BC.pack (show $ (maybe 1 getPage pageParam - 1) * 10)
    q =
        toQuery $
        "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, \
            \take_categories_list(category_id) as category_name, news_text  from news join authors using (author_id) join users using (user_id)" <>
        sort <> pg
