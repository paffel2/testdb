{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Databaseoperations.NewsAndComments where

import           Control.Monad.Except          (MonadError (catchError, throwError))
import qualified Data.ByteString.Char8         as BC
import           Data.Pool                     (Pool)
import qualified Data.Text                     as T
import           Database.PostgreSQL.Simple    (Connection, In (In),
                                                Only (Only))
import           Databaseoperations.CheckAdmin (checkAdmin)
import           HelpFunction                  (pageToBS, someErrorToInt,
                                                sortToBS, toQuery)
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
                                                NewsArray (NewsArray), Sort,
                                                TagAllFilterParam (getTagAllFp),
                                                TagFilterParam,
                                                TagInFilterParam (getTagInFp),
                                                TitleFilterParam (getTitleFp),
                                                toComment)
import           Types.Other                   (Id (getId), MonadIOWithError,
                                                Page,
                                                SomeError (BadToken, OtherError),
                                                Token, TokenLifeTime)

-----------------------------------------------------------------------
addCommentToDb ::
       MonadIOWithError m
    => Pool Connection
    -> TokenLifeTime
    -> CommentWithoutTokenLifeTime
    -> m ()
addCommentToDb _ _ CommentWithoutTokenLifeTime {commentWTLNewsId = Nothing} =
    throwError $ OtherError "Commentary not added. No news id parameter"
addCommentToDb _ _ CommentWithoutTokenLifeTime {commentWTLText = Nothing} =
    throwError $ OtherError "Commentary not added. No comment parameter"
addCommentToDb _ _ CommentWithoutTokenLifeTime {commentWTLToken = Nothing} =
    throwError $ OtherError "Commentary not added. No token parameter"
addCommentToDb pool tokenLifeTime com =
    catchError
        (do result <- executeWithPool pool q (toComment tokenLifeTime com)
            if result > 0
                then return ()
                else throwError $ OtherError "Commentary not added") $ \e ->
        case someErrorToInt e of
            23503 -> throwError $ OtherError "News not exist"
            23502 -> throwError BadToken
            _     -> throwError e
  where
    q =
        "insert into users_comments (user_id, comment_text,news_id,comment_time) values (check_token(?,?),?,?,now())"

getNewsByIdFromDb ::
       MonadIOWithError m => Pool Connection -> Maybe Id -> m GetNews
getNewsByIdFromDb _ Nothing = throwError $ OtherError "Bad news_id parameter"
getNewsByIdFromDb pool (Just newsId) = do
    rows <- query_WithPool pool q
    if Prelude.null rows
        then throwError $ OtherError "News not exist"
        else return $ Prelude.head rows
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

--------------------------------------------------------------------------------------
getNewsFilterByTagInFromDb ::
       MonadIOWithError m
    => Pool Connection
    -> Maybe TagInFilterParam
    -> Maybe Page
    -> m NewsArray
getNewsFilterByTagInFromDb _ Nothing _ =
    throwError $ OtherError "No tag parameter"
getNewsFilterByTagInFromDb pool (Just tagList) page =
    NewsArray <$> queryWithPool pool q (Only (In (getTagInFp tagList)))
  where
    q =
        toQuery $
        "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, \
            \take_categories_list(category_id) as category_name, news_text from news join authors using (author_id) \
            \join users using (user_id) join news_tags using (news_id) where tag_id in ? \
            \group by news_id,author_name order by 2 DESC" <>
        pageToBS page

getNewsFilterByCategoryIdFromDb ::
       MonadIOWithError m
    => Pool Connection
    -> Maybe CategoryFilterParam
    -> Maybe Page
    -> Sort
    -> m NewsArray
getNewsFilterByCategoryIdFromDb _ Nothing _ _ =
    throwError $ OtherError "No category parameter"
getNewsFilterByCategoryIdFromDb pool (Just categoruId) page sortParam =
    NewsArray <$> queryWithPool pool q [getCategoryFp categoruId]
  where
    q =
        toQuery $
        "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, \
            \take_categories_list(category_id) as category_name, news_text  from news  join authors using (author_id) join users using (user_id) \
            \where category_id = ? " <>
        sortToBS sortParam <> pageToBS page

getNewsFilterByTitleFromDb ::
       MonadIOWithError m
    => Pool Connection
    -> Maybe TitleFilterParam
    -> Maybe Page
    -> Sort
    -> m NewsArray
getNewsFilterByTitleFromDb _ Nothing _ _ =
    throwError $ OtherError "No title parameter"
getNewsFilterByTitleFromDb pool (Just titleName) page sortParam =
    NewsArray <$> queryWithPool pool q [getTitleFp titleName]
  where
    q =
        toQuery $
        "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, \
            \take_categories_list(category_id) as category_name, news_text from news join authors using (author_id) join users using (user_id) \
            \where short_title = ? " <>
        sortToBS sortParam <> pageToBS page

getNewsFilterByAuthorNameFromDb ::
       MonadIOWithError m
    => Pool Connection
    -> Maybe AuthorFilterParam
    -> Maybe Page
    -> Sort
    -> m NewsArray
getNewsFilterByAuthorNameFromDb _ Nothing _ _ =
    throwError $ OtherError "No author_name parameter"
getNewsFilterByAuthorNameFromDb pool (Just authorName) page sortParam =
    NewsArray <$> queryWithPool pool q [getAuthorFp authorName]
  where
    q =
        toQuery $
        "select * from (select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, \
            \take_categories_list(category_id) as category_name, news_text from news join authors using (author_id) join users using (user_id)) as temp_t \
            \where author_name = ? " <>
        sortToBS sortParam <> pageToBS page

getNewsFilterByDateFromDb ::
       MonadIOWithError m
    => Pool Connection
    -> Maybe DateFilterParam
    -> Maybe Page
    -> Sort
    -> m NewsArray
getNewsFilterByDateFromDb _ Nothing _ _ =
    throwError $ OtherError "No date parameter"
getNewsFilterByDateFromDb pool (Just date) page sortParam =
    NewsArray <$> queryWithPool pool q [date]
  where
    q =
        toQuery $
        "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, \
            \take_categories_list(category_id) as category_name, news_text from news join authors using (author_id) join users using (user_id) \
            \where date_creation = ? " <>
        sortToBS sortParam <> pageToBS page

getNewsFilterByTagAllFromDb ::
       MonadIOWithError m
    => Pool Connection
    -> Maybe TagAllFilterParam
    -> Maybe Page
    -> Sort
    -> m NewsArray
getNewsFilterByTagAllFromDb _ Nothing _ _ =
    throwError $ OtherError "No tag parameter"
getNewsFilterByTagAllFromDb pool (Just tagList) page sortParam = do
    let tagIdsList = getTagAllFp tagList
    let countNum = BC.pack $ show $ Prelude.length tagIdsList - 1
    let q =
            toQuery $
            "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, \
                        \take_categories_list(category_id) as category_name, news_text  from news join authors using (author_id) \
                        \join users using (user_id) join news_tags using (news_id) where tag_id in ? \
                        \group by news_id,author_name having count(*) > " <>
            countNum <> " " <> sortToBS sortParam <> pageToBS page
    NewsArray <$> queryWithPool pool q (Only (In tagIdsList))

getNewsFilterByContentFromDb ::
       MonadIOWithError m
    => Pool Connection
    -> Maybe ContentFilterParam
    -> Maybe Page
    -> Sort
    -> m NewsArray
getNewsFilterByContentFromDb _ Nothing _ _ =
    throwError $ OtherError "No content parameter"
getNewsFilterByContentFromDb pool (Just contentFilter) page sortParam = do
    NewsArray <$>
        queryWithPool pool q [T.concat ["%", getContentFp contentFilter, "%"]]
  where
    q =
        toQuery $
        "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, \
            \take_categories_list(category_id) as category_name, news_text from news join authors using (author_id) join users using (user_id) \
            \where news_text like ? " <>
        sortToBS sortParam <> pageToBS page

getNewsFilterByAfterDateFromDb ::
       MonadIOWithError m
    => Pool Connection
    -> Maybe AfterDateFilterParam
    -> Maybe Page
    -> Sort
    -> m NewsArray
getNewsFilterByAfterDateFromDb _ Nothing _ _ =
    throwError $ OtherError "No date parameter"
getNewsFilterByAfterDateFromDb pool (Just date) page sortParam =
    NewsArray <$> queryWithPool pool q [date]
  where
    q =
        toQuery $
        "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, \
            \take_categories_list(category_id) as category_name, news_text from news join authors using (author_id) join users using (user_id) \
            \where date_creation > ? " <>
        sortToBS sortParam <> pageToBS page

getNewsFilterByBeforeDateFromDb ::
       MonadIOWithError m
    => Pool Connection
    -> Maybe BeforeDateFilterParam
    -> Maybe Page
    -> Sort
    -> m NewsArray
getNewsFilterByBeforeDateFromDb _ Nothing _ _ =
    throwError $ OtherError "No date parameter"
getNewsFilterByBeforeDateFromDb pool (Just date) page sortParam =
    NewsArray <$> queryWithPool pool q [date]
  where
    q =
        toQuery $
        "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, \
            \take_categories_list(category_id) as category_name, news_text from news join authors using (author_id) join users using (user_id) \
            \where date_creation < ? " <>
        sortToBS sortParam <> pageToBS page

getNewsFilterByTagIdFromDb ::
       MonadIOWithError m
    => Pool Connection
    -> Maybe TagFilterParam
    -> Maybe Page
    -> Sort
    -> m NewsArray
getNewsFilterByTagIdFromDb _ Nothing _ _ =
    throwError $ OtherError "No tag parameter"
getNewsFilterByTagIdFromDb pool (Just tagId) page sortParam =
    NewsArray <$> queryWithPool pool q [tagId]
  where
    q =
        toQuery $
        "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, \
            \take_categories_list(category_id) as category_name, news_text from news join authors using (author_id) join users using (user_id) \
            \join news_tags using (news_id) where tag_id = ?" <>
        sortToBS sortParam <> pageToBS page

getNewsFromDb ::
       MonadIOWithError m
    => Pool Connection
    -> Sort
    -> Maybe Page
    -> m NewsArray
getNewsFromDb pool sortParam pageParam = NewsArray <$> query_WithPool pool q
  where
    q =
        toQuery $
        "select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, \
            \take_categories_list(category_id) as category_name, news_text  from news join authors using (author_id) join users using (user_id)" <>
        sortToBS sortParam <> pageToBS pageParam

-----------------------------------------------------------------------
deleteCommentFromDb ::
       MonadIOWithError m
    => Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> Maybe Id
    -> m ()
deleteCommentFromDb _ _ _ Nothing =
    throwError $ OtherError "Commentary not deleted. Bad comment id"
deleteCommentFromDb pool tokenLifeTime token (Just commentId) = do
    checkAdmin pool tokenLifeTime token
    n <-
        executeWithPool
            pool
            "delete from users_comments where comment_id = ?"
            [commentId]
    if n > 0
        then return ()
        else throwError $ OtherError "Commentary not deleted"

------------------------------------------------------------------------------------
getCommentsByNewsIdFromDb ::
       MonadIOWithError m
    => Pool Connection
    -> Maybe Id
    -> Maybe Page
    -> m CommentArray
getCommentsByNewsIdFromDb _ Nothing _ =
    throwError $ OtherError "No news parameter"
getCommentsByNewsIdFromDb pool (Just newsId) page =
    catchError (CommentArray <$> queryWithPool pool q [newsId]) $ \e ->
        case someErrorToInt e of
            23503 -> throwError $ OtherError "News not exist"
            23502 -> throwError BadToken
            _     -> throwError e
  where
    q =
        toQuery $
        "select (concat(first_name, ' ', last_name)) as author_name, comment_text, comment_time, comment_id \
        \from users_comments join users using (user_id) where news_id = ? order by comment_time" <>
        pageToBS page
