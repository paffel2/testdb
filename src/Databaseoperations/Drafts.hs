{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Databaseoperations.Drafts where

import           Control.Monad.Except       (MonadError (catchError, throwError))
import qualified Data.ByteString.Char8      as BC
import           Data.Functor               (void)
import           Data.Pool                  (Pool)
import           Database.PostgreSQL.Simple (Connection, In (In),
                                             Only (Only, fromOnly))
import           HelpFunction               (someErrorToInt, toQuery)
import           PostgreSqlWithPool         (executeManyWithPool,
                                             executeWithPool, execute_WithPool,
                                             queryWithPool)
import           Types.Drafts               (Draft, DraftArray (DraftArray),
                                             DraftInf (..),
                                             DraftTags (getDraftTags))
import           Types.Images               (Image)
import           Types.Other                (Id (getId), MonadIOWithError,
                                             SendId,
                                             SomeError (BadToken, OtherError),
                                             Token, TokenLifeTime)
import           Types.Users                (TokenProfile (TokenProfile))

-----------------------------------------------------------------------------------------------------
checkAuthor ::
       MonadIOWithError m
    => Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> m SendId
checkAuthor _ _ Nothing = throwError $ OtherError "No token parameter"
checkAuthor pool tokenLifeTime (Just token) = do
    rows <- queryWithPool pool q [token]
    if Prelude.null rows
        then do
            throwError BadToken
        else return (fromOnly $ Prelude.head rows)
  where
    q =
        toQuery $
        "select author_id from authors join users using (user_id) join tokens using (user_id) where token = ? \
             \and (now()- tokens.creation_date) < make_interval(secs => " <>
        BC.pack (show tokenLifeTime) <> ")"

getDraftsByAuthorToken ::
       MonadIOWithError m
    => Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> m DraftArray
getDraftsByAuthorToken _ _ Nothing = do
    throwError $ OtherError "No token parameter"
getDraftsByAuthorToken pool tokenLifeTime (Just token) = do
    rows <- queryWithPool pool q (TokenProfile token tokenLifeTime)
    if Prelude.null rows
        then throwError $ OtherError "You are not author or don't have drafts "
        else return $ DraftArray rows
  where
    q =
        toQuery
            "select short_title, date_of_changes, category_id, draft_text, main_image, array_agg(image_id) \
             \from drafts join drafts_images using (draft_id) join authors using (author_id) join tokens using (user_id) \
             \where token = ? and (now() - tokens.creation_date) < make_interval(secs => ?) group by draft_id"

getDraftByIdFromDb ::
       MonadIOWithError m
    => Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> Id
    -> m Draft
getDraftByIdFromDb _ _ Nothing _ =
    throwError $ OtherError "Draft not sended. No token parameter"
getDraftByIdFromDb pool tokenLifeTime token draftId = do
    chAuthor <- checkAuthor pool tokenLifeTime token
    rows <- queryWithPool pool q [chAuthor]
    if Prelude.null rows
        then throwError $ OtherError "Wrong draft id or draft not exist"
        else return $ Prelude.head rows
  where
    q =
        toQuery $
        "with image_arr as (select array_agg(image_id) from drafts_images where draft_id = " <>
        (BC.pack . show . getId $ draftId) <>
        "), " <>
        " tags_arr as (select array_agg(tag_name) from draft_tags join tags using (tag_id) where draft_id = " <>
        (BC.pack . show . getId $ draftId) <>
        ") " <>
        "select short_title, date_of_changes, category_id, draft_text, main_image, " <>
        "(select * from image_arr) as other_images, (select * from  tags_arr) as tags from drafts where author_id = ? and draft_id = " <>
        (BC.pack . show . getId $ draftId)

deleteDraftFromDb ::
       MonadIOWithError m
    => Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> Maybe Id
    -> m ()
deleteDraftFromDb _ _ _ Nothing = do
    throwError $ OtherError "Draft not deleted. No draft_id parameter"
deleteDraftFromDb pool tokenLifeTime token (Just draftId) = do
    authorId <- checkAuthor pool tokenLifeTime token
    n <-
        executeWithPool
            pool
            "delete from drafts where draft_id = ? and author_id = ?"
            (draftId, authorId)
    if n > 0
        then return ()
        else throwError $ OtherError "Draft not exist or bad draft_id parameter"

publicNewsOnDb ::
       MonadIOWithError m
    => Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> Id
    -> m SendId
publicNewsOnDb pool tokenLifeTime token draftId =
    checkAuthor pool tokenLifeTime token >>= insertNews >>= insertTags >>=
    insertImages
  where
    insertNews authorId = do
        let q =
                toQuery
                    "with dus as (select short_title,author_id, category_id,draft_text,main_image from drafts where author_id = ? \
                             \and draft_id = ?) \
                             \insert into news (short_title, date_creation, author_id, category_id, news_text, main_image) values \
                            \((select short_title from dus), now(), \
                            \(select author_id from dus), \
                            \(select category_id from dus), \
                            \(select draft_text from dus), \
                            \(select main_image from dus)) returning news_id"
        rows <- queryWithPool pool q (authorId, draftId)
        if Prelude.null rows
            then throwError $ OtherError "News not published"
            else return $ fromOnly $ Prelude.head rows
    insertTags newsId = do
        let q =
                toQuery $
                "with tags_ids as (select tag_id from draft_tags where draft_id = " <>
                BC.pack (show $ getId draftId) <>
                " ), " <>
                "tag_n as (select " <>
                BC.pack (show newsId) <>
                " as news_id, tag_id from tags_ids) " <>
                "insert into news_tags (news_id, tag_id) select * from tag_n"
        _ <- execute_WithPool pool q
        return newsId
    insertImages newsId = do
        let q =
                toQuery $
                "with images_ids as (select image_id from drafts_images where draft_id = " <>
                BC.pack (show $ getId draftId) <>
                "), " <>
                "image_n as (select " <>
                BC.pack (show newsId) <>
                " as news_id, image_id from images_ids) " <>
                "insert into news_images (news_id, image_id) select * from image_n"
        _ <- execute_WithPool pool q
        return newsId

createDraftOnDb ::
       MonadIOWithError m
    => Pool Connection
    -> TokenLifeTime
    -> DraftInf
    -> Maybe DraftTags
    -> Maybe Image
    -> Maybe [Image]
    -> m SendId
createDraftOnDb _ _ DraftInf {draftInfToken = Nothing} _ _ _ =
    throwError $ OtherError "No token param"
createDraftOnDb _ _ DraftInf {draftInfCategory = Nothing} _ _ _ =
    throwError $ OtherError "No category field"
createDraftOnDb _ _ _ Nothing _ _ = throwError $ OtherError "No tags field"
createDraftOnDb _ _ DraftInf {draftInfTitle = Nothing} _ _ _ =
    throwError $ OtherError "No short_title field"
createDraftOnDb _ _ DraftInf {draftInfText = Nothing} _ _ _ =
    throwError $ OtherError "No text field"
createDraftOnDb pool tokenLifeTime draftUpd@(DraftInf (Just _) (Just _) (Just _) (Just _)) (Just tagsList) mainImage imagesList = do
    draftId <- newDraft tokenLifeTime pool draftUpd
    createTagConnections pool tagsList draftId >>= loadMainImage mainImage pool >>=
        loadImages imagesList pool

newDraft ::
       MonadIOWithError m
    => TokenLifeTime
    -> Pool Connection
    -> DraftInf
    -> m Int
newDraft tokenLifeTime pool draftUpd =
    catchError
        (do let q =
                    toQuery $
                    "with get_a as (select author_id from authors join tokens using (user_id) where token = ?" <>
                    " and (now() - tokens.creation_date) < make_interval(secs => " <>
                    BC.pack (show tokenLifeTime) <>
                    ")), get_c as (select category_id from categories where category_name = ?) " <>
                    "insert into drafts (author_id,short_title,date_of_changes,category_id,draft_text) values ((select author_id from get_a)," <>
                    " ? ,now(),(select category_id from get_c)," <>
                    " ?) returning draft_id"
            rows <- queryWithPool pool q draftUpd
            if Prelude.null rows
                then throwError $ OtherError "Draft not created"
                else return $ fromOnly $ Prelude.head rows) $ \e ->
        case someErrorToInt e of
            23502 -> throwError BadToken
            _     -> throwError e

getTagsIds :: MonadIOWithError m => Pool Connection -> BC.ByteString -> m [Int]
getTagsIds pool tagsBs = do
    let n = Prelude.length $ BC.split ' ' tagsBs
    let l = BC.split ' ' tagsBs
    let q = toQuery $ BC.concat ["select tag_id from tags where tag_name in ?"]
    rows <- queryWithPool pool q (Only (In l))
    if Prelude.length rows < n
        then throwError $ OtherError "Someone tags not exist"
        else do
            return $ fromOnly <$> rows

createTagConnections ::
       MonadIOWithError m => Pool Connection -> DraftTags -> Int -> m Int
createTagConnections pool tagsList draftId = do
    tagIds <- getTagsIds pool (getDraftTags tagsList)
    let q = toQuery "insert into draft_tags (draft_id,tag_id) values (?,?)"
    let a = Prelude.map (draftId, ) tagIds
    let nt = Prelude.length tagIds
    n <- executeManyWithPool pool q a
    if fromIntegral n < nt
        then throwError $ OtherError "Some tags not added"
        else return draftId

loadMainImage ::
       MonadIOWithError m => Maybe Image -> Pool Connection -> Int -> m Int
loadMainImage Nothing _ draftId = return draftId
loadMainImage (Just image) pool draftId = do
    let q =
            toQuery $
            "with m_id as (insert into images (image_name,content_type, image_b) values (?,?,?) returning image_id) " <>
            "update drafts set main_image = (select * from m_id) where draft_id = " <>
            BC.pack (show draftId)
    n <- executeWithPool pool q image
    if n < 1
        then do
            throwError $ OtherError "Image not loaded"
        else return draftId

loadImages ::
       MonadIOWithError m => Maybe [Image] -> Pool Connection -> Int -> m SendId
loadImages Nothing _ draftId = return draftId
loadImages (Just images) pool draftId = do
    let q =
            toQuery $
            "with m_id as (insert into images (image_name,content_type, image_b) values (?,?,?) returning image_id), " <>
            "d_i as (select " <>
            BC.pack (show draftId) <>
            " as draft_id, image_id from m_id) " <>
            "insert into drafts_images (draft_id,image_id) select * from d_i"
    n <- executeManyWithPool pool q images
    if fromIntegral n < Prelude.length images
        then throwError $ OtherError "Images not loaded"
        else return draftId

--------------------------------------------------------------------------------------------------------------------------
updateDraftInDb ::
       MonadIOWithError m
    => Pool Connection
    -> TokenLifeTime
    -> DraftInf
    -> Maybe DraftTags
    -> Maybe Image
    -> Maybe [Image]
    -> Id
    -> m ()
updateDraftInDb _ _ DraftInf {draftInfToken = Nothing} _ _ _ _ =
    throwError $ OtherError "No token param"
updateDraftInDb _ _ DraftInf {draftInfCategory = Nothing} _ _ _ _ =
    throwError $ OtherError "No category field"
updateDraftInDb _ _ _ Nothing _ _ _ = throwError $ OtherError "No tags field"
updateDraftInDb _ _ DraftInf {draftInfTitle = Nothing} _ _ _ _ =
    throwError $ OtherError "No short_title field"
updateDraftInDb _ _ DraftInf {draftInfText = Nothing} _ _ _ _ =
    throwError $ OtherError "No text field"
updateDraftInDb pool tokenLifeTime draftUpd@(DraftInf (Just _) (Just _) (Just _) (Just _)) (Just tagsList) mainImage imagesList draftId =
    void $
    updateDraft >>= deleteTagConnections >>= createTagConnections pool tagsList >>=
    deleteMainImage mainImage >>=
    loadMainImage mainImage pool >>=
    deleteOldImages imagesList >>=
    loadImages imagesList pool
  where
    updateDraft = do
        let q =
                toQuery $
                "with get_a as (select author_id from authors join tokens using (user_id) where token = ?" <>
                " and (now() - tokens.creation_date) < make_interval(secs => " <>
                BC.pack (show tokenLifeTime) <>
                ")), get_c as (select category_id from categories where category_name = ?) " <>
                "update drafts set short_title = ?, date_of_changes = now(), category_id = (select * from get_c), " <>
                "draft_text = ? where draft_id = " <>
                BC.pack (show $ getId draftId)
        n <- executeWithPool pool q draftUpd
        if n < 1
            then do
                throwError $ OtherError "Draft not updated"
            else return (getId draftId)
    deleteTagConnections mess = do
        let q =
                toQuery $
                "delete from draft_tags where draft_id = " <>
                BC.pack (show $ getId draftId)
        n <- execute_WithPool pool q
        if n < 1
            then throwError $ OtherError "Draft tags not updated"
            else return mess
    deleteMainImage Nothing mess = return mess
    deleteMainImage (Just _) mess = do
        let q =
                toQuery $
                "with m_id as (select main_image from drafts where draft_id = " <>
                BC.pack (show $ getId draftId) <>
                ") " <>
                "delete from images where image_id = (select * from m_id)"
        _ <- execute_WithPool pool q
        return mess
    deleteOldImages Nothing mess = return mess
    deleteOldImages (Just _) mess = do
        let q =
                toQuery $
                "delete from images where image_id in (select image_id from drafts_images where draft_id = " <>
                BC.pack (show $ getId draftId) <> ")"
        _ <- execute_WithPool pool q
        return mess
