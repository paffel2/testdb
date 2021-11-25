{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Databaseoperations.Drafts where

import           Control.Monad.Except       (MonadError (..), MonadIO)
import qualified Data.ByteString.Char8      as BC
import           Data.Pool                  (Pool)
import           Database.PostgreSQL.Simple (Connection, In (In),
                                             Only (Only, fromOnly))
import           HelpFunction               (toQuery)
import           PostgreSqlWithPool         (executeManyWithPoolNew,
                                             executeWithPoolNew,
                                             execute_WithPoolNew,
                                             queryWithPoolNew)
import           Types.Drafts               (Draft, DraftArray (DraftArray),
                                             DraftInf (..),
                                             DraftTags (getDraftTags))
import           Types.Images               (Image)
import           Types.Other                (Id (getId), SendId,
                                             SomeError (BadToken, OtherError),
                                             Token, TokenLifeTime,
                                             someErrorToInt)
import           Types.Users                (TokenProfile (TokenProfile))

-----------------------------------------------------------------------------------------------------
checkAuthor ::
       (MonadIO m, MonadError SomeError m)
    => Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> m SendId
checkAuthor _ _ Nothing = throwError $ OtherError "No token parameter"
checkAuthor pool tokenLifeTime (Just token) = do
    rows <- queryWithPoolNew pool q [token]
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
       (MonadIO m, MonadError SomeError m)
    => Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> m DraftArray
getDraftsByAuthorToken _ _ Nothing = do
    throwError $ OtherError "No token parameter"
getDraftsByAuthorToken pool tokenLifeTime (Just token) = do
    rows <- queryWithPoolNew pool q (TokenProfile token tokenLifeTime)
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
       (MonadIO m, MonadError SomeError m)
    => Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> Id
    -> m Draft
getDraftByIdFromDb _ _ Nothing _ =
    throwError $ OtherError "Draft not sended. No token parameter"
getDraftByIdFromDb pool tokenLifeTime token draftId = do
    chAuthor <- checkAuthor pool tokenLifeTime token
    rows <- queryWithPoolNew pool q [chAuthor]
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
       (MonadIO m, MonadError SomeError m)
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
        executeWithPoolNew
            pool
            "delete from drafts where draft_id = ? and author_id = ?"
            (draftId, authorId)
    if n > 0
        then return ()
        else throwError $ OtherError "Draft not exist or bad draft_id parameter"

publicNewsOnDb ::
       (MonadIO m, MonadError SomeError m)
    => Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> Id
    -> m SendId
publicNewsOnDb pool tokenLifeTime token draftId = do
    authorId <- checkAuthor pool tokenLifeTime token
    n <- insertNews authorId
    t <- insertTags n
    insertImages t
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
        rows <- queryWithPoolNew pool q (authorId, draftId)
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
        _ <- execute_WithPoolNew pool q
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
        _ <- execute_WithPoolNew pool q
        return newsId

createDraftOnDb ::
       (MonadIO m, MonadError SomeError m)
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
    c <- createTagConnections draftId pool tagsList
    l <- loadMainImage c mainImage pool
    loadImages l imagesList pool

newDraft ::
       (MonadIO m, MonadError SomeError m)
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
            rows <- queryWithPoolNew pool q draftUpd
            if Prelude.null rows
                then throwError $ OtherError "Draft not created"
                else return $ fromOnly $ Prelude.head rows) $ \e ->
        case someErrorToInt e of
            23502 -> throwError BadToken
            _     -> throwError e

getTagsIds ::
       (MonadIO m, MonadError SomeError m)
    => Pool Connection
    -> BC.ByteString
    -> m [Int]
getTagsIds pool tagsBs = do
    let n = Prelude.length $ BC.split ' ' tagsBs
    let l = BC.split ' ' tagsBs
    let q = toQuery $ BC.concat ["select tag_id from tags where tag_name in ?"]
    rows <- queryWithPoolNew pool q (Only (In l))
    if Prelude.length rows < n
        then throwError $ OtherError "Someone tags not exist"
        else do
            return $ fromOnly <$> rows

createTagConnections ::
       (MonadIO m, MonadError SomeError m)
    => Int
    -> Pool Connection
    -> DraftTags
    -> m Int
createTagConnections draftId pool tagsList = do
    tagIds <- getTagsIds pool (getDraftTags tagsList)
    let q = toQuery "insert into draft_tags (draft_id,tag_id) values (?,?)"
    let a = Prelude.map (draftId, ) tagIds
    let nt = Prelude.length tagIds
    n <- executeManyWithPoolNew pool q a
    if fromIntegral n < nt
        then throwError $ OtherError "Some tags not added"
        else return draftId

loadMainImage ::
       (MonadIO m, MonadError SomeError m)
    => Int
    -> Maybe Image
    -> Pool Connection
    -> m Int
loadMainImage draftId Nothing _ = return draftId
loadMainImage draftId (Just image) pool = do
    let q =
            toQuery $
            "with m_id as (insert into images (image_name,content_type, image_b) values (?,?,?) returning image_id) " <>
            "update drafts set main_image = (select * from m_id) where draft_id = " <>
            BC.pack (show draftId)
    n <- executeWithPoolNew pool q image
    if n < 1
        then do
            throwError $ OtherError "Image not loaded"
        else return draftId

loadImages ::
       (MonadIO m, MonadError SomeError m)
    => Int
    -> Maybe [Image]
    -> Pool Connection
    -> m SendId
loadImages draftId Nothing _ = return draftId
loadImages draftId (Just images) pool = do
    let q =
            toQuery $
            "with m_id as (insert into images (image_name,content_type, image_b) values (?,?,?) returning image_id), " <>
            "d_i as (select " <>
            BC.pack (show draftId) <>
            " as draft_id, image_id from m_id) " <>
            "insert into drafts_images (draft_id,image_id) select * from d_i"
    n <- executeManyWithPoolNew pool q images
    if fromIntegral n < Prelude.length images
        then throwError $ OtherError "Images not loaded"
        else return draftId

--------------------------------------------------------------------------------------------------------------------------
updateDraftInDb ::
       (MonadIO m, MonadError SomeError m)
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
updateDraftInDb pool tokenLifeTime draftUpd@(DraftInf (Just _) (Just _) (Just _) (Just _)) (Just tagsList) mainImage imagesList draftId = do
    u <- updateDraft
    dt <- deleteTagConnections u
    ct <- createTagConnections dt pool tagsList
    dmi <- deleteMainImage ct mainImage
    l <- loadMainImage dmi mainImage pool
    di <- deleteOldImages l imagesList
    _ <- loadImages di imagesList pool
    return ()
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
        n <- executeWithPoolNew pool q draftUpd
        if n < 1
            then do
                throwError $ OtherError "Draft not updated"
            else return (getId draftId)
    deleteTagConnections mess = do
        let q =
                toQuery $
                "delete from draft_tags where draft_id = " <>
                BC.pack (show $ getId draftId)
        n <- execute_WithPoolNew pool q
        if n < 1
            then throwError $ OtherError "Draft tags not updated"
            else return mess
    deleteMainImage mess Nothing = return mess
    deleteMainImage mess (Just _) = do
        let q =
                toQuery $
                "with m_id as (select main_image from drafts where draft_id = " <>
                BC.pack (show $ getId draftId) <>
                ") " <>
                "delete from images where image_id = (select * from m_id)"
        _ <- execute_WithPoolNew pool q
        return mess
    deleteOldImages mess Nothing = return mess
    deleteOldImages mess (Just _) = do
        let q =
                toQuery $
                "delete from images where image_id in (select image_id from drafts_images where draft_id = " <>
                BC.pack (show $ getId draftId) <> ")"
        _ <- execute_WithPoolNew pool q
        return mess
