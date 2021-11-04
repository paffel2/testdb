{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Databaseoperations.Drafts where

import           Control.Exception          (catch)
import qualified Data.ByteString.Char8      as BC
import           Data.Maybe                 (fromMaybe)
import           Data.Pool                  (Pool)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as E
import           Database.PostgreSQL.Simple (Connection, In (In), Only (..),
                                             SqlError (sqlErrorMsg, sqlState))

import           HelpFunction               (readByteStringToInt, toQuery)
import           Logger                     (LoggerHandle, logDebug, logError,
                                             logInfo)
import           PostgreSqlWithPool         (executeManyWithPool,
                                             executeWithPool, execute_WithPool,
                                             queryWithPool)
import           Types.Drafts               (Draft, DraftArray (DraftArray),
                                             DraftInf (..),
                                             DraftTags (getDraftTags))
import           Types.Images               (Image)
import           Types.Other                (Id (getId), SendId,
                                             SomeError (BadToken, DatabaseError, OtherError),
                                             Token, TokenLifeTime)
import           Types.Users                (TokenProfile (TokenProfile))

checkAuthor ::
       LoggerHandle IO
    -> Pool Connection
    -> TokenLifeTime
    -> Maybe Token
    -> IO (Either SomeError SendId)
checkAuthor hLogger _ _ Nothing = do
    logError hLogger "No token parameter"
    return . Left . OtherError $ "No token parameter"
checkAuthor hLogger pool tokenLifeTime (Just token) =
    catch
        (do rows <- queryWithPool pool q [token]
            if Prelude.null rows
                then do
                    return $ Left BadToken
                else return $ Right (fromOnly $ Prelude.head rows)) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ "Database error " <> T.pack (show errStateInt)
        return $ Left DatabaseError
  where
    q =
        toQuery $
        "select author_id from authors join users using (user_id) join tokens using (user_id) where token = ? \
             \and (now()- tokens.creation_date) < make_interval(secs => " <>
        BC.pack (show tokenLifeTime) <> ")"

getDraftsByAuthorToken ::
       Pool Connection
    -> TokenLifeTime
    -> LoggerHandle IO
    -> Maybe Token
    -> IO (Either SomeError DraftArray)
getDraftsByAuthorToken _ _ hLogger Nothing = do
    logError hLogger "No token parameter"
    return . Left . OtherError $ "No token parameter"
getDraftsByAuthorToken pool tokenLifeTime hLogger (Just token) =
    catch
        (do rows <- queryWithPool pool q (TokenProfile token tokenLifeTime)
            if Prelude.null rows
                then do
                    logError hLogger "User use bad token or haven't drafts"
                    return . Left . OtherError $
                        "You are not author or don't have drafts "
                else return $ Right $ DraftArray rows) $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ "Database error " <> T.pack (show errStateInt)
        return $ Left DatabaseError
  where
    q =
        toQuery
            "select short_title, date_of_changes, category_id, draft_text, main_image, array_agg(image_id) \
             \from drafts join drafts_images using (draft_id) join authors using (author_id) join tokens using (user_id) \
             \where token = ? and (now() - tokens.creation_date) < make_interval(secs => ?) group by draft_id"

deleteDraftFromDb ::
       Pool Connection
    -> TokenLifeTime
    -> LoggerHandle IO
    -> Maybe Token
    -> Maybe Id
    -> IO (Either SomeError ())
deleteDraftFromDb _ _ hLogger _ Nothing = do
    logError hLogger "Draft not deleted. No draft_id parameter"
    return . Left . OtherError $ "Draft not deleted. No draft_id parameter"
deleteDraftFromDb pool tokenLifeTime hLogger token (Just draftId) =
    catch
        (do ch <- checkAuthor hLogger pool tokenLifeTime token
            case ch of
                Left bs -> return $ Left bs
                Right authorId -> do
                    let q =
                            "delete from drafts where draft_id = ? and author_id = ?"
                    n <- executeWithPool pool q (draftId, authorId)
                    if n > 0
                        then do
                            logInfo hLogger "Draft deleted"
                            return $ Right ()
                        else do
                            logError
                                hLogger
                                "Deleting not existing draft or bad draft_id parameter"
                            return . Left . OtherError $
                                "Draft not exist or bad draft_id parameter") $ \e -> do
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ "Database error " <> T.pack (show errStateInt)
        return $ Left DatabaseError

getDraftByIdFromDb ::
       Pool Connection
    -> TokenLifeTime
    -> LoggerHandle IO
    -> Maybe Token
    -> Id
    -> IO (Either SomeError Draft)
getDraftByIdFromDb _ _ hLogger Nothing _ = do
    logError hLogger "Draft not sended. No token parameter"
    return . Left . OtherError $ "Draft not sended. No token parameter"
getDraftByIdFromDb pool tokenLifeTime hLogger token draftId = do
    chAuthor <- checkAuthor hLogger pool tokenLifeTime token
    case chAuthor of
        Left bs -> return $ Left bs
        Right authorId ->
            catch
                (do rows <- queryWithPool pool q [authorId]
                    if Prelude.null rows
                        then do
                            logError hLogger "Wrong draft id or draft not exist"
                            return . Left . OtherError $
                                "Wrong draft id or draft not exist"
                        else do
                            logInfo hLogger "Draft sended"
                            return $ Right $ Prelude.head rows) $ \e -> do
                let errState = sqlState e
                let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                logError hLogger $
                    "Database error " <> T.pack (show errStateInt)
                return $ Left DatabaseError
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

createDraftOnDb ::
       Pool Connection
    -> TokenLifeTime
    -> LoggerHandle IO
    -> DraftInf
    -> Maybe DraftTags
    -> Maybe Image
    -> Maybe [Image]
    -> IO (Either SomeError SendId)
createDraftOnDb _ _ hLogger DraftInf {draftInfToken = Nothing} _ _ _ = do
    logError hLogger "No token param"
    return . Left . OtherError $ "No token param"
createDraftOnDb _ _ hLogger DraftInf {draftInfCategory = Nothing} _ _ _ = do
    logError hLogger "No category field"
    return . Left . OtherError $ "No category field"
createDraftOnDb _ _ hLogger _ Nothing _ _ = do
    logError hLogger "No tags field"
    return . Left . OtherError $ "No tags field"
createDraftOnDb _ _ hLogger DraftInf {draftInfTitle = Nothing} _ _ _ = do
    logError hLogger "No short_title field"
    return . Left . OtherError $ "No short_title field"
createDraftOnDb _ _ hLogger DraftInf {draftInfText = Nothing} _ _ _ = do
    logError hLogger "No text field"
    return . Left . OtherError $ "No text field"
createDraftOnDb pool tokenLifeTime hLogger draftUpd@(DraftInf (Just _) (Just _) (Just _) (Just _)) (Just tagsList) mainImage imagesList = do
    logInfo hLogger "Someone try add new draft"
    draftId <- newDraft
    c <- createTagConnections draftId
    l <- loadMainImage c mainImage
    loadImages l imagesList
  where
    newDraft =
        catch
            (do let q =
                        toQuery $
                        "with get_a as (select author_id from authors join tokens using (user_id) where token = ?" <>
                        " and (now() - tokens.creation_date) < make_interval(secs => " <>
                        BC.pack (show tokenLifeTime) <>
                        ")), get_c as (select category_id from categories where category_name = ?) " <>
                        "insert into drafts (author_id,short_title,date_of_changes,category_id,draft_text) values ((select author_id from get_a)," <>
                        " ? ,now(),(select category_id from get_c)," <>
                        " ?) returning draft_id"
                logDebug hLogger "Insert draft info"
                rows <- queryWithPool pool q draftUpd :: IO [Only Int]
                if Prelude.null rows
                    then do
                        logError hLogger "Draft not created"
                        return . Left . OtherError $ "Draft not created"
                    else do
                        logDebug hLogger "Draft info added"
                        return $ Right $ fromOnly $ Prelude.head rows) $ \e -> do
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            case errStateInt of
                23502 -> do
                    logError hLogger "Bad token"
                    return $ Left BadToken
                _ -> do
                    let err = E.decodeUtf8 $ sqlErrorMsg e
                    logError hLogger err
                    return $ Left DatabaseError
    createTagConnections (Left someError) = return $ Left someError
    createTagConnections (Right draftId) =
        catch
            (do tagIds <- getTagsIds hLogger pool (getDraftTags tagsList)
                case tagIds of
                    Left bs -> return $ Left bs
                    Right ns -> do
                        let q =
                                toQuery
                                    "insert into draft_tags (draft_id,tag_id) values (?,?)"
                        let a = Prelude.map (draftId, ) ns
                        let nt = Prelude.length ns
                        logDebug hLogger "Add tag connections"
                        n <- executeManyWithPool pool q a
                        if fromIntegral n < nt
                            then do
                                logError hLogger "Some tags not added"
                                return . Left . OtherError $
                                    "Some tags not added"
                            else do
                                logDebug hLogger "Connections added"
                                return $ Right draftId) $ \e -> do
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ "Database error " <> T.pack (show errStateInt)
            return $ Left DatabaseError
    loadMainImage (Left someError) _ = return $ Left someError
    loadMainImage (Right draftId) Nothing = return $ Right draftId
    loadMainImage (Right draftId) (Just image) =
        catch
            (do let q =
                        toQuery $
                        "with m_id as (insert into images (image_name,content_type, image_b) values (?,?,?) returning image_id) " <>
                        "update drafts set main_image = (select * from m_id) where draft_id = " <>
                        BC.pack (show draftId)
                logDebug hLogger "Load main image"
                n <- executeWithPool pool q image
                if n < 1
                    then do
                        logError hLogger "Image not loaded"
                        return . Left . OtherError $ "Image not loaded"
                    else do
                        logDebug hLogger "Main image loaded"
                        return $ Right draftId) $ \e -> do
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ "Database error " <> T.pack (show errStateInt)
            return $ Left DatabaseError
    loadImages (Left someError) _ = return $ Left someError
    loadImages (Right draftId) Nothing = return $ Right draftId
    loadImages (Right draftId) (Just images) =
        catch
            (do let q =
                        toQuery $
                        "with m_id as (insert into images (image_name,content_type, image_b) values (?,?,?) returning image_id), " <>
                        "d_i as (select " <>
                        BC.pack (show draftId) <>
                        " as draft_id, image_id from m_id) " <>
                        "insert into drafts_images (draft_id,image_id) select * from d_i"
                logDebug hLogger "Load other images"
                n <- executeManyWithPool pool q images
                if fromIntegral n < Prelude.length images
                    then do
                        logError hLogger "Images not loaded"
                        return . Left . OtherError $ "Images not loaded"
                    else do
                        return $ Right draftId) $ \e -> do
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ "Database error " <> T.pack (show errStateInt)
            return $ Left DatabaseError

updateDraftInDb ::
       Pool Connection
    -> TokenLifeTime
    -> LoggerHandle IO
    -> DraftInf
    -> Maybe DraftTags
    -> Maybe Image
    -> Maybe [Image]
    -> Id
    -> IO (Either SomeError ())
updateDraftInDb _ _ hLogger DraftInf {draftInfToken = Nothing} _ _ _ _ = do
    logError hLogger "No token param"
    return . Left . OtherError $ "No token param"
updateDraftInDb _ _ hLogger DraftInf {draftInfCategory = Nothing} _ _ _ _ = do
    logError hLogger "No category field"
    return . Left . OtherError $ "No category field"
updateDraftInDb _ _ hLogger _ Nothing _ _ _ = do
    logError hLogger "No tags field"
    return . Left . OtherError $ "No tags field"
updateDraftInDb _ _ hLogger DraftInf {draftInfTitle = Nothing} _ _ _ _ = do
    logError hLogger "No short_title field"
    return . Left . OtherError $ "No short_title field"
updateDraftInDb _ _ hLogger DraftInf {draftInfText = Nothing} _ _ _ _ = do
    logError hLogger "No text field"
    return . Left . OtherError $ "No text field"
updateDraftInDb pool tokenLifeTime hLogger draftUpd@(DraftInf (Just _) (Just _) (Just _) (Just _)) (Just tagsList) mainImage imagesList draftId = do
    logInfo hLogger "Someone try update draft"
    u <- updateDraft
    dt <- deleteTagConnections u
    ct <- createTagConnections dt
    dmi <- deleteMainImage ct mainImage
    l <- loadMainImage dmi mainImage
    di <- deleteOldImages l imagesList
    loadImages di imagesList
  where
    updateDraft =
        catch
            (do logDebug hLogger "Update drafts"
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
                        return . Left . OtherError $ "Draft not updated"
                    else do
                        logDebug hLogger "Update drafts complete"
                        return $ Right ()) $ \e -> do
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ "Database error " <> T.pack (show errStateInt)
            return $ Left DatabaseError
    deleteTagConnections (Left someError) = return $ Left someError
    deleteTagConnections (Right mess) =
        catch
            (do logDebug hLogger "Deleting old tags"
                let q =
                        toQuery $
                        "delete from draft_tags where draft_id = " <>
                        BC.pack (show $ getId draftId)
                n <- execute_WithPool pool q
                if n < 1
                    then do
                        logError hLogger "Draft tags not updated"
                        return . Left . OtherError $ "Draft tags not updated"
                    else do
                        logDebug hLogger "Deleting old tags complete"
                        return $ Right mess) $ \e -> do
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ "Database error " <> T.pack (show errStateInt)
            return $ Left DatabaseError
    createTagConnections (Left someError) = return $ Left someError
    createTagConnections (Right mess) =
        catch
            (do tagIds <- getTagsIds hLogger pool (getDraftTags tagsList)
                case tagIds of
                    Left bs -> return $ Left bs
                    Right ns -> do
                        logDebug hLogger "Add new tags"
                        let q =
                                toQuery
                                    "insert into draft_tags (draft_id,tag_id) values (?,?)"
                        let a = Prelude.map (draftId, ) ns
                        let nt = Prelude.length ns
                        n <- executeManyWithPool pool q a
                        if fromIntegral n < nt
                            then do
                                logError hLogger "Some tags not added"
                                return . Left . OtherError $
                                    "Some tags not added"
                            else do
                                logDebug hLogger "Add new tags complete"
                                return $ Right mess) $ \e -> do
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ "Database error " <> T.pack (show errStateInt)
            return $ Left DatabaseError
    deleteMainImage (Left someError) _ = return $ Left someError
    deleteMainImage (Right mess) Nothing = return $ Right mess
    deleteMainImage (Right mess) (Just _) =
        catch
            (do logDebug hLogger "Deleting old main image"
                let q =
                        toQuery $
                        "with m_id as (select main_image from drafts where draft_id = " <>
                        BC.pack (show $ getId draftId) <>
                        ") " <>
                        "delete from images where image_id = (select * from m_id)"
                _ <- execute_WithPool pool q
                logDebug hLogger "Deleting old main image complete"
                return $ Right mess) $ \e -> do
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ "Database error " <> T.pack (show errStateInt)
            return $ Left DatabaseError
    loadMainImage (Left someError) _ = return $ Left someError
    loadMainImage (Right mess) Nothing = return $ Right mess
    loadMainImage (Right mess) (Just image) =
        catch
            (do logDebug hLogger "Add new main image"
                let q =
                        toQuery $
                        "with m_id as (insert into images (image_name,content_type, image_b) values (?,?,?) returning image_id) \
                             \update drafts set main_image = (select * from m_id) where draft_id = " <>
                        BC.pack (show $ getId draftId)
                n <- executeWithPool pool q image
                if n < 1
                    then do
                        logError hLogger "Image not loaded"
                        return . Left . OtherError $ "Image not loaded"
                    else do
                        logDebug hLogger "Add new main image"
                        return $ Right mess) $ \e -> do
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ "Database error " <> T.pack (show errStateInt)
            return $ Left DatabaseError
    deleteOldImages (Left someError) _ = return $ Left someError
    deleteOldImages (Right mess) Nothing = return $ Right mess
    deleteOldImages (Right mess) (Just _) =
        catch
            (do logDebug hLogger "Deleting old images"
                let q =
                        toQuery $
                        "delete from images where image_id in (select image_id from drafts_images where draft_id = " <>
                        BC.pack (show $ getId draftId) <> ")"
                _ <- execute_WithPool pool q
                logDebug hLogger "Deleting old images complete"
                return $ Right mess) $ \e -> do
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ "Database error " <> T.pack (show errStateInt)
            return $ Left DatabaseError
    loadImages (Left someError) _ = return $ Left someError
    loadImages (Right mess) Nothing = return $ Right mess
    loadImages (Right mess) (Just images) =
        catch
            (do logDebug hLogger "Add new images"
                let q =
                        toQuery $
                        "with m_id as (insert into images (image_name,content_type, image_b) values (?,?,?) returning image_id), \
                                    \d_i as (select " <>
                        BC.pack (show $ getId draftId) <>
                        " as draft_id, image_id from m_id) \
                         \insert into drafts_images (draft_id,image_id) select * from d_i"
                n <- executeManyWithPool pool q images
                if fromIntegral n < Prelude.length images
                    then do
                        logError hLogger "Images not loaded"
                        return . Left . OtherError $ "Images not loaded"
                    else do
                        logDebug hLogger "Add new images complete"
                        return $ Right mess) $ \e -> do
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ "Database error " <> T.pack (show errStateInt)
            return $ Left DatabaseError

getTagsIds ::
       LoggerHandle IO
    -> Pool Connection
    -> BC.ByteString
    -> IO (Either SomeError [Int])
getTagsIds hLogger pool tagsBs = do
    let n = Prelude.length $ BC.split ' ' tagsBs
    let l = BC.split ' ' tagsBs
    let q = toQuery $ BC.concat ["select tag_id from tags where tag_name in ?"]
    rows <- queryWithPool pool q (Only (In l))
    if Prelude.length rows < n
        then do
            logError hLogger "Someone tags not exist"
            return . Left . OtherError $ "Someone tags not exist"
        else do
            return $ Right $ fromOnly <$> rows

publicNewsOnDb ::
       Pool Connection
    -> TokenLifeTime
    -> LoggerHandle IO
    -> Maybe Token
    -> Id
    -> IO (Either SomeError SendId)
publicNewsOnDb pool tokenLifeTime hLogger token draftId = do
    ch <- checkAuthor hLogger pool tokenLifeTime token
    case ch of
        Left bs -> return $ Left bs
        Right authorId -> do
            n <- insertNews authorId
            t <- insertTags n
            insertImages t
  where
    insertNews authorId =
        catch
            (do logDebug hLogger "Adding news to db"
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
                    then do
                        logError hLogger "News not published"
                        return . Left . OtherError $ "News not published"
                    else do
                        logDebug hLogger "Adding news to db complete"
                        return $ Right $ fromOnly $ Prelude.head rows) $ \e -> do
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ "Database error " <> T.pack (show errStateInt)
            return $ Left DatabaseError
    insertTags (Left someError) = return $ Left someError
    insertTags (Right newsId) =
        catch
            (do logDebug hLogger "Adding news tags to db"
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
                logDebug hLogger "Adding news tags to db complete"
                return $ Right newsId) $ \e -> do
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ "Database error " <> T.pack (show errStateInt)
            return $ Left DatabaseError
    insertImages (Left someError) = return $ Left someError
    insertImages (Right newsId) =
        catch
            (do logDebug hLogger "Adding news images to db"
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
                logDebug hLogger "Adding news images to db complete"
                return $ Right newsId) $ \e -> do
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ "Database error " <> T.pack (show errStateInt)
            return $ Left DatabaseError
