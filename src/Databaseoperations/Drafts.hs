{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Databaseoperations.Drafts where

import Control.Exception (catch)
import Data.ByteString as B (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
    ( Connection, Only(..), SqlError(sqlState), In(In) )
import HelpFunction (readByteStringToInt, toQuery)
import Logger (Handle, logDebug, logError, logInfo)
import PostgreSqlWithPool
    ( executeManyWithPool
    , executeWithPool
    , execute_WithPool
    , queryWithPool
    )
import Types
    ( Draft
    , DraftArray(DraftArray)
    , Image
    , TokenLifeTime
    , TokenProfile(TokenProfile)
    )

{-checkAuthor :: Handle -> Maybe T.Text -> IO (Either LBS.ByteString Int)
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
        return $ Left "Database error"-}
checkAuthor ::
       Handle
    -> Pool Connection
    -> TokenLifeTime
    -> Maybe T.Text
    -> IO (Either LBS.ByteString Int)
checkAuthor hLogger _ _ Nothing = do
    logError hLogger "No token parameter"
    return $ Left "No token parameter"
checkAuthor hLogger pool token_lifetime (Just token') =
    catch
        (do rows <- queryWithPool pool q [token']
            if Prelude.null rows
                then do
                    return $ Left "You are not author"
                else return $ Right (fromOnly $ Prelude.head rows)) $ \e -> do
        --let err = E.decodeUtf8 $ sqlErrorMsg e
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ T.concat ["Database error ", T.pack $ show errStateInt]
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
        --let err = E.decodeUtf8 $ sqlErrorMsg e
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ T.concat ["Database error ", T.pack $ show errStateInt]
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
    -> Pool Connection
    -> TokenLifeTime
    -> Maybe T.Text
    -> Maybe ByteString
    -> IO (Either LBS.ByteString LBS.ByteString)
deleteDraftFromDb hLogger _ _ _ Nothing = do
    logError hLogger "No draft_id parameter"
    return $ Left "No draft_id parameter"
deleteDraftFromDb hLogger pool token_lifetime token' (Just draft_id) =
    catch
        (do ch <- checkAuthor hLogger pool token_lifetime token'
            case ch of
                Left bs -> return $ Left bs
                Right author_id -> do
                    let q =
                            "delete from drafts where draft_id = ? and author_id = ?"
                    let dr_id = fromMaybe (-1) $ readByteStringToInt draft_id
                    n <- executeWithPool pool q (dr_id, author_id)
                    if n > 0
                        then do
                            logInfo hLogger $
                                T.concat
                                    ["Draft ", T.pack $ show dr_id, " deleted"]
                            return $ Right "Draft deleted"
                        else do
                            logError
                                hLogger
                                "Deleting not existing draft or bad draft_id parameter"
                            return $
                                Left "Draft not exist or bad draft_id parameter") $ \e -> do
        --let err = E.decodeUtf8 $ sqlErrorMsg e
        let errState = sqlState e
        let errStateInt = fromMaybe 0 (readByteStringToInt errState)
        logError hLogger $ T.concat ["Database error ", T.pack $ show errStateInt]
        return $ Left "Database error"
    {-catch
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
        return $ Left "Database error"-}

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
    ch_author <- checkAuthor hLogger pool token_lifetime token'
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
                --let err = E.decodeUtf8 $ sqlErrorMsg e
                let errState = sqlState e
                let errStateInt = fromMaybe 0 (readByteStringToInt errState)
                logError hLogger $
                    T.concat ["Database error ", T.pack $ show errStateInt]
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
            --let err = E.decodeUtf8 $ sqlErrorMsg e
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ T.concat ["Database error ", T.pack $ show errStateInt]
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
            --let err = E.decodeUtf8 $ sqlErrorMsg e
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ T.concat ["Database error ", T.pack $ show errStateInt]
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
            --let err = E.decodeUtf8 $ sqlErrorMsg e
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ T.concat ["Database error ", T.pack $ show errStateInt]
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
            --let err = E.decodeUtf8 $ sqlErrorMsg e
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ T.concat ["Database error ", T.pack $ show errStateInt]
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
            --let err = E.decodeUtf8 $ sqlErrorMsg e
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ T.concat ["Database error ", T.pack $ show errStateInt]
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
            --let err = E.decodeUtf8 $ sqlErrorMsg e
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ T.concat ["Database error ", T.pack $ show errStateInt]
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
            --let err = E.decodeUtf8 $ sqlErrorMsg e
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ T.concat ["Database error ", T.pack $ show errStateInt]
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
            --let err = E.decodeUtf8 $ sqlErrorMsg e
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ T.concat ["Database error ", T.pack $ show errStateInt]
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
            --let err = E.decodeUtf8 $ sqlErrorMsg e
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ T.concat ["Database error ", T.pack $ show errStateInt]
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
            --let err = E.decodeUtf8 $ sqlErrorMsg e
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ T.concat ["Database error ", T.pack $ show errStateInt]
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
            --let err = E.decodeUtf8 $ sqlErrorMsg e
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ T.concat ["Database error ", T.pack $ show errStateInt]
            return $ Left "Database error"

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

publicNewsOnDb ::
       Handle
    -> Pool Connection
    -> TokenLifeTime
    -> Maybe T.Text
    -> Int
    -> IO (Either LBS.ByteString Int)
publicNewsOnDb hLogger pool token_lifetime token' draft_id = do
    ch <- checkAuthor hLogger pool token_lifetime token'
    case ch of
        Left bs -> return $ Left bs
        Right a_id -> do
            n <- insertNews a_id
            t <- insertTags n
            insertImages t
  where
    insertNews author'_id =
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
            --let err = E.decodeUtf8 $ sqlErrorMsg e
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ T.concat ["Database error ", T.pack $ show errStateInt]
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
            --let err = E.decodeUtf8 $ sqlErrorMsg e
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ T.concat ["Database error ", T.pack $ show errStateInt]
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
            --let err = E.decodeUtf8 $ sqlErrorMsg e
            let errState = sqlState e
            let errStateInt = fromMaybe 0 (readByteStringToInt errState)
            logError hLogger $ T.concat ["Database error ", T.pack $ show errStateInt]
            return $ Left "Database error"
