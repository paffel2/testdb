{-# LANGUAGE OverloadedStrings #-}

module ImagesTests where

import           Control.Monad.IO.Class
import           Data.Functor.Identity
import           Database.PostgreSQL.Simple
import           Logger
import           Network.HTTP.Types
import           Network.HTTP.Types.Method
import           Network.Wai
import           OperationsHandle           (AuthorsHandle (..),
                                             CategoriesHandle (..),
                                             DraftsHandle (..),
                                             ImagesHandle (..),
                                             NewsAndCommentsHandle (..),
                                             OperationsHandle (..),
                                             TagsHandle (..), UsersHandle (..))
import           Router
import           Test.Hspec
import           Types.Images
import           Types.Other

instance MonadIO Identity where
    liftIO = liftIO

hLogger :: LoggerHandle Identity
hLogger =
    LoggerHandle {priority = Debug, Logger.log = \prior message -> return ()}

authorsHandler :: AuthorsHandle Identity
authorsHandler =
    AuthorsHandle
        { create_author_in_db =
              \token create_author -> return $ Left $ OtherError "ErrorMessage"
        , delete_author_in_db =
              \token author_login -> return $ Left $ OtherError "ErrorMessage"
        , get_authors_list = \page -> return $ Left $ OtherError "ErrorMessage"
        , edit_author_in_db =
              \token edit_author -> return $ Left $ OtherError "ErrorMessage"
        , authors_logger = hLogger
        }

categoriesHandler :: CategoriesHandle Identity
categoriesHandler =
    CategoriesHandle
        { get_categories_list_from_db =
              \page -> return $ Left $ OtherError "ErrorMessage"
        , create_category_on_db =
              \token create_category ->
                  return $ Left $ OtherError "ErrorMessage"
        , delete_category_from_db =
              \token category_name -> return $ Left $ OtherError "ErrorMessage"
        , edit_category_on_db =
              \token edit_category -> return $ Left $ OtherError "ErrorMessage"
        , categories_logger = hLogger
        }

draftsHandler :: DraftsHandle Identity
draftsHandler =
    DraftsHandle
        { get_drafts_by_author_token =
              \token -> return $ Left $ OtherError "ErrorMessage"
        , delete_draft_from_db =
              \token id -> return $ Left $ OtherError "ErrorMessage"
        , get_draft_by_id_from_db =
              \token id -> return $ Left $ OtherError "ErrorMessage"
        , create_draft_on_db =
              \draft_information draft_tags draft_main_image draft_other_images ->
                  return $ Left $ OtherError "ErrorMessage"
        , update_draft_in_db =
              \draft_information draft_tags draft_main_image draft_other_images draft_id ->
                  return $ Left $ OtherError "ErrorMessage"
        , public_news_on_db =
              \token draft_id -> return $ Left $ OtherError "ErrorMessage"
        , drafts_logger = hLogger
        }

imagesHandler :: ImagesHandle Identity
imagesHandler =
    ImagesHandle
        { get_photo = \photo_id -> return $ Left $ OtherError "ErrorMessage"
        , get_photo_list = \page -> return $ Left $ OtherError "ErrorMessage"
        , photos_logger = hLogger
        }

newsAndCommentsHandler :: NewsAndCommentsHandle Identity
newsAndCommentsHandler =
    NewsAndCommentsHandle
        { add_comment_to_db =
              \comment_information -> return $ Left $ OtherError "ErrorMessage"
        , delete_comment_from_db =
              \token comment_id -> return $ Left $ OtherError "ErrorMessage"
        , get_comments_by_news_id_from_db =
              \news_id page -> return $ Left $ OtherError "ErrorMessage"
        , get_news_by_id_from_db =
              \news_id -> return $ Left $ OtherError "ErrorMessage"
        , get_news_filter_by_tag_in_from_db =
              \tag_in_filter_param page ->
                  return $ Left $ OtherError "ErrorMessage"
        , get_news_filter_by_category_id_from_db =
              \category_id_filter_param page sort ->
                  return $ Left $ OtherError "ErrorMessage"
        , get_news_filter_by_title_from_db =
              \title_filter_param page sort ->
                  return $ Left $ OtherError "ErrorMessage"
        , get_news_filter_by_author_name_from_db =
              \author_filter_param page sort ->
                  return $ Left $ OtherError "ErrorMessage"
        , get_news_filter_by_date_from_db =
              \date_filter_param page sort ->
                  return $ Left $ OtherError "ErrorMessage"
        , get_news_filter_by_tag_all_from_db =
              \tag_all_filter_param page sort ->
                  return $ Left $ OtherError "ErrorMessage"
        , get_news_filter_by_content_from_db =
              \content_filter_param page sort ->
                  return $ Left $ OtherError "ErrorMessage"
        , get_news_filter_by_after_date_from_db =
              \after_date_filter_param page sort ->
                  return $ Left $ OtherError "ErrorMessage"
        , get_news_filter_by_before_date_from_db =
              \before_date_filter_param page sort ->
                  return $ Left $ OtherError "ErrorMessage"
        , get_news_filter_by_tag_id_from_db =
              \tag_filter_param page sort ->
                  return $ Left $ OtherError "ErrorMessage"
        , get_news_from_db =
              \sort page -> return $ Left $ OtherError "ErrorMessage"
        , news_logger = hLogger
        }

tagsHandler :: TagsHandle Identity
tagsHandler =
    TagsHandle
        { create_tag_in_db =
              \token tag_name -> return $ Left $ OtherError "ErrorMessage"
        , delete_tag_from_db =
              \token tag_name -> return $ Left $ OtherError "ErrorMessage"
        , get_tags_list_from_db =
              \page -> return $ Left $ OtherError "ErrorMessage"
        , edit_tag_in_db =
              \token edit_tag -> return $ Left $ OtherError "ErrorMessage"
        , tags_logger = hLogger
        }

usersHandler :: UsersHandle Identity
usersHandler =
    UsersHandle
        { auth = \login password -> return $ Left $ OtherError "ErrorMessage"
        , create_user_in_db =
              \create_user -> return $ Left $ OtherError "ErrorMessage"
        , delete_user_from_db =
              \token login -> return $ Left $ OtherError "ErrorMessage"
        , profile_on_db = \token -> return $ Left $ OtherError "ErrorMessage"
        , users_logger = hLogger
        }

operationsHandler :: OperationsHandle Identity
operationsHandler =
    OperationsHandle
        { authors_handle = authorsHandler
        , categories_handle = categoriesHandler
        , drafts_handle = draftsHandler
        , images_handle = imagesHandler
        , news_and_comments_handle = newsAndCommentsHandler
        , tags_handle = tagsHandler
        , users_handle = usersHandler
        }

testRespond :: Response -> Identity Status
testRespond response = return $ responseStatus response

tstGetPhotoListReq :: Request
tstGetPhotoListReq =
    defaultRequest {rawPathInfo = "/image", requestMethod = methodGet}

tstGetPhotoReq :: Request
tstGetPhotoReq =
    defaultRequest {rawPathInfo = "/image/1", requestMethod = methodGet}

imagesTests' :: IO ()
imagesTests' =
    hspec $ do
        describe "testing images functions" $ do
            describe "testing get_photo_list" $ do
                it "server should return error 400 because something happend" $
                    routes operationsHandler tstGetPhotoListReq testRespond `shouldBe`
                    return status400
                it
                    "server should return status 405, because request sended with bad requestMethod" $
                    routes
                        operationsHandler
                        (tstGetPhotoListReq {requestMethod = methodPut})
                        testRespond `shouldBe`
                    return status405
                it "server should return 404, because path is wrong" $
                    routes
                        operationsHandler
                        (tstGetPhotoListReq {rawPathInfo = "/imagewertyuio"})
                        testRespond `shouldBe`
                    return status404
                it "server should return 200, because all is good" $
                    routes
                        (operationsHandler
                             { images_handle =
                                   imagesHandler
                                       { get_photo_list =
                                             \_ ->
                                                 return $ Right (ImageArray [])
                                       }
                             })
                        (tstGetPhotoListReq {rawPathInfo = "/imagewertyuio"})
                        testRespond `shouldBe`
                    return status404
            describe "testing get_photo" $ do
                it "server should return error 400 because something happend" $
                    routes operationsHandler tstGetPhotoReq testRespond `shouldBe`
                    return status400
                it
                    "server should return status 405, because request sended with bad requestMethod" $
                    routes
                        operationsHandler
                        (tstGetPhotoReq {requestMethod = methodPut})
                        testRespond `shouldBe`
                    return status405
                it "server should return 400, because image id is wrong" $
                    routes
                        operationsHandler
                        (tstGetPhotoReq {rawPathInfo = "/image/wertyuio"})
                        testRespond `shouldBe`
                    return status400
                it "server should return 200, because all is good" $
                    routes
                        (operationsHandler
                             { images_handle =
                                   imagesHandler
                                       { get_photo =
                                             \_ ->
                                                 return $
                                                 Right (ImageB (Binary "") "")
                                       }
                             })
                        (tstGetPhotoReq {rawPathInfo = "/imagewertyuio"})
                        testRespond `shouldBe`
                    return status404
