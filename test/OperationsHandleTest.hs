{-# LANGUAGE OverloadedStrings #-}

module OperationsHandleTest where

import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as LBS
import           Data.Functor.Identity      (Identity)
import           Data.Pool                  (Pool, createPool)
import           Data.Time                  (Day)
import           Data.Time.Clock            (UTCTime (utctDay))
import           Database.PostgreSQL.Simple (Binary (Binary), Connection, close,
                                             connectPostgreSQL)
import           Logger                     (LoggerHandle (..),
                                             Priority (Debug))
import           Network.HTTP.Types         (methodGet, methodPost)
import           Network.Wai                as W (Application)
import           OperationsHandle           (AuthorsHandle (..),
                                             CategoriesHandle (..),
                                             DraftsHandle (..),
                                             ImagesHandle (..),
                                             NewsAndCommentsHandle (..),
                                             OperationsHandle (..),
                                             TagsHandle (..), UsersHandle (..))
import           Router                     (routes)
import           Test.Hspec                 (Spec, describe, hspec, it)
import           Test.Hspec.Wai             (ResponseMatcher (matchBody, matchHeaders, matchStatus),
                                             delete, get, post, put, request,
                                             shouldRespondWith, with, (<:>))
import           Types.Authors              (AuthorsList (AuthorsList),
                                             ElemAuthorsList (ElemAuthorsList))
import           Types.Categories
import           Types.Drafts
import           Types.Images
import           Types.NewsAndComments
import           Types.Other
import           Types.Tags
import           Types.Users

hLogger :: LoggerHandle IO
hLogger =
    LoggerHandle {priority = Debug, Logger.log = \prior message -> return ()}

authorsHandler :: AuthorsHandle IO
authorsHandler =
    AuthorsHandle
        { create_author_in_db =
              \hLogger token_life_time token create_author ->
                  return $ Left "ErrorMessage"
        , delete_author_in_db =
              \hLogger token_life_time token author_login ->
                  return $ Left "ErrorMessage"
        , get_authors_list = \hLogger page -> return $ Left "ErrorMessage"
        , edit_author_in_db =
              \hLogger token_life_time token edit_author ->
                  return $ Left "ErrorMessage"
        }

categoriesHandler :: CategoriesHandle IO
categoriesHandler =
    CategoriesHandle
        { get_categories_list_from_db =
              \hLogger page -> return $ Left "ErrorMessage"
        , create_category_on_db =
              \hLogger token_life_time token create_category ->
                  return $ Left "ErrorMessage"
        , delete_category_from_db =
              \hLogger token_life_time token category_name ->
                  return $ Left "ErrorMessage"
        , edit_category_on_db =
              \hLogger token_life_time token edit_category ->
                  return $ Left "ErrorMessage"
        }

draftsHandler :: DraftsHandle IO
draftsHandler =
    DraftsHandle
        { get_drafts_by_author_token =
              \hLogger token_life_time token -> return $ Left "ErrorMessage"
        , delete_draft_from_db =
              \hLogger token_life_time token id -> return $ Left "ErrorMessage"
        , get_draft_by_id_from_db =
              \hLogger token_life_time token id -> return $ Left "ErrorMessage"
        , create_draft_on_db =
              \hLogger token_life_time draft_information draft_tags draft_main_image draft_other_images ->
                  return $ Left "ErrorMessage"
        , update_draft_in_db =
              \hLogger token_life_time draft_information draft_tags draft_main_image draft_other_images draft_id ->
                  return $ Left "ErrorMessage"
        , public_news_on_db =
              \hLogger token_life_time token draft_id ->
                  return $ Left "ErrorMessage"
        }

imagesHandler :: ImagesHandle IO
imagesHandler =
    ImagesHandle
        { get_photo = \hLogger photo_id -> return $ Left "ErrorMessage"
        , get_photo_list = \hLogger page -> return $ Left "ErrorMessage"
        }

newsAndCommentsHandler :: NewsAndCommentsHandle IO
newsAndCommentsHandler =
    NewsAndCommentsHandle
        { add_comment_to_db =
              \hLogger comment_information -> return $ Left "ErrorMessage"
        , delete_comment_from_db =
              \hLogger token_life_time token comment_id ->
                  return $ Left "ErrorMessage"
        , get_comments_by_news_id_from_db =
              \hLogger news_id page -> return $ Left "ErrorMessage"
        , get_news_by_id_from_db =
              \hLogger news_id -> return $ Left "ErrorMessage"
        , get_news_filter_by_tag_in_from_db =
              \hLogger tag_in_filter_param page -> return $ Left "ErrorMessage"
        , get_news_filter_by_category_id_from_db =
              \hLogger category_id_filter_param page sort ->
                  return $ Left "ErrorMessage"
        , get_news_filter_by_title_from_db =
              \hLogger title_filter_param page sort ->
                  return $ Left "ErrorMessage"
        , get_news_filter_by_author_name_from_db =
              \hLogger author_filter_param page sort ->
                  return $ Left "ErrorMessage"
        , get_news_filter_by_date_from_db =
              \hLogger date_filter_param page sort ->
                  return $ Left "ErrorMessage"
        , get_news_filter_by_tag_all_from_db =
              \hLogger tag_all_filter_param page sort ->
                  return $ Left "ErrorMessage"
        , get_news_filter_by_content_from_db =
              \hLogger content_filter_param page sort ->
                  return $ Left "ErrorMessage"
        , get_news_filter_by_after_date_from_db =
              \hLogger after_date_filter_param page sort ->
                  return $ Left "ErrorMessage"
        , get_news_filter_by_before_date_from_db =
              \hLogger before_date_filter_param page sort ->
                  return $ Left "ErrorMessage"
        , get_news_filter_by_tag_id_from_db =
              \hLogger tag_filter_param page sort ->
                  return $ Left "ErrorMessage"
        , get_news_from_db = \hLogger sort page -> return $ Left "ErrorMessage"
        }

tagsHandler :: TagsHandle IO
tagsHandler =
    TagsHandle
        { create_tag_in_db =
              \hLogger token_life_time token tag_name ->
                  return $ Left "ErrorMessage"
        , delete_tag_from_db =
              \hLogger token_life_time token tag_name ->
                  return $ Left "ErrorMessage"
        , get_tags_list_from_db = \pool page -> return $ Left "ErrorMessage"
        , edit_tag_in_db =
              \hLogger token_life_time token edit_tag ->
                  return $ Left "ErrorMessage"
        }

usersHandler :: UsersHandle IO
usersHandler =
    UsersHandle
        { auth = \hLogger login password -> return $ Left "ErrorMessage"
        , create_user_in_db =
              \hLogger create_user -> return $ Left "ErrorMessage"
        , delete_user_from_db =
              \hLogger token_life_time token login ->
                  return $ Left "ErrorMessage"
        , profile_on_db =
              \hLogger token_life_time token -> return $ Left "ErrorMessage"
        }

operationsHandler :: OperationsHandle IO
operationsHandler =
    OperationsHandle
        { authors_handle = authorsHandler
        , categories_handle = categoriesHandler
        , drafts_handle = draftsHandler
        , images_handle = imagesHandler
        , news_and_comments_handle = newsAndCommentsHandler
        , tags_handle = tagsHandler
        , users_handle = usersHandler
        , logger_handle = hLogger
        }

tstPool :: IO (Pool Connection)
tstPool = do
    createPool (connectPostgreSQL "") close 10 10 10

imageTests :: Spec
imageTests = do
    with (toIOAp operationsHandler) $ do
        describe "test for images functions" $ do
            describe "get_photo" $ do
                it "server return error message about wrong method" $ do
                    delete "/image/1" `shouldRespondWith`
                        "Bad method request" {matchStatus = 405}
    with
        (toIOAp
             (operationsHandler
                  { images_handle =
                        imagesHandler
                            { get_photo =
                                  \hLogger photo_id ->
                                      return $
                                      Right (ImageB (Binary "image") "con_type")
                            }
                  })) $ do
        it "  server return image" $ do
            get "/image/1" `shouldRespondWith`
                200
                    { matchHeaders = ["Content-Type" <:> "con_type"]
                    , matchBody = "image"
                    }
    with
        (toIOAp
             (operationsHandler
                  { images_handle =
                        imagesHandler
                            { get_photo =
                                  \hLogger photo_id ->
                                      return $
                                      Right (ImageB (Binary "image") "con_type")
                            }
                  })) $ do
        it "  server return error message, because get bad image_id" $ do
            get "/image/a" `shouldRespondWith`
                "Bad image id" {matchStatus = 400}
    with (toIOAp operationsHandler) $ do
        describe "get_photo_list" $ do
            it "server return error message about bad request method" $ do
                delete "/image" `shouldRespondWith`
                    "Bad method request" {matchStatus = 405}
    with
        (toIOAp
             operationsHandler
                 { images_handle =
                       imagesHandler
                           { get_photo_list =
                                 \hLogger page ->
                                     return $
                                     Right
                                         (ImageArray
                                              [ElemImageArray 1 "image_name"])
                           }
                 }) $ do
        it "  server return json-object with list of images" $ do
            get "/image" `shouldRespondWith`
                200
                    { matchHeaders = ["Content-Type" <:> "application/json"]
                    , matchBody =
                          "{\"images\":[{\"image_name\":\"image_name\",\"image_id\":1}]}"
                    }
    with (toIOAp operationsHandler) $ do
        it "  the server will send a message about error" $ do
            get "/image" `shouldRespondWith`
                "Images list not sended" {matchStatus = 400}

usersTests :: Spec
usersTests = do
    with (toIOAp operationsHandler) $ do
        describe "test for users functions" $ do
            describe "auth" $ do
                it "server return error message about wrong method" $ do
                    delete "/login" `shouldRespondWith`
                        "Bad method request" {matchStatus = 405}
    with
        (toIOAp
             operationsHandler
                 { users_handle =
                       usersHandler
                           { auth =
                                 \hLogger login password ->
                                     return $ Right $ Token "token"
                           }
                 }) $ do
        it "  server return token after succsessful authentication" $ do
            request methodGet "/login" [] "" `shouldRespondWith`
                "token" {matchStatus = 200}
    with (toIOAp operationsHandler) $ do
        it "  server return error message" $ do
            get "/login" `shouldRespondWith`
                "Bad authorization" {matchStatus = 400}
    with (toIOAp operationsHandler) $ do
        describe " create_user_in_db" $ do
            it "server return error message about wrong method" $ do
                delete "/registration" `shouldRespondWith`
                    "Bad method request" {matchStatus = 405}
    with
        (toIOAp
             operationsHandler
                 { users_handle =
                       usersHandler
                           { create_user_in_db =
                                 \hLogger create_user ->
                                     return $ Right $ Token "token"
                           }
                 }) $ do
        it "  server return token after succsessful registration" $ do
            request methodPost "/registration" [] "" `shouldRespondWith`
                "token" {matchStatus = 201}
    with (toIOAp operationsHandler) $ do
        it "  server return error message" $ do
            request methodPost "/registration" [] "" `shouldRespondWith`
                "User not registered." {matchStatus = 400}
    with (toIOAp operationsHandler) $ do
        describe " delete_user_from_db" $ do
            it "server return error message about wrong method" $ do
                get "/deleteUser" `shouldRespondWith`
                    "Bad method request" {matchStatus = 405}
    with
        (toIOAp
             operationsHandler
                 { users_handle =
                       usersHandler
                           { delete_user_from_db =
                                 \hLogger token_lifetime token login ->
                                     return $ Right ()
                           }
                 }) $ do
        it "  server return message about succsessful deleting" $ do
            delete "/deleteUser?token=123&&login=abc789" `shouldRespondWith`
                "User deleted" {matchStatus = 200}
    with
        (toIOAp
             operationsHandler
                 { users_handle =
                       usersHandler
                           { delete_user_from_db =
                                 \hLogger token_lifetime token login ->
                                     return $ Left "Bad token"
                           }
                 }) $ do
        it "  server return message about bad token" $ do
            delete "/deleteUser?token=123&&login=abc789" `shouldRespondWith`
                "Bad token" {matchStatus = 403}
    with
        (toIOAp
             operationsHandler
                 { users_handle =
                       usersHandler
                           { delete_user_from_db =
                                 \hLogger token_lifetime token login ->
                                     return $ Left "Not admin"
                           }
                 }) $ do
        it "  server return message about not admin token" $ do
            delete "/deleteUser?token=123&&login=abc789" `shouldRespondWith`
                "Not admin" {matchStatus = 403}
    with (toIOAp operationsHandler) $ do
        it "  server return error message" $ do
            delete "/deleteUser?token=123&&login=abc789" `shouldRespondWith`
                "User not deleted" {matchStatus = 400}
    with (toIOAp operationsHandler) $ do
        describe " profile_on_db" $ do
            it "server return error message about wrong method" $ do
                delete "/profile?token=qwerty1" `shouldRespondWith`
                    "Bad method request" {matchStatus = 405}
    with
        (toIOAp
             operationsHandler
                 { users_handle =
                       usersHandler
                           { profile_on_db =
                                 \hLogger token_lifetime token ->
                                     return $
                                     Right
                                         (Profile
                                              (Just "f_name")
                                              (Just "l_name")
                                              (Just 1))
                           }
                 }) $ do
        it "  server return profile information" $ do
            get "/profile?token=qwerty1" `shouldRespondWith`
                "{\"profile_last_name\":\"l_name\",\"profile_avatar\":1,\"profile_first_name\":\"f_name\"}"
                    {matchStatus = 200}
    with (toIOAp operationsHandler) $ do
        it "  server return error message" $ do
            get "/profile?token=qwerty1" `shouldRespondWith`
                "Information not sended." {matchStatus = 400}

authorsTests :: Spec
authorsTests = do
    with (toIOAp operationsHandler) $ do
        describe "test for authors functions" $ do
            describe "create_author_in_db" $ do
                it "server return error message about wrong method" $ do
                    delete "/authors/create_author?token=qwerty1" `shouldRespondWith`
                        "Bad request method" {matchStatus = 405}
    with
        (toIOAp
             operationsHandler
                 { authors_handle =
                       authorsHandler
                           { create_author_in_db =
                                 \hLogger token_lifetime token create_author ->
                                     return (Right 1)
                           }
                 }) $ do
        it "    server return new author_id" $ do
            post "/authors/create_author?token=qwerty1" "" `shouldRespondWith`
                "1" {matchStatus = 201}
    with (toIOAp operationsHandler) $ do
        it "    server return error message " $ do
            post "/authors/create_author?token=qwerty1" "" `shouldRespondWith`
                "Author not created." {matchStatus = 400}
    with
        (toIOAp
             operationsHandler
                 { authors_handle =
                       authorsHandler
                           { create_author_in_db =
                                 \hLogger token_lifetime token create_author ->
                                     return (Left "Not admin")
                           }
                 }) $ do
        it "    server error message about not admin token" $ do
            post "/authors/create_author?token=qwerty1" "" `shouldRespondWith`
                "Author not created. Not admin." {matchStatus = 403}
    with
        (toIOAp
             operationsHandler
                 { authors_handle =
                       authorsHandler
                           { create_author_in_db =
                                 \hLogger token_lifetime token create_author ->
                                     return (Left "Bad token")
                           }
                 }) $ do
        it "    server error message about bad token" $ do
            post "/authors/create_author?token=qwerty1" "" `shouldRespondWith`
                "Author not created. Bad token." {matchStatus = 403}
    with (toIOAp operationsHandler) $ do
        describe "delete_author_in_db" $ do
            it "server return error message about wrong method" $ do
                get "/authors/create_author?token=qwerty1" `shouldRespondWith`
                    "Bad request method" {matchStatus = 405}
    with
        (toIOAp
             operationsHandler
                 { authors_handle =
                       authorsHandler
                           { delete_author_in_db =
                                 \hLogger token_lifetime token delete_author ->
                                     return (Right ())
                           }
                 }) $ do
        it "  server return new author_id" $ do
            delete "/authors/delete_author?token=qwerty1" `shouldRespondWith`
                "Author deleted." {matchStatus = 201}
    with (toIOAp operationsHandler) $ do
        it "  server return error message " $ do
            delete "/authors/delete_author?token=qwerty1" `shouldRespondWith`
                "Author not deleted." {matchStatus = 400}
    with
        (toIOAp
             operationsHandler
                 { authors_handle =
                       authorsHandler
                           { delete_author_in_db =
                                 \hLogger token_lifetime token delete_author ->
                                     return (Left "Not admin")
                           }
                 }) $ do
        it "  server error message about not admin token" $ do
            delete "/authors/delete_author?token=qwerty1" `shouldRespondWith`
                "Author not deleted. Not admin." {matchStatus = 403}
    with
        (toIOAp
             operationsHandler
                 { authors_handle =
                       authorsHandler
                           { delete_author_in_db =
                                 \hLogger token_lifetime token delete_author ->
                                     return (Left "Bad token")
                           }
                 }) $ do
        it "  server error message about bad token" $ do
            delete "/authors/delete_author?token=qwerty1" `shouldRespondWith`
                "Author not deleted. Bad token." {matchStatus = 403}
    with (toIOAp operationsHandler) $ do
        describe "get_authors_list" $ do
            it "server return error message about wrong method" $ do
                delete "/authors" `shouldRespondWith`
                    "Bad request method" {matchStatus = 405}
    with
        (toIOAp
             operationsHandler
                 { authors_handle =
                       authorsHandler
                           { get_authors_list =
                                 \hLogger page ->
                                     return $
                                     Right
                                         (AuthorsList
                                              [ ElemAuthorsList
                                                    1
                                                    "name"
                                                    (Just "description")
                                              ])
                           }
                 }) $ do
        it "  server return list of authors" $ do
            get "/authors" `shouldRespondWith`
                "{\"authors\":[{\"author_id\":1,\"authors_description\":\"description\",\"author_name'\":\"name\"}]}"
                    {matchStatus = 200}
    with (toIOAp operationsHandler) $ do
        it "  server return error message " $ do
            get "/authors" `shouldRespondWith`
                "List of authors not sended." {matchStatus = 400}
    with (toIOAp operationsHandler) $ do
        describe "edit_author_in_db" $ do
            it "server return error message about wrong method" $ do
                delete "/authors/edit_author?token=qwerty1" `shouldRespondWith`
                    "Bad request method" {matchStatus = 405}
    with
        (toIOAp
             operationsHandler
                 { authors_handle =
                       authorsHandler
                           { edit_author_in_db =
                                 \hLogger token_lifetime token edit_author ->
                                     return (Right ())
                           }
                 }) $ do
        it "  server return message about succsessful editing" $ do
            put "/authors/edit_author?token=qwerty1" "" `shouldRespondWith`
                "Author edited." {matchStatus = 201}
    with (toIOAp operationsHandler) $ do
        it "  server return error message " $ do
            put "/authors/edit_author?token=qwerty1" "" `shouldRespondWith`
                "Author not edited." {matchStatus = 400}
    with
        (toIOAp
             operationsHandler
                 { authors_handle =
                       authorsHandler
                           { edit_author_in_db =
                                 \hLogger token_lifetime token edit_author ->
                                     return (Left "Not admin")
                           }
                 }) $ do
        it "  server error message about not admin token" $ do
            put "/authors/edit_author?token=qwerty1" "" `shouldRespondWith`
                "Not admin" {matchStatus = 403}
    with
        (toIOAp
             operationsHandler
                 { authors_handle =
                       authorsHandler
                           { edit_author_in_db =
                                 \hLogger token_lifetime token edit_author ->
                                     return (Left "Bad token")
                           }
                 }) $ do
        it "  server error message about bad token" $ do
            put "/authors/edit_author?token=qwerty1" "" `shouldRespondWith`
                "Bad token" {matchStatus = 403}

categoriesTests :: Spec
categoriesTests = do
    with (toIOAp operationsHandler) $ do
        describe "test for categories functions" $ do
            describe "create_category_on_db" $ do
                it "server return error message about wrong method" $ do
                    delete "/categories/create_category?token=qwerty1" `shouldRespondWith`
                        "Bad method request" {matchStatus = 405}
    with
        (toIOAp
             operationsHandler
                 { categories_handle =
                       categoriesHandler
                           { create_category_on_db =
                                 \hLogger token_lifetime token create_category ->
                                     return (Right 1)
                           }
                 }) $ do
        it "    server return message about successful creating " $ do
            post "/categories/create_category?token=qwerty1" "" `shouldRespondWith`
                "1" {matchStatus = 201}
    with (toIOAp operationsHandler) $ do
        it "    server return error message " $ do
            post "/categories/create_category?token=qwerty1" "" `shouldRespondWith`
                "Category not created." {matchStatus = 400}
    with
        (toIOAp
             operationsHandler
                 { categories_handle =
                       categoriesHandler
                           { create_category_on_db =
                                 \hLogger token_lifetime token create_category ->
                                     return (Left "Not admin")
                           }
                 }) $ do
        it "    server error message about not admin token" $ do
            post "/categories/create_category?token=qwerty1" "" `shouldRespondWith`
                "Category not created. Not admin." {matchStatus = 403}
    with
        (toIOAp
             operationsHandler
                 { categories_handle =
                       categoriesHandler
                           { create_category_on_db =
                                 \hLogger token_lifetime token create_category ->
                                     return (Left "Bad token")
                           }
                 }) $ do
        it "    server error message about bad token" $ do
            post "/categories/create_category?token=qwerty1" "" `shouldRespondWith`
                "Category not created. Bad token." {matchStatus = 403}
    with (toIOAp operationsHandler) $ do
        describe "delete_category_on_db" $ do
            it "server return error message about wrong method" $ do
                get "/categories/delete_category?token=qwerty1" `shouldRespondWith`
                    "Bad method request" {matchStatus = 405}
    with
        (toIOAp
             operationsHandler
                 { categories_handle =
                       categoriesHandler
                           { delete_category_from_db =
                                 \hLogger token_lifetime token category_name ->
                                     return (Right ())
                           }
                 }) $ do
        it "  server return message about successful deleting " $ do
            delete "/categories/delete_category?token=qwerty1" `shouldRespondWith`
                "Category deleted." {matchStatus = 200}
    with (toIOAp operationsHandler) $ do
        it "  server return error message " $ do
            delete "/categories/delete_category?token=qwerty1" `shouldRespondWith`
                "Category not deleted." {matchStatus = 400}
    with
        (toIOAp
             operationsHandler
                 { categories_handle =
                       categoriesHandler
                           { delete_category_from_db =
                                 \hLogger token_lifetime token category_name ->
                                     return (Left "Not admin")
                           }
                 }) $ do
        it "  server error message about not admin token" $ do
            delete "/categories/delete_category?token=qwerty1" `shouldRespondWith`
                "Not admin" {matchStatus = 403}
    with
        (toIOAp
             operationsHandler
                 { categories_handle =
                       categoriesHandler
                           { delete_category_from_db =
                                 \hLogger token_lifetime token category_name ->
                                     return (Left "Bad token")
                           }
                 }) $ do
        it "  server error message about bad token" $ do
            delete "/categories/delete_category?token=qwerty1" `shouldRespondWith`
                "Bad token" {matchStatus = 403}
    with (toIOAp operationsHandler) $ do
        describe "edit_category_on_db" $ do
            it "server return error message about wrong method" $ do
                get "/categories/edit_category?token=qwerty1" `shouldRespondWith`
                    "Bad method request" {matchStatus = 405}
    with
        (toIOAp
             operationsHandler
                 { categories_handle =
                       categoriesHandler
                           { edit_category_on_db =
                                 \hLogger token_lifetime token edit_category ->
                                     return (Right ())
                           }
                 }) $ do
        it "  server return message about successful editing " $ do
            put "/categories/edit_category?token=qwerty1" "" `shouldRespondWith`
                "Category edited." {matchStatus = 201}
    with (toIOAp operationsHandler) $ do
        it "  server return error message " $ do
            put "/categories/edit_category?token=qwerty1" "" `shouldRespondWith`
                "Category not edited." {matchStatus = 400}
    with
        (toIOAp
             operationsHandler
                 { categories_handle =
                       categoriesHandler
                           { edit_category_on_db =
                                 \hLogger token_lifetime token edit_category ->
                                     return (Left "Not admin")
                           }
                 }) $ do
        it "  server error message about not admin token" $ do
            put "/categories/edit_category?token=qwerty1" "" `shouldRespondWith`
                "Category not edited. Not admin." {matchStatus = 403}
    with
        (toIOAp
             operationsHandler
                 { categories_handle =
                       categoriesHandler
                           { edit_category_on_db =
                                 \hLogger token_lifetime token edit_category ->
                                     return (Left "Bad token")
                           }
                 }) $ do
        it "  server error message about bad token" $ do
            put "/categories/edit_category?token=qwerty1" "" `shouldRespondWith`
                "Category not edited. Bad token." {matchStatus = 403}
    with (toIOAp operationsHandler) $ do
        describe "get_categories_list_from_db" $ do
            it "server return error message about wrong method" $ do
                delete "/categories" `shouldRespondWith`
                    "Bad method request" {matchStatus = 405}
    with
        (toIOAp
             operationsHandler
                 { categories_handle =
                       categoriesHandler
                           { get_categories_list_from_db =
                                 \hLogger page ->
                                     return
                                         (Right $
                                          ListOfCategories
                                              [ElemOfCategoryList "sport"])
                           }
                 }) $ do
        it "  server return list of categories" $ do
            get "/categories" `shouldRespondWith`
                "{\"list_of_categories\":[{\"category_get_name\":\"sport\"}]}"
                    {matchStatus = 200}
    with (toIOAp operationsHandler) $ do
        it "  server return error message" $ do
            get "/categories" `shouldRespondWith`
                "List of categories not sended." {matchStatus = 400}

tagsTests :: Spec
tagsTests = do
    with (toIOAp operationsHandler) $ do
        describe "test for tags functions" $ do
            describe "create_tag_in_db" $ do
                it "server return error message about wrong method" $ do
                    delete "/tags/create_tag?token=qwerty1" `shouldRespondWith`
                        "Bad method request" {matchStatus = 405}
    with
        (toIOAp
             operationsHandler
                 { tags_handle =
                       tagsHandler
                           { create_tag_in_db =
                                 \hLogger token_lifetime token tag_name ->
                                     return (Right 1)
                           }
                 }) $ do
        it "    server return id of new tag " $ do
            post "/tags/create_tag?token=qwerty1" "" `shouldRespondWith`
                "1" {matchStatus = 201}
    with (toIOAp operationsHandler) $ do
        it "    server return error message " $ do
            post "/tags/create_tag?token=qwerty1" "" `shouldRespondWith`
                "Tag not created." {matchStatus = 400}
    with
        (toIOAp
             operationsHandler
                 { tags_handle =
                       tagsHandler
                           { create_tag_in_db =
                                 \hLogger token_lifetime token create_category ->
                                     return (Left "Not admin")
                           }
                 }) $ do
        it "    server error message about not admin token" $ do
            post "/tags/create_tag?token=qwerty1" "" `shouldRespondWith`
                "Not admin" {matchStatus = 403}
    with
        (toIOAp
             operationsHandler
                 { tags_handle =
                       tagsHandler
                           { create_tag_in_db =
                                 \hLogger token_lifetime token create_category ->
                                     return (Left "Bad token")
                           }
                 }) $ do
        it "    server error message about bad token" $ do
            post "/tags/create_tag?token=qwerty1" "" `shouldRespondWith`
                "Bad token" {matchStatus = 403}
    with (toIOAp operationsHandler) $ do
        describe "delete_tag_from_db" $ do
            it "server return error message about wrong method" $ do
                get "/tags/delete_tag?token=qwerty1" `shouldRespondWith`
                    "Bad method request" {matchStatus = 405}
    with
        (toIOAp
             operationsHandler
                 { tags_handle =
                       tagsHandler
                           { delete_tag_from_db =
                                 \hLogger token_lifetime token tag_name ->
                                     return (Right ())
                           }
                 }) $ do
        it "  server return message about successful deleting " $ do
            delete "/tags/delete_tag?token=qwerty1" `shouldRespondWith`
                "Tag deleted." {matchStatus = 200}
    with (toIOAp operationsHandler) $ do
        it "  server return error message " $ do
            delete "/tags/delete_tag?token=qwerty1" `shouldRespondWith`
                "Tag not deleted." {matchStatus = 400}
    with
        (toIOAp
             operationsHandler
                 { tags_handle =
                       tagsHandler
                           { delete_tag_from_db =
                                 \hLogger token_lifetime token tag_name ->
                                     return (Left "Not admin")
                           }
                 }) $ do
        it "  server error message about not admin token" $ do
            delete "/tags/delete_tag?token=qwerty1" `shouldRespondWith`
                "Not admin" {matchStatus = 403}
    with
        (toIOAp
             operationsHandler
                 { tags_handle =
                       tagsHandler
                           { delete_tag_from_db =
                                 \hLogger token_lifetime token tag_name ->
                                     return (Left "Bad token")
                           }
                 }) $ do
        it "  server error message about bad token" $ do
            delete "/tags/delete_tag?token=qwerty1" `shouldRespondWith`
                "Bad token" {matchStatus = 403}
    with (toIOAp operationsHandler) $ do
        describe "edit_tag" $ do
            it "server return error message about wrong method" $ do
                get "/tags/edit_tag?token=qwerty1" `shouldRespondWith`
                    "Bad method request" {matchStatus = 405}
    with
        (toIOAp
             operationsHandler
                 { tags_handle =
                       tagsHandler
                           { edit_tag_in_db =
                                 \hLogger token_lifetime token edit_tag ->
                                     return (Right ())
                           }
                 }) $ do
        it "  server return message about successful editing " $ do
            put "/tags/edit_tag?token=qwerty1" "" `shouldRespondWith`
                "Tag edited." {matchStatus = 200}
    with (toIOAp operationsHandler) $ do
        it "  server return error message " $ do
            put "/tags/edit_tag?token=qwerty1" "" `shouldRespondWith`
                "Tag not edited." {matchStatus = 400}
    with
        (toIOAp
             operationsHandler
                 { tags_handle =
                       tagsHandler
                           { edit_tag_in_db =
                                 \hLogger token_lifetime token edit_tag ->
                                     return (Left "Not admin")
                           }
                 }) $ do
        it "  server error message about not admin token" $ do
            put "/tags/edit_tag?token=qwerty1" "" `shouldRespondWith`
                "Not admin" {matchStatus = 403}
    with
        (toIOAp
             operationsHandler
                 { tags_handle =
                       tagsHandler
                           { edit_tag_in_db =
                                 \hLogger token_lifetime token edit_tag ->
                                     return (Left "Bad token")
                           }
                 }) $ do
        it "  server error message about bad token" $ do
            put "/tags/edit_tag?token=qwerty1" "" `shouldRespondWith`
                "Bad token" {matchStatus = 403}
    with (toIOAp operationsHandler) $ do
        describe "get_tags_list_from_db" $ do
            it "server return error message about wrong method" $ do
                delete "/tags" `shouldRespondWith`
                    "Bad method request" {matchStatus = 405}
    with
        (toIOAp
             operationsHandler
                 { tags_handle =
                       tagsHandler
                           { get_tags_list_from_db =
                                 \hLogger page ->
                                     return (Right $ TagsList [Tag "sport"])
                           }
                 }) $ do
        it "  server return list of tags" $ do
            get "/tags" `shouldRespondWith`
                "{\"tags\":[{\"tag_name\":\"sport\"}]}" {matchStatus = 200}
    with (toIOAp operationsHandler) $ do
        it "  server return error message" $ do
            get "/tags" `shouldRespondWith`
                "Tags list not sended" {matchStatus = 400}

draftsTests :: Spec
draftsTests = do
    let time = read "2021-11-19 18:28:52.607875 UTC" :: UTCTime
    with (toIOAp operationsHandler) $ do
        describe "test for drafts functions" $ do
            describe "get_drafts_by_author_token" $ do
                it "server return error message about wrong method" $ do
                    delete "/drafts?token=qwerty1" `shouldRespondWith`
                        "Bad method request" {matchStatus = 405}
    with
        (toIOAp
             operationsHandler
                 { drafts_handle =
                       draftsHandler
                           { get_drafts_by_author_token =
                                 \hLogger token_lifetime token ->
                                     return
                                         (Right $
                                          DraftArray
                                              [ DraftGet
                                                    "title"
                                                    time
                                                    (Just 1)
                                                    (Just "draft_text")
                                                    (Just 1)
                                                    Nothing
                                              ])
                           }
                 }) $ do
        it "    server list of drafts " $ do
            get "/drafts?token=qwerty1" `shouldRespondWith`
                "{\"drafts\":[{\"date_of_changes'\":\"2021-11-19T18:28:52.607875Z\",\"draft_text'\":\"draft_text\",\"draft_short_title'\":\"title\",\"draft_images\":null,\"draft_main_image_id'\":1,\"draft_category_id'\":1}]}"
                    {matchStatus = 200}
    with (toIOAp operationsHandler) $ do
        it "    server return error message " $ do
            get "/drafts?token=qwerty1" `shouldRespondWith`
                "Draft not created." {matchStatus = 400}
    with (toIOAp operationsHandler) $ do
        describe "delete_draft_from_db" $ do
            it "server return error message about wrong method" $ do
                get "/drafts/delete_draft?token=qwerty1" `shouldRespondWith`
                    "Bad method request" {matchStatus = 405}
    with
        (toIOAp
             operationsHandler
                 { drafts_handle =
                       draftsHandler
                           { delete_draft_from_db =
                                 \hLogger token_lifetime token draft_id ->
                                     return (Right ())
                           }
                 }) $ do
        it "  server return message about successful deleting " $ do
            delete "/drafts/delete_draft?token=qwerty1" `shouldRespondWith`
                "Draft deleted." {matchStatus = 200}
    with (toIOAp operationsHandler) $ do
        it "  server return error message " $ do
            delete "/drafts/delete_draft?token=qwerty1" `shouldRespondWith`
                "Draft not deleted." {matchStatus = 400}
    with
        (toIOAp
             operationsHandler
                 { drafts_handle =
                       draftsHandler
                           { delete_draft_from_db =
                                 \hLogger token_lifetime token tag_name ->
                                     return (Left "Bad token")
                           }
                 }) $ do
        it "  server error message about bad token" $ do
            delete "/drafts/delete_draft?token=qwerty1" `shouldRespondWith`
                "Bad token" {matchStatus = 403}
    with (toIOAp operationsHandler) $ do
        describe "get_draft_by_id_from_db" $ do
            it "server return error message about wrong method" $ do
                delete "/drafts/1?token=qwerty1" `shouldRespondWith`
                    "Bad method request" {matchStatus = 405}
    with
        (toIOAp
             operationsHandler
                 { drafts_handle =
                       draftsHandler
                           { get_draft_by_id_from_db =
                                 \hLogger token_lifetime token draft_id ->
                                     return
                                         (Right $
                                          Draft
                                              "title"
                                              time
                                              (Just 1)
                                              (Just "text")
                                              (Just 1)
                                              Nothing
                                              Nothing)
                           }
                 }) $ do
        it "  server send draft information " $ do
            get "/drafts/1?token=qwerty1" `shouldRespondWith`
                "{\"draft_text''\":\"text\",\"draft_short_title''\":\"title\",\"draft_category_id''\":1,\"draft_tags'\":null,\"date_of_changes''\":\"2021-11-19T18:28:52.607875Z\",\"draft_main_image_id''\":1,\"draft_images'\":null}"
                    {matchStatus = 200}
    with (toIOAp operationsHandler) $ do
        it "  server return error message " $ do
            get "/drafts/1?token=qwerty1" `shouldRespondWith`
                "Draft not sended." {matchStatus = 400}
    with
        (toIOAp
             operationsHandler
                 { drafts_handle =
                       draftsHandler
                           { get_draft_by_id_from_db =
                                 \hLogger token_lifetime token draft_id ->
                                     return (Left "Bad token")
                           }
                 }) $ do
        it "  server error message about bad token" $ do
            get "/drafts/1?token=qwerty1" `shouldRespondWith`
                "Bad token" {matchStatus = 403}
    with (toIOAp operationsHandler) $ do
        describe "create_draft_on_db" $ do
            it "server return error message about wrong method" $ do
                delete "/new_draft?token=qwerty1" `shouldRespondWith`
                    "Bad method request" {matchStatus = 405}
    with
        (toIOAp
             operationsHandler
                 { drafts_handle =
                       draftsHandler
                           { create_draft_on_db =
                                 \hLogger token_lifetime draft_inf draft_tags main_image other_images ->
                                     return $ Right 1
                           }
                 }) $ do
        it "  server return id of new draft" $ do
            post "/new_draft?token=qwerty5" draftRequest `shouldRespondWith`
                "1" {matchStatus = 201}
    with (toIOAp operationsHandler) $ do
        it "  server return error message" $ do
            post "/new_draft?token=qwerty5" draftRequest `shouldRespondWith`
                "Draft not created." {matchStatus = 400}
    with
        (toIOAp
             operationsHandler
                 { drafts_handle =
                       draftsHandler
                           { create_draft_on_db =
                                 \hLogger token_lifetime draft_inf draft_tags main_image other_images ->
                                     return (Left "Bad token")
                           }
                 }) $ do
        it "  server error message about bad token" $ do
            post "/new_draft?token=qwerty5" draftRequest `shouldRespondWith`
                "Draft not created. Bad token" {matchStatus = 403}
    with (toIOAp operationsHandler) $ do
        it "  server return error message, because get bad image file" $ do
            post "/new_draft?token=qwerty5" draftRequestErr `shouldRespondWith`
                "Draft not created." {matchStatus = 400}
    with (toIOAp operationsHandler) $ do
        describe "update_draft_in_db" $ do
            it "server return error message about wrong method" $ do
                delete "/drafts/4/update_draft?token=qwerty1" `shouldRespondWith`
                    "Bad method request" {matchStatus = 405}
    with
        (toIOAp
             operationsHandler
                 { drafts_handle =
                       draftsHandler
                           { update_draft_in_db =
                                 \hLogger token_lifetime draft_inf draft_tags main_image other_images draft_id ->
                                     return $ Right ()
                           }
                 }) $ do
        it "  server return message about successful editing " $ do
            put "/drafts/4/update_draft?token=qwerty1" draftRequest `shouldRespondWith`
                "Draft updated" {matchStatus = 200}
    with (toIOAp operationsHandler) $ do
        it "  server return error message" $ do
            put "/drafts/4/update_draft?token=qwerty1" draftRequest `shouldRespondWith`
                "Draft not updated" {matchStatus = 400}
    with
        (toIOAp
             operationsHandler
                 { drafts_handle =
                       draftsHandler
                           { update_draft_in_db =
                                 \hLogger token_lifetime draft_inf draft_tags main_image other_images draft_id ->
                                     return (Left "Bad token")
                           }
                 }) $ do
        it "  server error message about bad token" $ do
            put "/drafts/4/update_draft?token=qwerty1" draftRequest `shouldRespondWith`
                "Bad token" {matchStatus = 403}
    with (toIOAp operationsHandler) $ do
        it "  server return error message, because get bad image file" $ do
            put "/drafts/4/update_draft?token=qwerty1" draftRequestErr `shouldRespondWith`
                "Draft not updated" {matchStatus = 400}
    with (toIOAp operationsHandler) $ do
        describe "public_news_on_db" $ do
            it "server return error message about wrong method" $ do
                delete "/drafts/4/public_news?token=qwerty1" `shouldRespondWith`
                    "Bad method request" {matchStatus = 405}
    with
        (toIOAp
             operationsHandler
                 { drafts_handle =
                       draftsHandler
                           { public_news_on_db =
                                 \hLogger token_lifetime token draft_id ->
                                     return $ Right 1
                           }
                 }) $ do
        it "  server return id of new news" $ do
            put "/drafts/4/public_news?token=qwerty1" "" `shouldRespondWith`
                "1" {matchStatus = 201}
    with (toIOAp operationsHandler) $ do
        it "  server return error message" $ do
            put "/drafts/4/public_news?token=qwerty1" "" `shouldRespondWith`
                "News not created" {matchStatus = 400}
    with
        (toIOAp
             operationsHandler
                 { drafts_handle =
                       draftsHandler
                           { public_news_on_db =
                                 \hLogger token_lifetime token draft_id ->
                                     return (Left "Bad token")
                           }
                 }) $ do
        it "  server error message about bad token" $ do
            put "/drafts/4/public_news?token=qwerty1" "" `shouldRespondWith`
                "Bad token" {matchStatus = 403}

newsTests :: Spec
newsTests = do
    let time = read "2021-11-19 18:28:52.607875 UTC" :: UTCTime
    with (toIOAp operationsHandler) $ do
        describe "test for news and comments functions" $ do
            describe "add_comment_to_db" $ do
                it "server return error message about wrong method" $ do
                    delete "/news/1/comments/add_comment?token=qwerty1" `shouldRespondWith`
                        "Bad request method" {matchStatus = 405}
    with
        (toIOAp
             (operationsHandler
                  { news_and_comments_handle =
                        newsAndCommentsHandler
                            { add_comment_to_db =
                                  \hLogger comment_inf -> return $ Right ()
                            }
                  })) $ do
        it "server return message about successful posting" $ do
            post "/news/1/comments/add_comment?token=qwerty1" "" `shouldRespondWith`
                "Commentary added" {matchStatus = 201}
    with
        (toIOAp
             (operationsHandler
                  { news_and_comments_handle =
                        newsAndCommentsHandler
                            { add_comment_to_db =
                                  \hLogger comment_inf ->
                                      return $ Left "Bad token"
                            }
                  })) $ do
        it "  server return message about bad token" $ do
            post "/news/1/comments/add_comment?token=qwerty1" "" `shouldRespondWith`
                "Bad token" {matchStatus = 403}
    with
        (toIOAp
             (operationsHandler
                  { news_and_comments_handle =
                        newsAndCommentsHandler
                            { add_comment_to_db =
                                  \hLogger comment_inf ->
                                      return $ Left "News not exist"
                            }
                  })) $ do
        it "  server return a message that the news does not exist" $ do
            post "/news/1/comments/add_comment?token=qwerty1" "" `shouldRespondWith`
                "News not exist" {matchStatus = 400}
    with (toIOAp operationsHandler) $ do
        it "  server return a error message" $ do
            post "/news/1/comments/add_comment?token=qwerty1" "" `shouldRespondWith`
                "Commentary not added." {matchStatus = 400}
    with (toIOAp operationsHandler) $ do
        describe "delete_comment_from_db" $ do
            it "server return error message about wrong method" $ do
                get "/news/1/comments/delete_comment?token=qwerty1" `shouldRespondWith`
                    "Bad request method" {matchStatus = 405}
    with
        (toIOAp
             (operationsHandler
                  { news_and_comments_handle =
                        newsAndCommentsHandler
                            { delete_comment_from_db =
                                  \hLogger token_lifetime token comment_id ->
                                      return $ Right ()
                            }
                  })) $ do
        it "  server return message about successful deleting" $ do
            delete "/news/1/comments/delete_comment?token=qwerty1" `shouldRespondWith`
                "Commentary deleted." {matchStatus = 200}
    with
        (toIOAp
             (operationsHandler
                  { news_and_comments_handle =
                        newsAndCommentsHandler
                            { delete_comment_from_db =
                                  \hLogger token_lifetime token comment_id ->
                                      return $ Left "Bad token"
                            }
                  })) $ do
        it "  server return message about bad token" $ do
            delete "/news/1/comments/delete_comment?token=qwerty1" `shouldRespondWith`
                "Bad token" {matchStatus = 403}
    with
        (toIOAp
             (operationsHandler
                  { news_and_comments_handle =
                        newsAndCommentsHandler
                            { delete_comment_from_db =
                                  \hLogger token_lifetime token comment_id ->
                                      return $ Left "Not admin"
                            }
                  })) $ do
        it "  server return a message about not admin token" $ do
            delete "/news/1/comments/delete_comment?token=qwerty1" `shouldRespondWith`
                "Not admin" {matchStatus = 403}
    with (toIOAp operationsHandler) $ do
        it "  server return a error message" $ do
            delete "/news/1/comments/delete_comment?token=qwerty1" `shouldRespondWith`
                "Commentary not deleted." {matchStatus = 400}
    with (toIOAp operationsHandler) $ do
        describe "get_comments_by_news_id_from_db" $ do
            it "server return error message about wrong method" $ do
                delete "/news/1/comments" `shouldRespondWith`
                    "Bad request method" {matchStatus = 405}
    with
        (toIOAp
             (operationsHandler
                  { news_and_comments_handle =
                        newsAndCommentsHandler
                            { get_comments_by_news_id_from_db =
                                  \hLogger news_id page ->
                                      return $
                                      Right
                                          (CommentArray
                                               [ ElemOfCommentArray
                                                     "author_name"
                                                     "comment_text"
                                                     time
                                                     1
                                               ])
                            }
                  })) $ do
        it "  server return list of comments" $ do
            get "/news/1/comments" `shouldRespondWith`
                "{\"comments\":[{\"comment_id'\":1,\"comment_time'\":\"2021-11-19T18:28:52.607875Z\",\"comment_text'\":\"comment_text\",\"comment_author_name\":\"author_name\"}]}"
                    {matchStatus = 200}
    with (toIOAp operationsHandler) $ do
        it "  server return a error message" $ do
            get "/news/1/comments" `shouldRespondWith`
                "Commentaries not sended." {matchStatus = 400}
    with (toIOAp operationsHandler) $ do
        describe "get_news_by_id_from_db" $ do
            it "server return error message about wrong method" $ do
                delete "/news/1" `shouldRespondWith`
                    "Bad request method" {matchStatus = 405}
    with
        (toIOAp
             (operationsHandler
                  { news_and_comments_handle =
                        newsAndCommentsHandler
                            { get_news_by_id_from_db =
                                  \hLogger news_id ->
                                      return $ Right (testNews (utctDay time))
                            }
                  })) $ do
        it "  server return news" $ do
            get "/news/1" `shouldRespondWith`
                "{\"news_id''\":1,\"news_main_image\":null,\"category_name'''\":\"category_name\",\"news_other_images\":null,\"author_name'\":\"author_name\",\"date_creation''\":\"2021-11-19\",\"news_text''\":\"news_text\",\"short_title''\":\"title\",\"news_tags\":null}"
                    {matchStatus = 200}
    with (toIOAp operationsHandler) $ do
        it "  server return a error message" $ do
            get "/news/1" `shouldRespondWith`
                "News not sended." {matchStatus = 400}
    with (toIOAp operationsHandler) $ do
        describe "get_news_filter_by_tag_in_from_db" $ do
            it "server return error message about wrong method" $ do
                delete "/news?tag_in=[1,2,3]" `shouldRespondWith`
                    "Bad request method" {matchStatus = 405}
    with
        (toIOAp
             (operationsHandler
                  { news_and_comments_handle =
                        newsAndCommentsHandler
                            { get_news_filter_by_tag_in_from_db =
                                  \hLogger tag_in_param page ->
                                      return $
                                      Right (testListOfNews (utctDay time))
                            }
                  })) $ do
        it "  server return news" $ do
            get "/news?tag_in=[1,2,3]" `shouldRespondWith`
                "{\"news\":[{\"category_name'\":\"category_name\",\"news_id'\":1,\"author_name\":\"author_name\",\"date_creation'\":\"2021-11-19\",\"news_text'\":\"news_text\",\"short_title'\":\"title\"}]}"
                    {matchStatus = 200}
    with (toIOAp operationsHandler) $ do
        it "  server return a error message" $ do
            get "/news?tag_in=[1,2,3]" `shouldRespondWith`
                "News not sended" {matchStatus = 400}
    with (toIOAp operationsHandler) $ do
        describe "get_news_filter_by_tag_all_from_db" $ do
            it "server return error message about wrong method" $ do
                delete "/news?tag_all=[1,2,3]" `shouldRespondWith`
                    "Bad request method" {matchStatus = 405}
    with
        (toIOAp
             (operationsHandler
                  { news_and_comments_handle =
                        newsAndCommentsHandler
                            { get_news_filter_by_tag_all_from_db =
                                  \hLogger tag_all_param page sort ->
                                      return $
                                      Right (testListOfNews (utctDay time))
                            }
                  })) $ do
        it "  server return news" $ do
            get "/news?tag_all=[1,2,3]" `shouldRespondWith`
                "{\"news\":[{\"category_name'\":\"category_name\",\"news_id'\":1,\"author_name\":\"author_name\",\"date_creation'\":\"2021-11-19\",\"news_text'\":\"news_text\",\"short_title'\":\"title\"}]}"
                    {matchStatus = 200}
    with (toIOAp operationsHandler) $ do
        it "  server return a error message" $ do
            get "/news?tag_all=[1,2,3]" `shouldRespondWith`
                "News not sended" {matchStatus = 400}
    with (toIOAp operationsHandler) $ do
        describe "get_news_filter_by_tag_id_from_db" $ do
            it "server return error message about wrong method" $ do
                delete "/news?tag=3" `shouldRespondWith`
                    "Bad request method" {matchStatus = 405}
    with
        (toIOAp
             (operationsHandler
                  { news_and_comments_handle =
                        newsAndCommentsHandler
                            { get_news_filter_by_tag_id_from_db =
                                  \hLogger tag_id_param page sort ->
                                      return $
                                      Right (testListOfNews (utctDay time))
                            }
                  })) $ do
        it "  server return news" $ do
            get "/news?tag=3" `shouldRespondWith`
                "{\"news\":[{\"category_name'\":\"category_name\",\"news_id'\":1,\"author_name\":\"author_name\",\"date_creation'\":\"2021-11-19\",\"news_text'\":\"news_text\",\"short_title'\":\"title\"}]}"
                    {matchStatus = 200}
    with (toIOAp operationsHandler) $ do
        it "  server return a error message" $ do
            get "/news?tag=3" `shouldRespondWith`
                "News not sended" {matchStatus = 400}
    with (toIOAp operationsHandler) $ do
        describe "get_news_filter_by_category_id_from_db" $ do
            it "server return error message about wrong method" $ do
                delete "/news?category=5" `shouldRespondWith`
                    "Bad request method" {matchStatus = 405}
    with
        (toIOAp
             (operationsHandler
                  { news_and_comments_handle =
                        newsAndCommentsHandler
                            { get_news_filter_by_category_id_from_db =
                                  \hLogger category_id_param page sort ->
                                      return $
                                      Right (testListOfNews (utctDay time))
                            }
                  })) $ do
        it "  server return news" $ do
            get "/news?category=5" `shouldRespondWith`
                "{\"news\":[{\"category_name'\":\"category_name\",\"news_id'\":1,\"author_name\":\"author_name\",\"date_creation'\":\"2021-11-19\",\"news_text'\":\"news_text\",\"short_title'\":\"title\"}]}"
                    {matchStatus = 200}
    with (toIOAp operationsHandler) $ do
        it "  server return a error message" $ do
            get "/news?category=5" `shouldRespondWith`
                "News not sended" {matchStatus = 400}
    with (toIOAp operationsHandler) $ do
        describe "get_news_filter_by_title_from_db" $ do
            it "server return error message about wrong method" $ do
                delete "/news?title=something" `shouldRespondWith`
                    "Bad request method" {matchStatus = 405}
    with
        (toIOAp
             (operationsHandler
                  { news_and_comments_handle =
                        newsAndCommentsHandler
                            { get_news_filter_by_title_from_db =
                                  \hLogger title_param page sort ->
                                      return $
                                      Right (testListOfNews (utctDay time))
                            }
                  })) $ do
        it "  server return news" $ do
            get "/news?title=something" `shouldRespondWith`
                "{\"news\":[{\"category_name'\":\"category_name\",\"news_id'\":1,\"author_name\":\"author_name\",\"date_creation'\":\"2021-11-19\",\"news_text'\":\"news_text\",\"short_title'\":\"title\"}]}"
                    {matchStatus = 200}
    with (toIOAp operationsHandler) $ do
        it "  server return a error message" $ do
            get "/news?title=something" `shouldRespondWith`
                "News not sended" {matchStatus = 400}
    with (toIOAp operationsHandler) $ do
        describe "get_news_filter_by_author_name_from_db" $ do
            it "server return error message about wrong method" $ do
                delete "/news?author=someone" `shouldRespondWith`
                    "Bad request method" {matchStatus = 405}
    with
        (toIOAp
             (operationsHandler
                  { news_and_comments_handle =
                        newsAndCommentsHandler
                            { get_news_filter_by_author_name_from_db =
                                  \hLogger author_param page sort ->
                                      return $
                                      Right (testListOfNews (utctDay time))
                            }
                  })) $ do
        it "  server return news" $ do
            get "/news?author=someone" `shouldRespondWith`
                "{\"news\":[{\"category_name'\":\"category_name\",\"news_id'\":1,\"author_name\":\"author_name\",\"date_creation'\":\"2021-11-19\",\"news_text'\":\"news_text\",\"short_title'\":\"title\"}]}"
                    {matchStatus = 200}
    with (toIOAp operationsHandler) $ do
        it "  server return a error message" $ do
            get "/news?author=someone" `shouldRespondWith`
                "News not sended" {matchStatus = 400}
    with (toIOAp operationsHandler) $ do
        describe "get_news_filter_by_date_from_db" $ do
            it "server return error message about wrong method" $ do
                delete "/news?date=2021-11-19" `shouldRespondWith`
                    "Bad request method" {matchStatus = 405}
    with
        (toIOAp
             (operationsHandler
                  { news_and_comments_handle =
                        newsAndCommentsHandler
                            { get_news_filter_by_date_from_db =
                                  \hLogger date_param page sort ->
                                      return $
                                      Right (testListOfNews (utctDay time))
                            }
                  })) $ do
        it "  server return news" $ do
            get "/news?date=2021-11-19" `shouldRespondWith`
                "{\"news\":[{\"category_name'\":\"category_name\",\"news_id'\":1,\"author_name\":\"author_name\",\"date_creation'\":\"2021-11-19\",\"news_text'\":\"news_text\",\"short_title'\":\"title\"}]}"
                    {matchStatus = 200}
    with (toIOAp operationsHandler) $ do
        it "  server return a error message" $ do
            get "/news?date=2021-11-19" `shouldRespondWith`
                "News not sended" {matchStatus = 400}
    with (toIOAp operationsHandler) $ do
        describe "get_news_filter_by_after_date_from_db" $ do
            it "server return error message about wrong method" $ do
                delete "/news?after_date=2020-11-19" `shouldRespondWith`
                    "Bad request method" {matchStatus = 405}
    with
        (toIOAp
             (operationsHandler
                  { news_and_comments_handle =
                        newsAndCommentsHandler
                            { get_news_filter_by_after_date_from_db =
                                  \hLogger after_date_param page sort ->
                                      return $
                                      Right (testListOfNews (utctDay time))
                            }
                  })) $ do
        it "  server return news" $ do
            get "/news?after_date=2020-11-19" `shouldRespondWith`
                "{\"news\":[{\"category_name'\":\"category_name\",\"news_id'\":1,\"author_name\":\"author_name\",\"date_creation'\":\"2021-11-19\",\"news_text'\":\"news_text\",\"short_title'\":\"title\"}]}"
                    {matchStatus = 200}
    with (toIOAp operationsHandler) $ do
        it "  server return a error message" $ do
            get "/news?after_date=2020-11-19" `shouldRespondWith`
                "News not sended" {matchStatus = 400}
    with (toIOAp operationsHandler) $ do
        describe "get_news_filter_by_before_date_from_db" $ do
            it "server return error message about wrong method" $ do
                delete "/news?before_date=2022-11-19" `shouldRespondWith`
                    "Bad request method" {matchStatus = 405}
    with
        (toIOAp
             (operationsHandler
                  { news_and_comments_handle =
                        newsAndCommentsHandler
                            { get_news_filter_by_before_date_from_db =
                                  \hLogger before_date_param page sort ->
                                      return $
                                      Right (testListOfNews (utctDay time))
                            }
                  })) $ do
        it "  server return news" $ do
            get "/news?before_date=2022-11-19" `shouldRespondWith`
                "{\"news\":[{\"category_name'\":\"category_name\",\"news_id'\":1,\"author_name\":\"author_name\",\"date_creation'\":\"2021-11-19\",\"news_text'\":\"news_text\",\"short_title'\":\"title\"}]}"
                    {matchStatus = 200}
    with (toIOAp operationsHandler) $ do
        it "  server return a error message" $ do
            get "/news?before_date=2022-11-19" `shouldRespondWith`
                "News not sended" {matchStatus = 400}
    with (toIOAp operationsHandler) $ do
        describe "get_news_filter_by_content_from_db" $ do
            it "server return error message about wrong method" $ do
                delete "/news?content=something" `shouldRespondWith`
                    "Bad request method" {matchStatus = 405}
    with
        (toIOAp
             (operationsHandler
                  { news_and_comments_handle =
                        newsAndCommentsHandler
                            { get_news_filter_by_content_from_db =
                                  \hLogger content_param page sort ->
                                      return $
                                      Right (testListOfNews (utctDay time))
                            }
                  })) $ do
        it "  server return news" $ do
            get "/news?content=something" `shouldRespondWith`
                "{\"news\":[{\"category_name'\":\"category_name\",\"news_id'\":1,\"author_name\":\"author_name\",\"date_creation'\":\"2021-11-19\",\"news_text'\":\"news_text\",\"short_title'\":\"title\"}]}"
                    {matchStatus = 200}
    with (toIOAp operationsHandler) $ do
        it "  server return a error message" $ do
            get "/news?content=something" `shouldRespondWith`
                "News not sended" {matchStatus = 400}
    with (toIOAp operationsHandler) $ do
        describe "get_news_from_db" $ do
            it "server return error message about wrong method" $ do
                delete "/news" `shouldRespondWith`
                    "Bad request method" {matchStatus = 405}
    with
        (toIOAp
             (operationsHandler
                  { news_and_comments_handle =
                        newsAndCommentsHandler
                            { get_news_from_db =
                                  \hLogger sort page ->
                                      return $
                                      Right (testListOfNews (utctDay time))
                            }
                  })) $ do
        it "  server return news" $ do
            get "/news" `shouldRespondWith`
                "{\"news\":[{\"category_name'\":\"category_name\",\"news_id'\":1,\"author_name\":\"author_name\",\"date_creation'\":\"2021-11-19\",\"news_text'\":\"news_text\",\"short_title'\":\"title\"}]}"
                    {matchStatus = 200}
    with (toIOAp operationsHandler) $ do
        it "  server return a error message" $ do
            get "/news" `shouldRespondWith`
                "News not sended" {matchStatus = 400}

toIOAp :: OperationsHandle IO -> IO Application
toIOAp oh = return $ routes (TokenLifeTime 86400) oh

operationsTests :: IO ()
operationsTests = do
    hspec imageTests
    hspec usersTests
    hspec authorsTests
    hspec categoriesTests
    hspec tagsTests
    hspec draftsTests
    hspec newsTests

draftRequest :: LBS.ByteString
draftRequest =
    "Content-Disposition: form-data; name=\"main_image\"; filename=\"/D:/Desktop/avatarss/avatar1509.jpg\" " <>
    "Content-Type: image/jpeg " <> "image "

draftRequestErr :: LBS.ByteString
draftRequestErr =
    "Content-Disposition: form-data; name=\"main_image\"; filename=\"/D:/Desktop/avatarss/avatar1509.jpg\" " <>
    "Content-Type: tst/jpeg " <> "image "

testNews :: Day -> GetNews
testNews day =
    GetNews
        1
        "title"
        day
        "author_name"
        "category_name"
        "news_text"
        Nothing
        Nothing
        Nothing

testListOfNews :: Day -> NewsArray
testListOfNews day =
    NewsArray
        [ ElemOfNewsArray
              1
              "title"
              day
              "author_name"
              "category_name"
              "news_text"
        ]
