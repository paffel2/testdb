module OperationsHandle where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Pool (Pool)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection)
import Databaseoperations.Authors
    ( createAuthorInDb
    , deleteAuthorInDb
    , editAuthorInDb
    , getAuthorsList
    )
import Databaseoperations.Categories
    ( createCategoryOnDb
    , deleteCategoryFromDb
    , editCategoryOnDb
    , getCategoriesListFromDb
    )
import Databaseoperations.Drafts
    ( createDraftOnDb
    , deleteDraftFromDb
    , getDraftByIdFromDb
    , getDraftsByAuthorToken
    , publicNewsOnDb
    , updateDraftInDb
    )
import Databaseoperations.Images (getPhoto, getPhotoList)
import Databaseoperations.InitDb (createDb)
import Databaseoperations.NewsAndComments
    ( addCommentToDb
    , deleteCommentFromDb
    , getCommentsByNewsIdFromDb
    , getNewsByIdFromDb
    , getNewsFilterByAfterDateFromDb
    , getNewsFilterByAuthorNameFromDb
    , getNewsFilterByBeforeDateFromDb
    , getNewsFilterByCategoryIdFromDb
    , getNewsFilterByContentFromDb
    , getNewsFilterByDateFromDb
    , getNewsFilterByTagAllFromDb
    , getNewsFilterByTagIdFromDb
    , getNewsFilterByTagInFromDb
    , getNewsFilterByTitleFromDb
    , getNewsFromDb
    )
import Databaseoperations.Tags
    ( createTagInDb
    , deleteTagFromDb
    , editTagInDb
    , getTagsListFromDb
    )
import Databaseoperations.Users
    ( authentication
    , createUserInDb
    , deleteUserFromDb
    , profileOnDb
    )
import Logger (Handle)
import Types
    ( AuthorsList
    , CommentArray
    , DatabaseAddress
    , Draft
    , DraftArray
    , GetNews
    , Image
    , ImageArray
    , ImageB
    , ListOfCategories
    , NewsArray
    , Profile
    , TagsList
    , TokenLifeTime
    )

data OperationsHandle m =
    OperationsHandle
        { authors_handle :: AuthorsHandle m
        , categories_handle :: CategoriesHandle m
        , drafts_handle :: DraftsHandle m
        , images_handle :: ImagesHandle m
        , init_db_handle :: InitDbHandle m
        , news_and_comments_handle :: NewsAndCommentsHandle m
        , tags_handle :: TagsHandle m
        , users_handle :: UsersHandle m
        }

operationsHandler :: OperationsHandle IO
operationsHandler =
    OperationsHandle
        { authors_handle = authorsHandler
        , categories_handle = categoriesHandler
        , drafts_handle = draftsHandler
        , images_handle = imagesHandler
        , init_db_handle = initDbHandler
        , news_and_comments_handle = newsAndCommentsHandler
        , tags_handle = tagsHandler
        , users_handle = usersHandler
        }

data AuthorsHandle m =
    AuthorsHandle
        { create_author_in_db :: Handle m -> Pool Connection -> TokenLifeTime -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> m (Either LBS.ByteString Int)
        , delete_author_in_db :: Handle m -> Pool Connection -> TokenLifeTime -> Maybe T.Text -> Maybe T.Text -> m (Either LBS.ByteString LBS.ByteString)
        , get_authors_list :: Handle m -> Pool Connection -> Maybe BC.ByteString -> m (Either LBS.ByteString AuthorsList)
        , edit_author_in_db :: Handle m -> Pool Connection -> TokenLifeTime -> Maybe T.Text -> Maybe Int -> Maybe T.Text -> m (Either LBS.ByteString LBS.ByteString)
        }

authorsHandler :: AuthorsHandle IO
authorsHandler =
    AuthorsHandle
        { create_author_in_db = createAuthorInDb
        , delete_author_in_db = deleteAuthorInDb
        , get_authors_list = getAuthorsList
        , edit_author_in_db = editAuthorInDb
        }

data CategoriesHandle m =
    CategoriesHandle
        { get_categories_list_from_db :: Handle m -> Pool Connection -> Maybe BC.ByteString -> m (Either LBS.ByteString ListOfCategories)
        , create_category_on_db :: Handle m -> Pool Connection -> TokenLifeTime -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> m (Either LBS.ByteString LBS.ByteString)
        , delete_category_from_db :: Handle m -> Pool Connection -> TokenLifeTime -> Maybe T.Text -> Maybe T.Text -> m (Either LBS.ByteString LBS.ByteString)
        , edit_category_on_db :: Handle m -> Pool Connection -> TokenLifeTime -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> m (Either LBS.ByteString LBS.ByteString)
        }

categoriesHandler :: CategoriesHandle IO
categoriesHandler =
    CategoriesHandle
        { get_categories_list_from_db = getCategoriesListFromDb
        , create_category_on_db = createCategoryOnDb
        , delete_category_from_db = deleteCategoryFromDb
        , edit_category_on_db = editCategoryOnDb
        }

data DraftsHandle m =
    DraftsHandle
        { get_drafts_by_author_token :: Handle m -> Pool Connection -> TokenLifeTime -> Maybe T.Text -> m (Either LBS.ByteString DraftArray)
        , delete_draft_from_db :: Handle m -> Pool Connection -> TokenLifeTime -> Maybe T.Text -> Maybe BC.ByteString -> m (Either LBS.ByteString LBS.ByteString)
        , get_draft_by_id_from_db :: Handle m -> Pool Connection -> TokenLifeTime -> Maybe T.Text -> Int -> m (Either LBS.ByteString Draft)
        , create_draft_on_db :: Handle m -> Pool Connection -> TokenLifeTime -> Maybe BC.ByteString -> Maybe T.Text -> Maybe BC.ByteString -> Maybe T.Text -> Maybe T.Text -> Maybe Image -> Maybe [Image] -> m (Either LBS.ByteString Int)
        , update_draft_in_db :: Handle m -> Pool Connection -> TokenLifeTime -> Maybe BC.ByteString -> Maybe T.Text -> Maybe BC.ByteString -> Maybe T.Text -> Maybe T.Text -> Maybe Image -> Maybe [Image] -> Int -> m (Either LBS.ByteString LBS.ByteString)
        , public_news_on_db :: Handle m -> Pool Connection -> TokenLifeTime -> Maybe T.Text -> Int -> m (Either LBS.ByteString Int)
        }

draftsHandler :: DraftsHandle IO
draftsHandler =
    DraftsHandle
        { get_drafts_by_author_token = getDraftsByAuthorToken
        , delete_draft_from_db = deleteDraftFromDb
        , get_draft_by_id_from_db = getDraftByIdFromDb
        , create_draft_on_db = createDraftOnDb
        , update_draft_in_db = updateDraftInDb
        , public_news_on_db = publicNewsOnDb
        }

data ImagesHandle m =
    ImagesHandle
        { get_photo :: Handle m -> Pool Connection -> Int -> m (Either LBS.ByteString ImageB)
        , get_photo_list :: Handle m -> Pool Connection -> Maybe BC.ByteString -> m (Either LBS.ByteString ImageArray)
        }

imagesHandler :: ImagesHandle IO
imagesHandler =
    ImagesHandle {get_photo = getPhoto, get_photo_list = getPhotoList}

newtype InitDbHandle m =
    InitDbHandle
        { create_db :: Handle m -> Pool Connection -> DatabaseAddress -> m (Either LBS.ByteString LBS.ByteString)
        }

initDbHandler :: InitDbHandle IO
initDbHandler = InitDbHandle createDb

data NewsAndCommentsHandle m =
    NewsAndCommentsHandle
        { add_comment_to_db :: Handle m -> Pool Connection -> TokenLifeTime -> T.Text -> Maybe Int -> Maybe T.Text -> m (Either LBS.ByteString LBS.ByteString)
        , delete_comment_from_db :: Handle m -> Pool Connection -> TokenLifeTime -> Maybe T.Text -> Maybe Int -> m (Either LBS.ByteString LBS.ByteString)
        , get_comments_by_news_id_from_db :: Handle m -> Pool Connection -> Maybe Int -> Maybe BC.ByteString -> m (Either LBS.ByteString CommentArray)
        , get_news_by_id_from_db :: Handle m -> Pool Connection -> Maybe Int -> m (Either LBS.ByteString GetNews)
        , get_news_filter_by_tag_in_from_db :: Handle m -> Pool Connection -> Maybe BC.ByteString -> Maybe BC.ByteString -> m (Either LBS.ByteString NewsArray)
        , get_news_filter_by_category_id_from_db :: Handle m -> Pool Connection -> Maybe BC.ByteString -> Maybe BC.ByteString -> BC.ByteString -> m (Either LBS.ByteString NewsArray)
        , get_news_filter_by_title_from_db :: Handle m -> Pool Connection -> Maybe BC.ByteString -> Maybe BC.ByteString -> BC.ByteString -> m (Either LBS.ByteString NewsArray)
        , get_news_filter_by_author_name_from_db :: Handle m -> Pool Connection -> Maybe BC.ByteString -> Maybe BC.ByteString -> BC.ByteString -> m (Either LBS.ByteString NewsArray)
        , get_news_filter_by_date_from_db :: Handle m -> Pool Connection -> Maybe BC.ByteString -> Maybe BC.ByteString -> BC.ByteString -> m (Either LBS.ByteString NewsArray)
        , get_news_filter_by_tag_all_from_db :: Handle m -> Pool Connection -> Maybe BC.ByteString -> Maybe BC.ByteString -> BC.ByteString -> m (Either LBS.ByteString NewsArray)
        , get_news_filter_by_content_from_db :: Handle m -> Pool Connection -> Maybe BC.ByteString -> Maybe BC.ByteString -> BC.ByteString -> m (Either LBS.ByteString NewsArray)
        , get_news_filter_by_after_date_from_db :: Handle m -> Pool Connection -> Maybe BC.ByteString -> Maybe BC.ByteString -> BC.ByteString -> m (Either LBS.ByteString NewsArray)
        , get_news_filter_by_before_date_from_db :: Handle m -> Pool Connection -> Maybe BC.ByteString -> Maybe BC.ByteString -> BC.ByteString -> m (Either LBS.ByteString NewsArray)
        , get_news_filter_by_tag_id_from_db :: Handle m -> Pool Connection -> Maybe BC.ByteString -> Maybe BC.ByteString -> BC.ByteString -> m (Either LBS.ByteString NewsArray)
        , get_news_from_db :: Handle m -> Pool Connection -> BC.ByteString -> Maybe BC.ByteString -> m (Either LBS.ByteString NewsArray)
        }

newsAndCommentsHandler :: NewsAndCommentsHandle IO
newsAndCommentsHandler =
    NewsAndCommentsHandle
        { add_comment_to_db = addCommentToDb
        , delete_comment_from_db = deleteCommentFromDb
        , get_comments_by_news_id_from_db = getCommentsByNewsIdFromDb
        , get_news_by_id_from_db = getNewsByIdFromDb
        , get_news_filter_by_tag_in_from_db = getNewsFilterByTagInFromDb
        , get_news_filter_by_category_id_from_db =
              getNewsFilterByCategoryIdFromDb
        , get_news_filter_by_title_from_db = getNewsFilterByTitleFromDb
        , get_news_filter_by_author_name_from_db =
              getNewsFilterByAuthorNameFromDb
        , get_news_filter_by_date_from_db = getNewsFilterByDateFromDb
        , get_news_filter_by_tag_all_from_db = getNewsFilterByTagAllFromDb
        , get_news_filter_by_content_from_db = getNewsFilterByContentFromDb
        , get_news_filter_by_after_date_from_db = getNewsFilterByAfterDateFromDb
        , get_news_filter_by_before_date_from_db =
              getNewsFilterByBeforeDateFromDb
        , get_news_filter_by_tag_id_from_db = getNewsFilterByTagIdFromDb
        , get_news_from_db = getNewsFromDb
        }

data TagsHandle m =
    TagsHandle
        { create_tag_in_db :: Handle m -> Pool Connection -> TokenLifeTime -> Maybe T.Text -> Maybe T.Text -> m (Either LBS.ByteString Int)
        , delete_tag_from_db :: Handle m -> Pool Connection -> TokenLifeTime -> Maybe T.Text -> Maybe T.Text -> m (Either LBS.ByteString LBS.ByteString)
        , get_tags_list_from_db :: Handle m -> Pool Connection -> Maybe BC.ByteString -> m (Either LBS.ByteString TagsList)
        , edit_tag_in_db :: Handle m -> Pool Connection -> TokenLifeTime -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> m (Either LBS.ByteString LBS.ByteString)
        }

tagsHandler :: TagsHandle IO
tagsHandler =
    TagsHandle
        { create_tag_in_db = createTagInDb
        , delete_tag_from_db = deleteTagFromDb
        , get_tags_list_from_db = getTagsListFromDb
        , edit_tag_in_db = editTagInDb
        }

data UsersHandle m =
    UsersHandle
        { auth :: Handle m -> Pool Connection -> T.Text -> T.Text -> m (Either LBS.ByteString LBS.ByteString)
        , create_user_in_db :: Handle m -> Pool Connection -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> BC.ByteString -> BC.ByteString -> LBS.ByteString -> m (Either LBS.ByteString LBS.ByteString)
        , delete_user_from_db :: Handle m -> Pool Connection -> TokenLifeTime -> Maybe T.Text -> BC.ByteString -> m (Either LBS.ByteString LBS.ByteString)
        , profile_on_db :: Handle m -> Pool Connection -> TokenLifeTime -> Maybe T.Text -> m (Either LBS.ByteString Profile)
        }

usersHandler :: UsersHandle IO
usersHandler =
    UsersHandle
        { auth = authentication
        , create_user_in_db = createUserInDb
        , delete_user_from_db = deleteUserFromDb
        , profile_on_db = profileOnDb
        }
