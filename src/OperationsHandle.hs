module OperationsHandle where

import           Data.Pool                          (Pool)
import           Database.PostgreSQL.Simple         (Connection)
import           Databaseoperations.Authors         (createAuthorInDb,
                                                     deleteAuthorInDb,
                                                     editAuthorInDb,
                                                     getAuthorsList)
import           Databaseoperations.Categories      (createCategoryOnDb,
                                                     deleteCategoryFromDb,
                                                     editCategoryOnDb,
                                                     getCategoriesListFromDb)
import           Databaseoperations.Drafts          (createDraftOnDb,
                                                     deleteDraftFromDb,
                                                     getDraftByIdFromDb,
                                                     getDraftsByAuthorToken,
                                                     publicNewsOnDb,
                                                     updateDraftInDb)
import           Databaseoperations.Images          (getPhoto, getPhotoList)
import           Databaseoperations.NewsAndComments (addCommentToDb,
                                                     deleteCommentFromDb,
                                                     getCommentsByNewsIdFromDb,
                                                     getNewsByIdFromDb,
                                                     getNewsFilterByAfterDateFromDb,
                                                     getNewsFilterByAuthorNameFromDb,
                                                     getNewsFilterByBeforeDateFromDb,
                                                     getNewsFilterByCategoryIdFromDb,
                                                     getNewsFilterByContentFromDb,
                                                     getNewsFilterByDateFromDb,
                                                     getNewsFilterByTagAllFromDb,
                                                     getNewsFilterByTagIdFromDb,
                                                     getNewsFilterByTagInFromDb,
                                                     getNewsFilterByTitleFromDb,
                                                     getNewsFromDb)
import           Databaseoperations.Tags            (createTagInDb,
                                                     deleteTagFromDb,
                                                     editTagInDb,
                                                     getTagsListFromDb)
import           Databaseoperations.Users           (authentication,
                                                     createUserInDb,
                                                     deleteUserFromDb,
                                                     profileOnDb)
import           Logger                             (LoggerHandle)
import           Types.Authors                      (AuthorLogin, AuthorsList,
                                                     CreateAuthor, EditAuthor)
import           Types.Categories                   (CategoryName,
                                                     CreateCategory,
                                                     EditCategory,
                                                     ListOfCategories)
import           Types.Drafts                       (Draft, DraftArray,
                                                     DraftInf, DraftTags)
import           Types.Images                       (Image, ImageArray, ImageB)
import           Types.NewsAndComments              (AfterDateFilterParam,
                                                     AuthorFilterParam,
                                                     BeforeDateFilterParam,
                                                     CategoryFilterParam,
                                                     CommentArray,
                                                     CommentWithoutTokenLifeTime,
                                                     ContentFilterParam,
                                                     DateFilterParam, GetNews,
                                                     NewsArray, Sort,
                                                     TagAllFilterParam,
                                                     TagFilterParam,
                                                     TagInFilterParam,
                                                     TitleFilterParam)
import           Types.Other                        (Id, Page, SendId,
                                                     SomeError, Token,
                                                     TokenLifeTime)
import           Types.Tags                         (EditTag, TagName, TagsList)
import           Types.Users                        (CreateUser, Login,
                                                     Password, Profile)

data OperationsHandle m =
    OperationsHandle
        { authors_handle           :: AuthorsHandle m
        , categories_handle        :: CategoriesHandle m
        , drafts_handle            :: DraftsHandle m
        , images_handle            :: ImagesHandle m
        , news_and_comments_handle :: NewsAndCommentsHandle m
        , tags_handle              :: TagsHandle m
        , users_handle             :: UsersHandle m
        }

operationsHandler ::
       LoggerHandle IO
    -> Pool Connection
    -> TokenLifeTime
    -> OperationsHandle IO
operationsHandler hLogger pool tokenLifeTime =
    OperationsHandle
        { authors_handle = authorsHandler pool hLogger tokenLifeTime
        , categories_handle = categoriesHandler pool hLogger tokenLifeTime
        , drafts_handle = draftsHandler pool hLogger tokenLifeTime
        , images_handle = imagesHandler pool hLogger
        , news_and_comments_handle =
              newsAndCommentsHandler pool hLogger tokenLifeTime
        , tags_handle = tagsHandler pool hLogger tokenLifeTime
        , users_handle = usersHandler pool hLogger tokenLifeTime
        }

data AuthorsHandle m =
    AuthorsHandle
        { create_author_in_db :: LoggerHandle m -> Maybe Token -> CreateAuthor -> m (Either SomeError SendId)
        , delete_author_in_db :: LoggerHandle m -> Maybe Token -> Maybe AuthorLogin -> m (Either SomeError ())
        , get_authors_list :: LoggerHandle m -> Maybe Page -> m (Either SomeError AuthorsList)
        , edit_author_in_db :: LoggerHandle m -> Maybe Token -> EditAuthor -> m (Either SomeError ())
        , authors_logger :: LoggerHandle m
        }

authorsHandler ::
       Pool Connection -> LoggerHandle IO -> TokenLifeTime -> AuthorsHandle IO
authorsHandler pool hLogger tokenLifeTime =
    AuthorsHandle
        { create_author_in_db = createAuthorInDb pool tokenLifeTime
        , delete_author_in_db = deleteAuthorInDb pool tokenLifeTime
        , get_authors_list = getAuthorsList pool
        , edit_author_in_db = editAuthorInDb pool tokenLifeTime
        , authors_logger = hLogger
        }

data CategoriesHandle m =
    CategoriesHandle
        { get_categories_list_from_db :: LoggerHandle m -> Maybe Page -> m (Either SomeError ListOfCategories)
        , create_category_on_db :: LoggerHandle m -> Maybe Token -> CreateCategory -> m (Either SomeError SendId)
        , delete_category_from_db :: LoggerHandle m -> Maybe Token -> Maybe CategoryName -> m (Either SomeError ())
        , edit_category_on_db :: LoggerHandle m -> Maybe Token -> EditCategory -> m (Either SomeError ())
        , categories_logger :: LoggerHandle m
        }

categoriesHandler ::
       Pool Connection
    -> LoggerHandle IO
    -> TokenLifeTime
    -> CategoriesHandle IO
categoriesHandler pool hLogger tokenLifeTime =
    CategoriesHandle
        { get_categories_list_from_db = getCategoriesListFromDb pool
        , create_category_on_db = createCategoryOnDb pool tokenLifeTime
        , delete_category_from_db = deleteCategoryFromDb pool tokenLifeTime
        , edit_category_on_db = editCategoryOnDb pool tokenLifeTime
        , categories_logger = hLogger
        }

data DraftsHandle m =
    DraftsHandle
        { get_drafts_by_author_token :: LoggerHandle m -> Maybe Token -> m (Either SomeError DraftArray)
        , delete_draft_from_db :: LoggerHandle m -> Maybe Token -> Maybe Id -> m (Either SomeError ())
        , get_draft_by_id_from_db :: LoggerHandle m -> Maybe Token -> Id -> m (Either SomeError Draft)
        , create_draft_on_db :: LoggerHandle m -> DraftInf -> Maybe DraftTags -> Maybe Image -> Maybe [Image] -> m (Either SomeError SendId)
        , update_draft_in_db :: LoggerHandle m -> DraftInf -> Maybe DraftTags -> Maybe Image -> Maybe [Image] -> Id -> m (Either SomeError ())
        , public_news_on_db :: LoggerHandle m -> Maybe Token -> Id -> m (Either SomeError SendId)
        , drafts_logger :: LoggerHandle m
        }

draftsHandler ::
       Pool Connection -> LoggerHandle IO -> TokenLifeTime -> DraftsHandle IO
draftsHandler pool hLogger tokenLifeTime =
    DraftsHandle
        { get_drafts_by_author_token = getDraftsByAuthorToken pool tokenLifeTime
        , delete_draft_from_db = deleteDraftFromDb pool tokenLifeTime
        , get_draft_by_id_from_db = getDraftByIdFromDb pool tokenLifeTime
        , create_draft_on_db = createDraftOnDb pool tokenLifeTime
        , update_draft_in_db = updateDraftInDb pool tokenLifeTime
        , public_news_on_db = publicNewsOnDb pool tokenLifeTime
        , drafts_logger = hLogger
        }

data ImagesHandle m =
    ImagesHandle
        { get_photo :: LoggerHandle m -> Id -> m (Either SomeError ImageB)
        , get_photo_list :: LoggerHandle m -> Maybe Page -> m (Either SomeError ImageArray)
        , photos_logger :: LoggerHandle m
        }

imagesHandler :: Pool Connection -> LoggerHandle IO -> ImagesHandle IO
imagesHandler pool hLogger =
    ImagesHandle
        { get_photo = getPhoto pool
        , get_photo_list = getPhotoList pool
        , photos_logger = hLogger
        }

data NewsAndCommentsHandle m =
    NewsAndCommentsHandle
        { add_comment_to_db :: LoggerHandle m -> CommentWithoutTokenLifeTime -> m (Either SomeError ())
        , delete_comment_from_db :: LoggerHandle m -> Maybe Token -> Maybe Id -> m (Either SomeError ())
        , get_comments_by_news_id_from_db :: LoggerHandle m -> Maybe Id -> Maybe Page -> m (Either SomeError CommentArray)
        , get_news_by_id_from_db :: LoggerHandle m -> Maybe Id -> m (Either SomeError GetNews)
        , get_news_filter_by_tag_in_from_db :: LoggerHandle m -> Maybe TagInFilterParam -> Maybe Page -> m (Either SomeError NewsArray)
        , get_news_filter_by_category_id_from_db :: LoggerHandle m -> Maybe CategoryFilterParam -> Maybe Page -> Sort -> m (Either SomeError NewsArray)
        , get_news_filter_by_title_from_db :: LoggerHandle m -> Maybe TitleFilterParam -> Maybe Page -> Sort -> m (Either SomeError NewsArray)
        , get_news_filter_by_author_name_from_db :: LoggerHandle m -> Maybe AuthorFilterParam -> Maybe Page -> Sort -> m (Either SomeError NewsArray)
        , get_news_filter_by_date_from_db :: LoggerHandle m -> Maybe DateFilterParam -> Maybe Page -> Sort -> m (Either SomeError NewsArray)
        , get_news_filter_by_tag_all_from_db :: LoggerHandle m -> Maybe TagAllFilterParam -> Maybe Page -> Sort -> m (Either SomeError NewsArray)
        , get_news_filter_by_content_from_db :: LoggerHandle m -> Maybe ContentFilterParam -> Maybe Page -> Sort -> m (Either SomeError NewsArray)
        , get_news_filter_by_after_date_from_db :: LoggerHandle m -> Maybe AfterDateFilterParam -> Maybe Page -> Sort -> m (Either SomeError NewsArray)
        , get_news_filter_by_before_date_from_db :: LoggerHandle m -> Maybe BeforeDateFilterParam -> Maybe Page -> Sort -> m (Either SomeError NewsArray)
        , get_news_filter_by_tag_id_from_db :: LoggerHandle m -> Maybe TagFilterParam -> Maybe Page -> Sort -> m (Either SomeError NewsArray)
        , get_news_from_db :: LoggerHandle m -> Sort -> Maybe Page -> m (Either SomeError NewsArray)
        , news_logger :: LoggerHandle m
        }

newsAndCommentsHandler ::
       Pool Connection
    -> LoggerHandle IO
    -> TokenLifeTime
    -> NewsAndCommentsHandle IO
newsAndCommentsHandler pool hLogger tokenLifeTime =
    NewsAndCommentsHandle
        { add_comment_to_db = addCommentToDb pool tokenLifeTime
        , delete_comment_from_db = deleteCommentFromDb pool tokenLifeTime
        , get_comments_by_news_id_from_db = getCommentsByNewsIdFromDb pool
        , get_news_by_id_from_db = getNewsByIdFromDb pool
        , get_news_filter_by_tag_in_from_db = getNewsFilterByTagInFromDb pool
        , get_news_filter_by_category_id_from_db =
              getNewsFilterByCategoryIdFromDb pool
        , get_news_filter_by_title_from_db = getNewsFilterByTitleFromDb pool
        , get_news_filter_by_author_name_from_db =
              getNewsFilterByAuthorNameFromDb pool
        , get_news_filter_by_date_from_db = getNewsFilterByDateFromDb pool
        , get_news_filter_by_tag_all_from_db = getNewsFilterByTagAllFromDb pool
        , get_news_filter_by_content_from_db = getNewsFilterByContentFromDb pool
        , get_news_filter_by_after_date_from_db =
              getNewsFilterByAfterDateFromDb pool
        , get_news_filter_by_before_date_from_db =
              getNewsFilterByBeforeDateFromDb pool
        , get_news_filter_by_tag_id_from_db = getNewsFilterByTagIdFromDb pool
        , get_news_from_db = getNewsFromDb pool
        , news_logger = hLogger
        }

data TagsHandle m =
    TagsHandle
        { create_tag_in_db :: LoggerHandle m -> Maybe Token -> Maybe TagName -> m (Either SomeError SendId)
        , delete_tag_from_db :: LoggerHandle m -> Maybe Token -> Maybe TagName -> m (Either SomeError ())
        , get_tags_list_from_db :: LoggerHandle m -> Maybe Page -> m (Either SomeError TagsList)
        , edit_tag_in_db :: LoggerHandle m -> Maybe Token -> EditTag -> m (Either SomeError ())
        , tags_logger :: LoggerHandle m
        }

tagsHandler ::
       Pool Connection -> LoggerHandle IO -> TokenLifeTime -> TagsHandle IO
tagsHandler pool hLogger tokenLifeTime =
    TagsHandle
        { create_tag_in_db = createTagInDb pool tokenLifeTime
        , delete_tag_from_db = deleteTagFromDb pool tokenLifeTime
        , get_tags_list_from_db = getTagsListFromDb pool
        , edit_tag_in_db = editTagInDb pool tokenLifeTime
        , tags_logger = hLogger
        }

data UsersHandle m =
    UsersHandle
        { auth :: LoggerHandle m -> Maybe Login -> Maybe Password -> m (Either SomeError Token)
        , create_user_in_db :: LoggerHandle m -> CreateUser -> m (Either SomeError Token)
        , delete_user_from_db :: LoggerHandle m -> Maybe Token -> Maybe Login -> m (Either SomeError ())
        , profile_on_db :: LoggerHandle m -> Maybe Token -> m (Either SomeError Profile)
        , users_logger :: LoggerHandle m
        }

usersHandler ::
       Pool Connection -> LoggerHandle IO -> TokenLifeTime -> UsersHandle IO
usersHandler pool hLogger tokenLifeTime =
    UsersHandle
        { auth = authentication pool
        , create_user_in_db = createUserInDb pool
        , delete_user_from_db = deleteUserFromDb pool tokenLifeTime
        , profile_on_db = profileOnDb pool tokenLifeTime
        , users_logger = hLogger
        }
