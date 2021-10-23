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
import           Logger                             (Handle)
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
                                                     Comment, CommentArray,
                                                     ContentFilterParam,
                                                     DateFilterParam, GetNews,
                                                     NewsArray, Sort,
                                                     TagAllFilterParam,
                                                     TagFilterParam,
                                                     TagInFilterParam,
                                                     TitleFilterParam)
import           Types.Other                        (ErrorMessage, Id, Page,
                                                     SendId, SuccessMessage,
                                                     Token, TokenLifeTime)
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
        , logger_handle            :: Handle m
        }

operationsHandler :: Handle IO -> Pool Connection -> OperationsHandle IO
operationsHandler hLogger pool =
    OperationsHandle
        { authors_handle = authorsHandler pool
        , categories_handle = categoriesHandler pool
        , drafts_handle = draftsHandler pool
        , images_handle = imagesHandler pool
        , news_and_comments_handle = newsAndCommentsHandler pool
        , tags_handle = tagsHandler pool
        , users_handle = usersHandler pool
        , logger_handle = hLogger
        }

data AuthorsHandle m =
    AuthorsHandle
        { create_author_in_db :: Handle m -> TokenLifeTime -> Maybe Token -> CreateAuthor -> m (Either ErrorMessage SendId)
        , delete_author_in_db :: Handle m -> TokenLifeTime -> Maybe Token -> Maybe AuthorLogin -> m (Either ErrorMessage SuccessMessage)
        , get_authors_list :: Handle m -> Maybe Page -> m (Either ErrorMessage AuthorsList)
        , edit_author_in_db :: Handle m -> TokenLifeTime -> Maybe Token -> EditAuthor -> m (Either ErrorMessage SuccessMessage)
        }

authorsHandler :: Pool Connection -> AuthorsHandle IO
authorsHandler pool =
    AuthorsHandle
        { create_author_in_db = createAuthorInDb pool
        , delete_author_in_db = deleteAuthorInDb pool
        , get_authors_list = getAuthorsList pool
        , edit_author_in_db = editAuthorInDb pool
        }

data CategoriesHandle m =
    CategoriesHandle
        { get_categories_list_from_db :: Handle m -> Maybe Page -> m (Either ErrorMessage ListOfCategories)
        , create_category_on_db :: Handle m -> TokenLifeTime -> Maybe Token -> CreateCategory -> m (Either ErrorMessage SuccessMessage)
        , delete_category_from_db :: Handle m -> TokenLifeTime -> Maybe Token -> Maybe CategoryName -> m (Either ErrorMessage SuccessMessage)
        , edit_category_on_db :: Handle m -> TokenLifeTime -> Maybe Token -> EditCategory -> m (Either ErrorMessage SuccessMessage)
        }

categoriesHandler :: Pool Connection -> CategoriesHandle IO
categoriesHandler pool =
    CategoriesHandle
        { get_categories_list_from_db = getCategoriesListFromDb pool
        , create_category_on_db = createCategoryOnDb pool
        , delete_category_from_db = deleteCategoryFromDb pool
        , edit_category_on_db = editCategoryOnDb pool
        }

data DraftsHandle m =
    DraftsHandle
        { get_drafts_by_author_token :: Handle m -> TokenLifeTime -> Maybe Token -> m (Either ErrorMessage DraftArray)
        , delete_draft_from_db :: Handle m -> TokenLifeTime -> Maybe Token -> Maybe Id -> m (Either ErrorMessage SuccessMessage)
        , get_draft_by_id_from_db :: Handle m -> TokenLifeTime -> Maybe Token -> Id -> m (Either ErrorMessage Draft)
        , create_draft_on_db :: Handle m -> TokenLifeTime -> DraftInf -> Maybe DraftTags -> Maybe Image -> Maybe [Image] -> m (Either ErrorMessage SendId)
        , update_draft_in_db :: Handle m -> TokenLifeTime -> DraftInf -> Maybe DraftTags -> Maybe Image -> Maybe [Image] -> Id -> m (Either ErrorMessage SuccessMessage)
        , public_news_on_db :: Handle m -> TokenLifeTime -> Maybe Token -> Id -> m (Either ErrorMessage SendId)
        }

draftsHandler :: Pool Connection -> DraftsHandle IO
draftsHandler pool =
    DraftsHandle
        { get_drafts_by_author_token = getDraftsByAuthorToken pool
        , delete_draft_from_db = deleteDraftFromDb pool
        , get_draft_by_id_from_db = getDraftByIdFromDb pool
        , create_draft_on_db = createDraftOnDb pool
        , update_draft_in_db = updateDraftInDb pool
        , public_news_on_db = publicNewsOnDb pool
        }

data ImagesHandle m =
    ImagesHandle
        { get_photo :: Handle m -> Id -> m (Either ErrorMessage ImageB)
        , get_photo_list :: Handle m -> Maybe Page -> m (Either ErrorMessage ImageArray)
        }

imagesHandler :: Pool Connection -> ImagesHandle IO
imagesHandler pool =
    ImagesHandle {get_photo = getPhoto pool, get_photo_list = getPhotoList pool}

data NewsAndCommentsHandle m =
    NewsAndCommentsHandle
        { add_comment_to_db :: Handle m -> Comment -> m (Either ErrorMessage SuccessMessage)
        , delete_comment_from_db :: Handle m -> TokenLifeTime -> Maybe Token -> Maybe Id -> m (Either ErrorMessage SuccessMessage)
        , get_comments_by_news_id_from_db :: Handle m -> Maybe Id -> Maybe Page -> m (Either ErrorMessage CommentArray)
        , get_news_by_id_from_db :: Handle m -> Maybe Id -> m (Either ErrorMessage GetNews)
        , get_news_filter_by_tag_in_from_db :: Handle m -> Maybe TagInFilterParam -> Maybe Page -> m (Either ErrorMessage NewsArray)
        , get_news_filter_by_category_id_from_db :: Handle m -> Maybe CategoryFilterParam -> Maybe Page -> Sort -> m (Either ErrorMessage NewsArray)
        , get_news_filter_by_title_from_db :: Handle m -> Maybe TitleFilterParam -> Maybe Page -> Sort -> m (Either ErrorMessage NewsArray)
        , get_news_filter_by_author_name_from_db :: Handle m -> Maybe AuthorFilterParam -> Maybe Page -> Sort -> m (Either ErrorMessage NewsArray)
        , get_news_filter_by_date_from_db :: Handle m -> Maybe DateFilterParam -> Maybe Page -> Sort -> m (Either ErrorMessage NewsArray)
        , get_news_filter_by_tag_all_from_db :: Handle m -> Maybe TagAllFilterParam -> Maybe Page -> Sort -> m (Either ErrorMessage NewsArray)
        , get_news_filter_by_content_from_db :: Handle m -> Maybe ContentFilterParam -> Maybe Page -> Sort -> m (Either ErrorMessage NewsArray)
        , get_news_filter_by_after_date_from_db :: Handle m -> Maybe AfterDateFilterParam -> Maybe Page -> Sort -> m (Either ErrorMessage NewsArray)
        , get_news_filter_by_before_date_from_db :: Handle m -> Maybe BeforeDateFilterParam -> Maybe Page -> Sort -> m (Either ErrorMessage NewsArray)
        , get_news_filter_by_tag_id_from_db :: Handle m -> Maybe TagFilterParam -> Maybe Page -> Sort -> m (Either ErrorMessage NewsArray)
        , get_news_from_db :: Handle m -> Sort -> Maybe Page -> m (Either ErrorMessage NewsArray)
        }

newsAndCommentsHandler :: Pool Connection -> NewsAndCommentsHandle IO
newsAndCommentsHandler pool =
    NewsAndCommentsHandle
        { add_comment_to_db = addCommentToDb pool
        , delete_comment_from_db = deleteCommentFromDb pool
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
        }

data TagsHandle m =
    TagsHandle
        { create_tag_in_db :: Handle m -> TokenLifeTime -> Maybe Token -> Maybe TagName -> m (Either ErrorMessage SendId)
        , delete_tag_from_db :: Handle m -> TokenLifeTime -> Maybe Token -> Maybe TagName -> m (Either ErrorMessage SuccessMessage)
        , get_tags_list_from_db :: Handle m -> Maybe Page -> m (Either ErrorMessage TagsList)
        , edit_tag_in_db :: Handle m -> TokenLifeTime -> Maybe Token -> EditTag -> m (Either ErrorMessage SuccessMessage)
        }

tagsHandler :: Pool Connection -> TagsHandle IO
tagsHandler pool =
    TagsHandle
        { create_tag_in_db = createTagInDb pool
        , delete_tag_from_db = deleteTagFromDb pool
        , get_tags_list_from_db = getTagsListFromDb pool
        , edit_tag_in_db = editTagInDb pool
        }

data UsersHandle m =
    UsersHandle
        { auth :: Handle m -> Maybe Login -> Maybe Password -> m (Either ErrorMessage SuccessMessage)
        , create_user_in_db :: Handle m -> CreateUser -> m (Either ErrorMessage SuccessMessage)
        , delete_user_from_db :: Handle m -> TokenLifeTime -> Maybe Token -> Maybe Login -> m (Either ErrorMessage SuccessMessage)
        , profile_on_db :: Handle m -> TokenLifeTime -> Maybe Token -> m (Either ErrorMessage Profile)
        }

usersHandler :: Pool Connection -> UsersHandle IO
usersHandler pool =
    UsersHandle
        { auth = authentication pool
        , create_user_in_db = createUserInDb pool
        , delete_user_from_db = deleteUserFromDb pool
        , profile_on_db = profileOnDb pool
        }
