{-# LANGUAGE FlexibleContexts #-}

module OperationsHandle where

import           Control.Monad.Except               (MonadError, MonadIO (..))
import qualified Data.ByteString.Lazy               as LBS
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
import           Databaseoperations.Images          (getPhotoFromDb,
                                                     getPhotoListFromDb)
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
import           Network.Wai                        (Request)
import           Network.Wai.Parse                  (File, Param, lbsBackEnd,
                                                     parseRequestBody)
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

data AuthorsHandle m =
    AuthorsHandle
        { ahGetAuthorsList   :: Maybe Page -> m AuthorsList
        , ahEditAuthorInDb   :: Maybe Token -> EditAuthor -> m ()
        , ahDeleteAuthorInDb :: Maybe Token -> Maybe AuthorLogin -> m ()
        , ahCreateAuthorInDb :: Maybe Token -> CreateAuthor -> m SendId
        , ahParseRequestBody :: Request -> m ([Param], [File LBS.ByteString])
        }

authorsHandler ::
       (MonadIO m, MonadError SomeError m)
    => Pool Connection
    -> TokenLifeTime
    -> AuthorsHandle m
authorsHandler pool tokenLifeTime =
    AuthorsHandle
        { ahGetAuthorsList = getAuthorsList pool
        , ahEditAuthorInDb = editAuthorInDb pool tokenLifeTime
        , ahDeleteAuthorInDb = deleteAuthorInDb pool tokenLifeTime
        , ahCreateAuthorInDb = createAuthorInDb pool tokenLifeTime
        , ahParseRequestBody = liftIO . parseRequestBody lbsBackEnd
        }

data CategoriesHandle m =
    CategoriesHandle
        { chGetCategoriesListFromDb :: Maybe Page -> m ListOfCategories
        , chCreateCategoryOnDb :: Maybe Token -> CreateCategory -> m SendId
        , chDeleteCategoryFromDb :: Maybe Token -> Maybe CategoryName -> m ()
        , chEditCategoryOnDb :: Maybe Token -> EditCategory -> m ()
        , chParseRequestBody :: Request -> m ([Param], [File LBS.ByteString])
        }

categoriesHandler ::
       (MonadIO m, MonadError SomeError m)
    => Pool Connection
    -> TokenLifeTime
    -> CategoriesHandle m
categoriesHandler pool tokenLifeTime =
    CategoriesHandle
        { chGetCategoriesListFromDb = getCategoriesListFromDb pool
        , chCreateCategoryOnDb = createCategoryOnDb pool tokenLifeTime
        , chDeleteCategoryFromDb = deleteCategoryFromDb pool tokenLifeTime
        , chEditCategoryOnDb = editCategoryOnDb pool tokenLifeTime
        , chParseRequestBody = liftIO . parseRequestBody lbsBackEnd
        }

data DraftsHandle m =
    DraftsHandle
        { dhGetDraftsByAuthorToken :: Maybe Token -> m DraftArray
        , dhDeleteDraftFromDb :: Maybe Token -> Maybe Id -> m ()
        , dhGetDraftByIdFromDb :: Maybe Token -> Id -> m Draft
        , dhCreateDraftOnDb :: DraftInf -> Maybe DraftTags -> Maybe Image -> Maybe [Image] -> m SendId
        , dhUpdateDraftInDb :: DraftInf -> Maybe DraftTags -> Maybe Image -> Maybe [Image] -> Id -> m ()
        , dhPublicNewsOnDb :: Maybe Token -> Id -> m SendId
        , dhParseRequestBody :: Request -> m ([Param], [File LBS.ByteString])
        }

draftsHandler ::
       (MonadIO m, MonadError SomeError m)
    => Pool Connection
    -> TokenLifeTime
    -> DraftsHandle m
draftsHandler pool tokenLifeTime =
    DraftsHandle
        { dhGetDraftsByAuthorToken = getDraftsByAuthorToken pool tokenLifeTime
        , dhDeleteDraftFromDb = deleteDraftFromDb pool tokenLifeTime
        , dhGetDraftByIdFromDb = getDraftByIdFromDb pool tokenLifeTime
        , dhCreateDraftOnDb = createDraftOnDb pool tokenLifeTime
        , dhUpdateDraftInDb = updateDraftInDb pool tokenLifeTime
        , dhPublicNewsOnDb = publicNewsOnDb pool tokenLifeTime
        , dhParseRequestBody = liftIO . parseRequestBody lbsBackEnd
        }

data ImagesHandle m =
    ImagesHandle
        { ihGetPhoto     :: Id -> m ImageB
        , ihGetPhotoList :: Maybe Page -> m ImageArray
        }

imagesHandler ::
       (MonadIO m, MonadError SomeError m) => Pool Connection -> ImagesHandle m
imagesHandler pool =
    ImagesHandle
        { ihGetPhoto = getPhotoFromDb pool
        , ihGetPhotoList = getPhotoListFromDb pool
        }

data NewsAndCommentsHandle m =
    NewsAndCommentsHandle
        { nchAddCommentToDb :: CommentWithoutTokenLifeTime -> m ()
        , nchDeleteCommentFromDb :: Maybe Token -> Maybe Id -> m ()
        , nchGetCommentsByNewsIdFromDb :: Maybe Id -> Maybe Page -> m CommentArray
        , nchGetNewsByIdFromDb :: Maybe Id -> m GetNews
        , nchGetNewsFilterByTagInFromDb :: Maybe TagInFilterParam -> Maybe Page -> m NewsArray
        , nchGetNewsFilterByCategoryIdFromDb :: Maybe CategoryFilterParam -> Maybe Page -> Sort -> m NewsArray
        , nchGetNewsFilterByTitleFromDb :: Maybe TitleFilterParam -> Maybe Page -> Sort -> m NewsArray
        , nchGetgetNewsFilterByAuthorNameFromDb :: Maybe AuthorFilterParam -> Maybe Page -> Sort -> m NewsArray
        , nchGetNewsFilterByDateFromDb :: Maybe DateFilterParam -> Maybe Page -> Sort -> m NewsArray
        , nchGetNewsFilterByTagAllFromDb :: Maybe TagAllFilterParam -> Maybe Page -> Sort -> m NewsArray
        , nchGetNewsFilterByContentFromDb :: Maybe ContentFilterParam -> Maybe Page -> Sort -> m NewsArray
        , nchGetNewsFilterByAfterDateFromDb :: Maybe AfterDateFilterParam -> Maybe Page -> Sort -> m NewsArray
        , nchGetNewsFilterByBeforeDateFromDb :: Maybe BeforeDateFilterParam -> Maybe Page -> Sort -> m NewsArray
        , nchGetNewsFilterByTagIdFromDb :: Maybe TagFilterParam -> Maybe Page -> Sort -> m NewsArray
        , nchGetNewsFromDb :: Sort -> Maybe Page -> m NewsArray
        , nchParseRequestBody :: Request -> m ([Param], [File LBS.ByteString])
        }

newsAndCommentsHandler ::
       (MonadIO m, MonadError SomeError m)
    => Pool Connection
    -> TokenLifeTime
    -> NewsAndCommentsHandle m
newsAndCommentsHandler pool tokenLifeTime =
    NewsAndCommentsHandle
        { nchAddCommentToDb = addCommentToDb pool tokenLifeTime
        , nchDeleteCommentFromDb = deleteCommentFromDb pool tokenLifeTime
        , nchGetCommentsByNewsIdFromDb = getCommentsByNewsIdFromDb pool
        , nchGetNewsByIdFromDb = getNewsByIdFromDb pool
        , nchGetNewsFilterByTagInFromDb = getNewsFilterByTagInFromDb pool
        , nchGetNewsFilterByCategoryIdFromDb =
              getNewsFilterByCategoryIdFromDb pool
        , nchGetNewsFilterByTitleFromDb = getNewsFilterByTitleFromDb pool
        , nchGetgetNewsFilterByAuthorNameFromDb =
              getNewsFilterByAuthorNameFromDb pool
        , nchGetNewsFilterByDateFromDb = getNewsFilterByDateFromDb pool
        , nchGetNewsFilterByTagAllFromDb = getNewsFilterByTagAllFromDb pool
        , nchGetNewsFilterByContentFromDb = getNewsFilterByContentFromDb pool
        , nchGetNewsFilterByAfterDateFromDb =
              getNewsFilterByAfterDateFromDb pool
        , nchGetNewsFilterByBeforeDateFromDb =
              getNewsFilterByBeforeDateFromDb pool
        , nchGetNewsFilterByTagIdFromDb = getNewsFilterByTagIdFromDb pool
        , nchGetNewsFromDb = getNewsFromDb pool
        , nchParseRequestBody = liftIO . parseRequestBody lbsBackEnd
        }

data TagsHandle m =
    TagsHandle
        { thCreateTagInDb     :: Maybe Token -> Maybe TagName -> m SendId
        , thDeleteTagFromDb   :: Maybe Token -> Maybe TagName -> m ()
        , thGetTagsListFromDb :: Maybe Page -> m TagsList
        , thEditTagInDb       :: Maybe Token -> EditTag -> m ()
        , thParseRequestBody  :: Request -> m ([Param], [File LBS.ByteString])
        }

tagsHandler ::
       (MonadIO m, MonadError SomeError m)
    => Pool Connection
    -> TokenLifeTime
    -> TagsHandle m
tagsHandler pool tokenLifeTime =
    TagsHandle
        { thCreateTagInDb = createTagInDb pool tokenLifeTime
        , thDeleteTagFromDb = deleteTagFromDb pool tokenLifeTime
        , thGetTagsListFromDb = getTagsListFromDb pool
        , thEditTagInDb = editTagInDb pool tokenLifeTime
        , thParseRequestBody = liftIO . parseRequestBody lbsBackEnd
        }

data UsersHandle m =
    UsersHandle
        { uhAuth             :: Maybe Login -> Maybe Password -> m Token
        , uhCreateUserInDb   :: CreateUser -> m Token
        , uhDeleteUserFromDb :: Maybe Token -> Maybe Login -> m ()
        , uhProfileOnDb      :: Maybe Token -> m Profile
        , uhParseRequestBody :: Request -> m ([Param], [File LBS.ByteString])
        }

usersHandler ::
       (MonadIO m, MonadError SomeError m)
    => Pool Connection
    -> TokenLifeTime
    -> UsersHandle m
usersHandler pool tokenLifeTime =
    UsersHandle
        { uhAuth = authentication pool
        , uhParseRequestBody = liftIO . parseRequestBody lbsBackEnd
        , uhDeleteUserFromDb = deleteUserFromDb pool tokenLifeTime
        , uhProfileOnDb = profileOnDb pool tokenLifeTime
        , uhCreateUserInDb = createUserInDb pool
        }

data OperationsHandle m =
    OperationsHandle
        { imagesHandle          :: ImagesHandle m
        , categoriesHandle      :: CategoriesHandle m
        , authorsHandle         :: AuthorsHandle m
        , tagsHandle            :: TagsHandle m
        , usersHandle           :: UsersHandle m
        , draftsHandle          :: DraftsHandle m
        , newsAndCommentsHandle :: NewsAndCommentsHandle m
        }

operationsHandler ::
       (MonadIO m, MonadError SomeError m)
    => Pool Connection
    -> TokenLifeTime
    -> OperationsHandle m
operationsHandler pool tokenLifeTime =
    OperationsHandle
        { imagesHandle = imagesHandler pool
        , categoriesHandle = categoriesHandler pool tokenLifeTime
        , authorsHandle = authorsHandler pool tokenLifeTime
        , draftsHandle = draftsHandler pool tokenLifeTime
        , newsAndCommentsHandle = newsAndCommentsHandler pool tokenLifeTime
        , tagsHandle = tagsHandler pool tokenLifeTime
        , usersHandle = usersHandler pool tokenLifeTime
        }
