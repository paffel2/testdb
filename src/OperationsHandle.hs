{-# LANGUAGE FlexibleContexts #-}

module OperationsHandle where

import           Control.Monad.Except
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
import           Databaseoperations.Users
import           Logger                             (LoggerHandle)
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

{-data OperationsHandle m =
    OperationsHandle
        { authorsHandle         :: AuthorsHandle m
        , categoriesHandle      :: CategoriesHandle m
        , draftsHandle          :: DraftsHandle m
        , imagesHandle          :: ImagesHandle m
        , newsAndCommentsHandle :: NewsAndCommentsHandle m
        , tagsHandle            :: TagsHandle m
        , usersHandle           :: UsersHandle m
        }

operationsHandler ::
       LoggerHandle IO
    -> Pool Connection
    -> TokenLifeTime
    -> OperationsHandle IO
operationsHandler hLogger pool tokenLifeTime =
    OperationsHandle
        { authorsHandle = authorsHandler pool hLogger tokenLifeTime
        , categoriesHandle = categoriesHandler pool hLogger tokenLifeTime
        , draftsHandle = draftsHandler pool hLogger tokenLifeTime
        , imagesHandle = imagesHandler pool hLogger
        , newsAndCommentsHandle =
              newsAndCommentsHandler pool hLogger tokenLifeTime
        , tagsHandle = tagsHandler pool hLogger tokenLifeTime
        , usersHandle = usersHandler pool hLogger tokenLifeTime
        } -}
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
        { dhGetDraftsByAuthorToken :: Maybe Token -> m (Either SomeError DraftArray)
        , dhDeleteDraftFromDb :: Maybe Token -> Maybe Id -> m (Either SomeError ())
        , dhGetDraftByIdFromDb :: Maybe Token -> Id -> m (Either SomeError Draft)
        , dhCreateDraftOnDb :: DraftInf -> Maybe DraftTags -> Maybe Image -> Maybe [Image] -> m (Either SomeError SendId)
        , dhUpdateDraftInDb :: DraftInf -> Maybe DraftTags -> Maybe Image -> Maybe [Image] -> Id -> m (Either SomeError ())
        , dhPublicNewsOnDb :: Maybe Token -> Id -> m (Either SomeError SendId)
        , dhLogger :: LoggerHandle m
        , dhParseRequestBody :: Request -> m ([Param], [File LBS.ByteString])
        }

draftsHandler ::
       Pool Connection -> LoggerHandle IO -> TokenLifeTime -> DraftsHandle IO
draftsHandler pool hLogger tokenLifeTime =
    DraftsHandle
        { dhGetDraftsByAuthorToken =
              getDraftsByAuthorToken pool tokenLifeTime hLogger
        , dhDeleteDraftFromDb = deleteDraftFromDb pool tokenLifeTime hLogger
        , dhGetDraftByIdFromDb = getDraftByIdFromDb pool tokenLifeTime hLogger
        , dhCreateDraftOnDb = createDraftOnDb pool tokenLifeTime hLogger
        , dhUpdateDraftInDb = updateDraftInDb pool tokenLifeTime hLogger
        , dhPublicNewsOnDb = publicNewsOnDb pool tokenLifeTime hLogger
        , dhLogger = hLogger
        , dhParseRequestBody = parseRequestBody lbsBackEnd
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
        { nchAddCommentToDb :: CommentWithoutTokenLifeTime -> m (Either SomeError ())
        , nchDeleteCommentFromDb :: Maybe Token -> Maybe Id -> m (Either SomeError ())
        , nchGetCommentsByNewsIdFromDb :: Maybe Id -> Maybe Page -> m (Either SomeError CommentArray)
        , nchGetNewsByIdFromDb :: Maybe Id -> m (Either SomeError GetNews)
        , nchGetNewsFilterByTagInFromDb :: Maybe TagInFilterParam -> Maybe Page -> m (Either SomeError NewsArray)
        , nchGetNewsFilterByCategoryIdFromDb :: Maybe CategoryFilterParam -> Maybe Page -> Sort -> m (Either SomeError NewsArray)
        , nchGetNewsFilterByTitleFromDb :: Maybe TitleFilterParam -> Maybe Page -> Sort -> m (Either SomeError NewsArray)
        , nchGetgetNewsFilterByAuthorNameFromDb :: Maybe AuthorFilterParam -> Maybe Page -> Sort -> m (Either SomeError NewsArray)
        , nchGetNewsFilterByDateFromDb :: Maybe DateFilterParam -> Maybe Page -> Sort -> m (Either SomeError NewsArray)
        , nchGetNewsFilterByTagAllFromDb :: Maybe TagAllFilterParam -> Maybe Page -> Sort -> m (Either SomeError NewsArray)
        , nchGetNewsFilterByContentFromDb :: Maybe ContentFilterParam -> Maybe Page -> Sort -> m (Either SomeError NewsArray)
        , nchGetNewsFilterByAfterDateFromDb :: Maybe AfterDateFilterParam -> Maybe Page -> Sort -> m (Either SomeError NewsArray)
        , nchGetNewsFilterByBeforeDateFromDb :: Maybe BeforeDateFilterParam -> Maybe Page -> Sort -> m (Either SomeError NewsArray)
        , nchGetNewsFilterByTagIdFromDb :: Maybe TagFilterParam -> Maybe Page -> Sort -> m (Either SomeError NewsArray)
        , nchGetNewsFromDb :: Sort -> Maybe Page -> m (Either SomeError NewsArray)
        , nchLogger :: LoggerHandle m
        , nchParseRequestBody :: Request -> m ([Param], [File LBS.ByteString])
        }

newsAndCommentsHandler ::
       Pool Connection
    -> LoggerHandle IO
    -> TokenLifeTime
    -> NewsAndCommentsHandle IO
newsAndCommentsHandler pool hLogger tokenLifeTime =
    NewsAndCommentsHandle
        { nchAddCommentToDb = addCommentToDb pool tokenLifeTime hLogger
        , nchDeleteCommentFromDb =
              deleteCommentFromDb pool tokenLifeTime hLogger
        , nchGetCommentsByNewsIdFromDb = getCommentsByNewsIdFromDb pool hLogger
        , nchGetNewsByIdFromDb = getNewsByIdFromDb pool hLogger
        , nchGetNewsFilterByTagInFromDb =
              getNewsFilterByTagInFromDb pool hLogger
        , nchGetNewsFilterByCategoryIdFromDb =
              getNewsFilterByCategoryIdFromDb pool hLogger
        , nchGetNewsFilterByTitleFromDb =
              getNewsFilterByTitleFromDb pool hLogger
        , nchGetgetNewsFilterByAuthorNameFromDb =
              getNewsFilterByAuthorNameFromDb pool hLogger
        , nchGetNewsFilterByDateFromDb = getNewsFilterByDateFromDb pool hLogger
        , nchGetNewsFilterByTagAllFromDb =
              getNewsFilterByTagAllFromDb pool hLogger
        , nchGetNewsFilterByContentFromDb =
              getNewsFilterByContentFromDb pool hLogger
        , nchGetNewsFilterByAfterDateFromDb =
              getNewsFilterByAfterDateFromDb pool hLogger
        , nchGetNewsFilterByBeforeDateFromDb =
              getNewsFilterByBeforeDateFromDb pool hLogger
        , nchGetNewsFilterByTagIdFromDb =
              getNewsFilterByTagIdFromDb pool hLogger
        , nchGetNewsFromDb = getNewsFromDb pool hLogger
        , nchLogger = hLogger
        , nchParseRequestBody = parseRequestBody lbsBackEnd
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
          --authorsHandle         :: AuthorsHandle m
        --, categoriesHandle      :: CategoriesHandle m
        --, draftsHandle          :: DraftsHandle m
        { imagesHandle     :: ImagesHandle m
        , categoriesHandle :: CategoriesHandle m
        , authorsHandle    :: AuthorsHandle m
        , tagsHandle       :: TagsHandle m
        , usersHandle      :: UsersHandle m
        }

operationsHandler ::
       (MonadIO m, MonadError SomeError m)
    => LoggerHandle IO
    -> Pool Connection
    -> TokenLifeTime
    -> OperationsHandle m
operationsHandler _ pool tokenLifeTime =
    OperationsHandle
        --, categoriesHandle = categoriesHandler pool hLogger tokenLifeTime
        --, draftsHandle = draftsHandler pool hLogger tokenLifeTime
        { imagesHandle = imagesHandler pool
        , categoriesHandle = categoriesHandler pool tokenLifeTime
        , authorsHandle = authorsHandler pool tokenLifeTime
        --, newsAndCommentsHandle =
        --      newsAndCommentsHandler pool hLogger tokenLifeTime
        , tagsHandle = tagsHandler pool tokenLifeTime
        , usersHandle = usersHandler pool tokenLifeTime
        }
