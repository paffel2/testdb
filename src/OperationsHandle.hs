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
        { ahCreateAuthorInDb :: Maybe Token -> CreateAuthor -> m (Either SomeError SendId)
        , ahDeleteAuthorInDb :: Maybe Token -> Maybe AuthorLogin -> m (Either SomeError ())
        , ahGetAuthorsList :: Maybe Page -> m (Either SomeError AuthorsList)
        , ahEditAuthorInDb :: Maybe Token -> EditAuthor -> m (Either SomeError ())
        , ahLogger :: LoggerHandle m
        , ahParseRequestBody :: Request -> m ([Param], [File LBS.ByteString])
        }

authorsHandler ::
       Pool Connection -> LoggerHandle IO -> TokenLifeTime -> AuthorsHandle IO
authorsHandler pool hLogger tokenLifeTime =
    AuthorsHandle
        { ahCreateAuthorInDb = createAuthorInDb pool tokenLifeTime hLogger
        , ahDeleteAuthorInDb = deleteAuthorInDb pool tokenLifeTime hLogger
        , ahGetAuthorsList = getAuthorsList pool hLogger
        , ahEditAuthorInDb = editAuthorInDb pool tokenLifeTime hLogger
        , ahLogger = hLogger
        , ahParseRequestBody = parseRequestBody lbsBackEnd
        }

data CategoriesHandle m =
    CategoriesHandle
        { chGetCategoriesListFromDb :: Maybe Page -> m (Either SomeError ListOfCategories)
        , chCreateCategoryOnDb :: Maybe Token -> CreateCategory -> m (Either SomeError SendId)
        , chDeleteCategoryFromDb :: Maybe Token -> Maybe CategoryName -> m (Either SomeError ())
        , chEditCategoryOnDb :: Maybe Token -> EditCategory -> m (Either SomeError ())
        , chLogger :: LoggerHandle m
        , chParseRequestBody :: Request -> m ([Param], [File LBS.ByteString])
        }

categoriesHandler ::
       Pool Connection
    -> LoggerHandle IO
    -> TokenLifeTime
    -> CategoriesHandle IO
categoriesHandler pool hLogger tokenLifeTime =
    CategoriesHandle
        { chGetCategoriesListFromDb = getCategoriesListFromDb pool hLogger
        , chCreateCategoryOnDb = createCategoryOnDb pool tokenLifeTime hLogger
        , chDeleteCategoryFromDb =
              deleteCategoryFromDb pool tokenLifeTime hLogger
        , chEditCategoryOnDb = editCategoryOnDb pool tokenLifeTime hLogger
        , chLogger = hLogger
        , chParseRequestBody = parseRequestBody lbsBackEnd
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
        { ihGetPhoto     :: Id -> m (Either SomeError ImageB)
        , ihGetPhotoList :: Maybe Page -> m (Either SomeError ImageArray)
        , ihLogger       :: LoggerHandle m
        }

imagesHandler :: Pool Connection -> LoggerHandle IO -> ImagesHandle IO
imagesHandler pool hLogger =
    ImagesHandle
        { ihGetPhoto = getPhotoFromDb pool hLogger
        , ihGetPhotoList = getPhotoListFromDb pool hLogger
        , ihLogger = hLogger
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
        { thCreateTagInDb :: Maybe Token -> Maybe TagName -> m (Either SomeError SendId)
        , thDeleteTagFromDb :: Maybe Token -> Maybe TagName -> m (Either SomeError ())
        , thGetTagsListFromDb :: Maybe Page -> m (Either SomeError TagsList)
        , thEditTagInDb :: Maybe Token -> EditTag -> m (Either SomeError ())
        , thLogger :: LoggerHandle m
        , thParseRequestBody :: Request -> m ([Param], [File LBS.ByteString])
        }

tagsHandler ::
       Pool Connection -> LoggerHandle IO -> TokenLifeTime -> TagsHandle IO
tagsHandler pool hLogger tokenLifeTime =
    TagsHandle
        { thCreateTagInDb = createTagInDb pool tokenLifeTime hLogger
        , thDeleteTagFromDb = deleteTagFromDb pool tokenLifeTime hLogger
        , thGetTagsListFromDb = getTagsListFromDb pool hLogger
        , thEditTagInDb = editTagInDb pool tokenLifeTime hLogger
        , thLogger = hLogger
        , thParseRequestBody = parseRequestBody lbsBackEnd
        }

{-data UsersHandle m =
    UsersHandle
        { uhAuth :: Maybe Login -> Maybe Password -> m (Either SomeError Token)
        , uhCreateUserInDb :: CreateUser -> m (Either SomeError Token)
        , uhDeleteUserFromDb :: Maybe Token -> Maybe Login -> m (Either SomeError ())
        , uhProfileOnDb :: Maybe Token -> m (Either SomeError Profile)
        , uhLogger :: LoggerHandle m
        , uhParseRequestBody :: Request -> m ([Param], [File LBS.ByteString])
        } -}
{-usersHandler ::
       Pool Connection -> LoggerHandle IO -> TokenLifeTime -> UsersHandle IO
usersHandler pool hLogger tokenLifeTime =
    UsersHandle
        { uhAuth = authentication pool hLogger
        , uhCreateUserInDb = createUserInDb pool hLogger
        , uhDeleteUserFromDb = deleteUserFromDb pool tokenLifeTime hLogger
        , uhProfileOnDb = profileOnDb pool tokenLifeTime hLogger
        , uhLogger = hLogger
        , uhParseRequestBody = parseRequestBody lbsBackEnd
        } -}
data UsersHandle m io =
    UsersHandle
        { uhAuth             :: Maybe Login -> Maybe Password -> m Token
        , uhCreateUserInDb   :: CreateUser -> m Token
        , uhDeleteUserFromDb :: Maybe Token -> Maybe Login -> m ()
        , uhProfileOnDb      :: Maybe Token -> m Profile
        , uhLogger           :: LoggerHandle io
        }

usersHandler ::
       (MonadIO m, MonadError SomeError m)
    => Pool Connection
    -> LoggerHandle IO
    -> TokenLifeTime
    -> UsersHandle m IO
usersHandler pool hLogger tokenLifeTime =
    UsersHandle
        { uhCreateUserInDb = createUserInDb pool hLogger
        , uhAuth = authentication pool hLogger
        , uhDeleteUserFromDb = deleteUserFromDb pool tokenLifeTime hLogger
        , uhProfileOnDb = profileOnDb pool tokenLifeTime hLogger
        , uhLogger = hLogger
        }

data OperationsHandle m io =
    OperationsHandle
          --authorsHandle         :: AuthorsHandle m
        --, categoriesHandle      :: CategoriesHandle m
        --, draftsHandle          :: DraftsHandle m
        --, imagesHandle          :: ImagesHandle m
        --, newsAndCommentsHandle :: NewsAndCommentsHandle m
        --, tagsHandle            :: TagsHandle m
        { usersHandle :: UsersHandle m io
        }

operationsHandler ::
       (MonadIO m, MonadError SomeError m)
    => LoggerHandle IO
    -> Pool Connection
    -> TokenLifeTime
    -> OperationsHandle m IO
operationsHandler hLogger pool tokenLifeTime =
    OperationsHandle
          --authorsHandle = authorsHandler pool hLogger tokenLifeTime
        --, categoriesHandle = categoriesHandler pool hLogger tokenLifeTime
        --, draftsHandle = draftsHandler pool hLogger tokenLifeTime
        --, imagesHandle = imagesHandler pool hLogger
        --, newsAndCommentsHandle =
        --      newsAndCommentsHandler pool hLogger tokenLifeTime
        --, tagsHandle = tagsHandler pool hLogger tokenLifeTime
        {usersHandle = usersHandler pool hLogger tokenLifeTime}
