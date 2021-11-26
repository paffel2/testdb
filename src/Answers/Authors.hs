{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Answers.Authors where

import           Answer                    (AnswerHandle (..))
import           Control.Monad.Except      (MonadError (throwError))
import           FromRequest               (takeToken, toAuthorLogin,
                                            toCreateAuthor, toEditAuthor,
                                            toPage)
import           Network.HTTP.Types.Method (methodDelete, methodGet, methodPost,
                                            methodPut)
import           Network.Wai               (Request (requestMethod))
import           OperationsHandle          (AuthorsHandle (ahCreateAuthorInDb, ahDeleteAuthorInDb, ahEditAuthorInDb, ahGetAuthorsList, ahParseRequestBody))
import           Types.Authors             (AuthorLogin, AuthorsList,
                                            CreateAuthor, EditAuthor)
import           Types.Other               (MonadIOWithError, Page, SendId,
                                            SomeError (BadMethod), Token)

-----------------------------------------------------------------------------------------------------------------------
authorsListParseInformation ::
       MonadIOWithError m => AuthorsHandle m -> Request -> m (Maybe Page)
authorsListParseInformation _ request =
    if requestMethod request /= methodGet
        then throwError BadMethod
        else return (toPage request)

authorsListDatabaseOperation ::
       MonadIOWithError m => AuthorsHandle m -> Maybe Page -> m AuthorsList
authorsListDatabaseOperation = ahGetAuthorsList

authorsListHandle ::
       MonadIOWithError m
    => AuthorsHandle m
    -> AnswerHandle m (Maybe Page) AuthorsList
authorsListHandle authorHandle =
    AnswerHandle
        { parseInformation = authorsListParseInformation authorHandle
        , databaseOperation = authorsListDatabaseOperation authorHandle
        }

-----------------------------------------------------------------------------------------------------------------
updateAuthorParseInformation ::
       MonadIOWithError m
    => AuthorsHandle m
    -> Request
    -> m (Maybe Token, EditAuthor)
updateAuthorParseInformation handler request =
    if requestMethod request /= methodPut
        then throwError BadMethod
        else do
            let token = takeToken request
            (i, _) <- ahParseRequestBody handler request
            let editInfo = toEditAuthor i
            return (token, editInfo)

updateAuthorDatabaseOperation ::
       MonadIOWithError m
    => AuthorsHandle m
    -> (Maybe Token, EditAuthor)
    -> m ()
updateAuthorDatabaseOperation authorHandle (token, editInfo) =
    ahEditAuthorInDb authorHandle token editInfo

updateAuthorHandle ::
       MonadIOWithError m
    => AuthorsHandle m
    -> AnswerHandle m (Maybe Token, EditAuthor) ()
updateAuthorHandle authorHandle =
    AnswerHandle
        { parseInformation = updateAuthorParseInformation authorHandle
        , databaseOperation = updateAuthorDatabaseOperation authorHandle
        }

----------------------------------------------------------------------------------------------------
deleteAuthorParseInformation ::
       MonadIOWithError m
    => AuthorsHandle m
    -> Request
    -> m (Maybe Token, Maybe AuthorLogin)
deleteAuthorParseInformation handler request =
    if requestMethod request /= methodDelete
        then throwError BadMethod
        else do
            let token = takeToken request
            (i, _) <- ahParseRequestBody handler request
            let authorLogin = toAuthorLogin i
            return (token, authorLogin)

deleteAuthorDatabaseOperation ::
       MonadIOWithError m
    => AuthorsHandle m
    -> (Maybe Token, Maybe AuthorLogin)
    -> m ()
deleteAuthorDatabaseOperation authorHandle (token, authorLogin) =
    ahDeleteAuthorInDb authorHandle token authorLogin

deleteAuthorHandle ::
       MonadIOWithError m
    => AuthorsHandle m
    -> AnswerHandle m (Maybe Token, Maybe AuthorLogin) ()
deleteAuthorHandle authorHandle =
    AnswerHandle
        { parseInformation = deleteAuthorParseInformation authorHandle
        , databaseOperation = deleteAuthorDatabaseOperation authorHandle
        }

--------------------------------------------------------------------------------
createAuthorParseInformation ::
       MonadIOWithError m
    => AuthorsHandle m
    -> Request
    -> m (Maybe Token, CreateAuthor)
createAuthorParseInformation handler request =
    if requestMethod request /= methodPost
        then throwError BadMethod
        else do
            (i, _) <- ahParseRequestBody handler request
            let token = takeToken request
            let createAuthorParams = toCreateAuthor i
            return (token, createAuthorParams)

createAuthorDatabaseOperation ::
       MonadIOWithError m
    => AuthorsHandle m
    -> (Maybe Token, CreateAuthor)
    -> m SendId
createAuthorDatabaseOperation authorHandle (token, craeteInfo) =
    ahCreateAuthorInDb authorHandle token craeteInfo

createAuthorHandle ::
       MonadIOWithError m
    => AuthorsHandle m
    -> AnswerHandle m (Maybe Token, CreateAuthor) SendId
createAuthorHandle authorHandle =
    AnswerHandle
        { parseInformation = createAuthorParseInformation authorHandle
        , databaseOperation = createAuthorDatabaseOperation authorHandle
        }
