{-# LANGUAGE OverloadedStrings #-}

module Router where

import           Control.Monad.IO.Class      (MonadIO (..))
import           Controllers.Authors         (authorsRouter)
import           Controllers.Categories      (categoriesRouter)
import           Controllers.Drafts          (createDraft, draftsRouter)
import           Controllers.Images          (imagesRouter)
import           Controllers.NewsAndComments (newsAndCommentsRouter)
import           Controllers.Tags            (tagsRouter)
import           Controllers.Users           (deleteUser, login, profile,
                                              registration)
import qualified Data.ByteString.Char8       as BC
import           Network.Wai                 (Application,
                                              Request (rawPathInfo), Response)
import           OperationsHandle            (OperationsHandle (authors_handle, categories_handle, drafts_handle, images_handle, logger_handle, news_and_comments_handle, tags_handle, users_handle))
import           Responses                   (responseNotFound)
import           Types.Other                 (TokenLifeTime)

routes' ::
       MonadIO m => TokenLifeTime -> OperationsHandle m -> Request -> m Response
routes' token_lifetime operations req =
    case pathHead of
        "news" ->
            newsAndCommentsRouter
                (logger_handle operations)
                (news_and_comments_handle operations)
                token_lifetime
                req
        "login" ->
            login (logger_handle operations) (users_handle operations) req
        "registration" ->
            registration
                (logger_handle operations)
                (users_handle operations)
                req
        "deleteUser" ->
            deleteUser
                (logger_handle operations)
                (users_handle operations)
                token_lifetime
                req
        "categories" ->
            categoriesRouter
                (logger_handle operations)
                (categories_handle operations)
                token_lifetime
                req
        "profile" ->
            profile
                (logger_handle operations)
                (users_handle operations)
                token_lifetime
                req
        "drafts" ->
            draftsRouter
                (logger_handle operations)
                (drafts_handle operations)
                token_lifetime
                req
        "new_draft" ->
            createDraft
                (logger_handle operations)
                (drafts_handle operations)
                token_lifetime
                req
        "tags" ->
            tagsRouter
                (logger_handle operations)
                (tags_handle operations)
                token_lifetime
                req
        "image" ->
            imagesRouter
                (logger_handle operations)
                (images_handle operations)
                req
        --"initDb" -> initDb hLogger (init_db_handle operations) pool req
        "authors" ->
            authorsRouter
                (logger_handle operations)
                (authors_handle operations)
                token_lifetime
                req
        _ -> return $ responseNotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathHead = head pathElems

routes :: TokenLifeTime -> OperationsHandle IO -> Application
routes token_lifetime operations req respond = do
    resp <- routes' token_lifetime operations req
    respond resp
