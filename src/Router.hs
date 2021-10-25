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
                                              Request (rawPathInfo), Response,
                                              ResponseReceived)
import           OperationsHandle            (OperationsHandle (authors_handle, categories_handle, drafts_handle, images_handle, news_and_comments_handle, tags_handle, users_handle))
import           Responses                   (responseNotFound)

routes' :: MonadIO m => OperationsHandle m -> Request -> m Response
routes' operations req =
    case pathHead of
        "news" ->
            newsAndCommentsRouter (news_and_comments_handle operations) req
        "login" -> login (users_handle operations) req
        "registration" -> registration (users_handle operations) req
        "deleteUser" -> deleteUser (users_handle operations) req
        "categories" -> categoriesRouter (categories_handle operations) req
        "profile" -> profile (users_handle operations) req
        "drafts" -> draftsRouter (drafts_handle operations) req
        "new_draft" -> createDraft (drafts_handle operations) req
        "tags" -> tagsRouter (tags_handle operations) req
        "image" -> imagesRouter (images_handle operations) req
        "authors" -> authorsRouter (authors_handle operations) req
        _ -> return $ responseNotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathHead = head pathElems

routes'' :: OperationsHandle IO -> Application
routes'' operations req respond = do
    resp <- routes' operations req
    respond resp

routes :: MonadIO m => OperationsHandle m -> Request -> (Response -> m a) -> m a
routes operations request respond = routes' operations request >>= respond
