{-# LANGUAGE OverloadedStrings #-}

module Router where

import           Controllers.Authors         (authorsRouter)
import           Controllers.Categories      (categoriesRouter)
import           Controllers.Drafts          (draftsRouter, postDraft)
import           Controllers.Images          (imagesRouter)
import           Controllers.NewsAndComments (newsAndCommentsRouter)
import           Controllers.Tags            (tagsRouter)
import           Controllers.Users           (deleteUser, login, profile,
                                              registration)
import qualified Data.ByteString.Char8       as BC
import           Network.Wai                 (Request (rawPathInfo), Response)
import           OperationsHandle            (OperationsHandle (authors_handle, categories_handle, drafts_handle, images_handle, news_and_comments_handle, tags_handle, users_handle))
import           Responses                   (toResponse)
import           Types.Other                 (ResponseErrorMessage (NotFound),
                                              ResponseOkMessage)

routes ::
       Monad m
    => OperationsHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
routes operations req =
    case pathHead of
        "news" ->
            newsAndCommentsRouter (news_and_comments_handle operations) req
        "login" -> login (users_handle operations) req
        "registration" -> registration (users_handle operations) req
        "deleteUser" -> deleteUser (users_handle operations) req
        "categories" -> categoriesRouter (categories_handle operations) req
        "profile" -> profile (users_handle operations) req
        "drafts" -> draftsRouter (drafts_handle operations) req
        "new_draft" -> postDraft (drafts_handle operations) req
        "tags" -> tagsRouter (tags_handle operations) req
        "image" -> imagesRouter (images_handle operations) req
        "authors" -> authorsRouter (authors_handle operations) req
        _ -> return $ Left $ NotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathHead = head pathElems

responder ::
       Monad m => Either ResponseErrorMessage ResponseOkMessage -> m Response
responder something = return $ toResponse something

application ::
       Monad m => OperationsHandle m -> Request -> (Response -> m b) -> m b
application operations req respond =
    routes operations req >>= responder >>= respond
