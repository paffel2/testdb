{-# LANGUAGE OverloadedStrings #-}

module Router where

import           Control.Monad.Except        (ExceptT, MonadIO)
import           Controllers.Authors         (authorsRouter)
import           Controllers.Categories      (categoriesRouter)
import           Controllers.Drafts          (createDraft, draftsRouter)
import           Controllers.Images          (imagesRouter)
import           Controllers.NewsAndComments (newsAndCommentsRouter)
import           Controllers.Tags            (tagsRouter)
import           Controllers.Users           (deleteUser, profile, registration,
                                              signIn)
import qualified Data.ByteString.Char8       as BC
import           Logger                      (LoggerHandle)
import           Network.Wai                 (Request (rawPathInfo), Response)
import           OperationsHandle            (OperationsHandle (authorsHandle, categoriesHandle, draftsHandle, imagesHandle, newsAndCommentsHandle, tagsHandle, usersHandle))
import           Responses                   (toResponse)
import           Types.Other                 (ResponseErrorMessage (NotFound),
                                              ResponseOkMessage, SomeError)

routes ::
       MonadIO m
    => OperationsHandle (ExceptT SomeError m)
    -> LoggerHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
routes operations hLogger req =
    case pathHead of
        "news" ->
            newsAndCommentsRouter (newsAndCommentsHandle operations) hLogger req
        "login" -> signIn (usersHandle operations) hLogger req
        "registration" -> registration (usersHandle operations) hLogger req
        "deleteUser" -> deleteUser (usersHandle operations) hLogger req
        "categories" ->
            categoriesRouter (categoriesHandle operations) hLogger req
        "profile" -> profile (usersHandle operations) hLogger req
        "drafts" -> draftsRouter (draftsHandle operations) hLogger req
        "new_draft" -> createDraft (draftsHandle operations) hLogger req
        "tags" -> tagsRouter (tagsHandle operations) hLogger req
        "image" -> imagesRouter (imagesHandle operations) hLogger req
        "authors" -> authorsRouter (authorsHandle operations) hLogger req
        _ -> return $ Left $ NotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathHead = head pathElems

responder ::
       Monad m => Either ResponseErrorMessage ResponseOkMessage -> m Response
responder something = return $ toResponse something

application ::
       MonadIO m
    => OperationsHandle (ExceptT SomeError m)
    -> LoggerHandle m
    -> Request
    -> (Response -> m b)
    -> m b
application operations hLogger req respond =
    routes operations hLogger req >>= responder >>= respond
