{-# LANGUAGE OverloadedStrings #-}

module Router where

import           Answer
import           Answers.Drafts
import           Answers.Users
import           Control.Monad.Except
import           Controllers.Authors         (authorsRouter)
import           Controllers.Categories      (categoriesRouter)
import           Controllers.Drafts
import           Controllers.Images          (imagesRouter)
import           Controllers.NewsAndComments (newsAndCommentsRouter)
import           Controllers.Tags            (tagsRouter)
import           Controllers.Users
import qualified Data.ByteString.Char8       as BC
import           Logger
import           Network.Wai                 (Request (rawPathInfo), Response)
import           OperationsHandle
import           Responses                   (toResponse)
import           Types.Other                 (ResponseErrorMessage (NotFound),
                                              ResponseOkMessage, SomeError)

{-routes ::
       Monad m
    => OperationsHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)-}
routes ::
       MonadIO m
    => OperationsHandle (ExceptT SomeError m)
    -> LoggerHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
routes operations hLogger req =
    case pathHead
        --"news" -> newsAndCommentsRouter (newsAndCommentsHandle operations) req
          of
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
