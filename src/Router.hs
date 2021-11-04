{-# LANGUAGE OverloadedStrings #-}

module Router where

import           Controllers.Authors         (authorsRouter)
import           Controllers.Categories      (categoriesRouter)
import           Controllers.Drafts          (draftsRouter, postDraft)
import           Controllers.Images          (imagesRouter)
import           Controllers.NewsAndComments (newsAndCommentsRouter)
import           Controllers.Tags            (tagsRouter)
import           Controllers.Users           (deleteUser, profile, registration,
                                              signIn)
import qualified Data.ByteString.Char8       as BC
import           Network.Wai                 (Request (rawPathInfo), Response)
import           OperationsHandle            (OperationsHandle (authorsHandle, categoriesHandle, draftsHandle, imagesHandle, newsAndCommentsHandle, tagsHandle, usersHandle))
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
        "news" -> newsAndCommentsRouter (newsAndCommentsHandle operations) req
        "login" -> signIn (usersHandle operations) req
        "registration" -> registration (usersHandle operations) req
        "deleteUser" -> deleteUser (usersHandle operations) req
        "categories" -> categoriesRouter (categoriesHandle operations) req
        "profile" -> profile (usersHandle operations) req
        "drafts" -> draftsRouter (draftsHandle operations) req
        "new_draft" -> postDraft (draftsHandle operations) req
        "tags" -> tagsRouter (tagsHandle operations) req
        "image" -> imagesRouter (imagesHandle operations) req
        "authors" -> authorsRouter (authorsHandle operations) req
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
