{-# LANGUAGE OverloadedStrings #-}

module Router where

import Control.Monad.IO.Class (MonadIO)
import Controllers.Authors (authorsRouter)
import Controllers.Categories (categoriesRouter)
import Controllers.Drafts (createDraft, draftsRouter)
import Controllers.Images (imagesRouter)
import Controllers.NewsAndComments (newsAndCommentsRouter)
import Controllers.Tags (tagsRouter)
import Controllers.Users (deleteUser, login, profile, registration)
import qualified Data.ByteString.Char8 as BC
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Logger (Handle)
import Network.Wai (Application, Request(rawPathInfo), Response)
import OperationsHandle
    ( OperationsHandle(authors_handle, categories_handle, drafts_handle,
                 images_handle, news_and_comments_handle, tags_handle, users_handle)
    )
import Responses (responseNotFound)
import Types (TokenLifeTime)

routes' ::
       MonadIO m
    => Handle m
    -> TokenLifeTime
    -> Pool Connection
    -> OperationsHandle m
    -> Request
    -> m Response
routes' hLogger token_lifetime pool operations req = do
    case pathHead of
        "news" ->
            newsAndCommentsRouter
                hLogger
                (news_and_comments_handle operations)
                pool
                token_lifetime
                req
        "login" -> login hLogger (users_handle operations) pool req
        "registration" ->
            registration hLogger (users_handle operations) pool req
        "deleteUser" ->
            deleteUser hLogger (users_handle operations) pool token_lifetime req
        "categories" ->
            categoriesRouter
                hLogger
                (categories_handle operations)
                pool
                token_lifetime
                req
        "profile" ->
            profile hLogger (users_handle operations) pool token_lifetime req
        "drafts" ->
            draftsRouter
                hLogger
                (drafts_handle operations)
                pool
                token_lifetime
                req
        "new_draft" ->
            createDraft
                hLogger
                (drafts_handle operations)
                pool
                token_lifetime
                req
        "tags" ->
            tagsRouter hLogger (tags_handle operations) pool token_lifetime req
        "image" -> imagesRouter hLogger (images_handle operations) pool req
        "authors" ->
            authorsRouter
                hLogger
                (authors_handle operations)
                pool
                token_lifetime
                req
        _ -> return $ responseNotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathHead = head pathElems

routes ::
       Handle IO
    -> TokenLifeTime
    -> Pool Connection
    -> OperationsHandle IO
    -> Application
routes hLogger token_lifetime pool operations req respond =
    routes' hLogger token_lifetime pool operations req >>= respond
