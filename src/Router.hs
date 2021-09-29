{-# LANGUAGE OverloadedStrings #-}

module Router where

import Config (ConfigModules(idle_time, max_resources, num_stripes))
import Control.Monad.IO.Class
import Controllers.Authors (authorsRouter)
import Controllers.Categories (categoriesRouter)
import Controllers.Drafts (createDraft, draftsRouter)
import Controllers.Images (imagesRouter)
import Controllers.InitDb (initDb)
import Controllers.NewsAndComments (newsAndCommentsRouter)
import Controllers.Tags (tagsRouter)
import Controllers.Users (deleteUser, login, profile, registration)
import qualified Data.ByteString.Char8 as BC
import Data.Pool (createPool)
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Logger (Handle)
import Network.Wai
import OperationsHandle
    ( OperationsHandle(authors_handle, categories_handle, drafts_handle,
                 images_handle, init_db_handle, news_and_comments_handle,
                 tags_handle, users_handle)
    )
import Responses (responseNotFound)
import Types (DatabaseAddress, TokenLifeTime)

{-routes ::
       Handle IO
    -> DatabaseAddress
    -> DatabaseAddress
    -> TokenLifeTime
    -> ConfigModules
    -> OperationsHandle IO
    -> Application
routes hLogger db_address db_server_address token_lifetime confPool operations req respond = do
    pool <-
        createPool
            (connectPostgreSQL db_address)
            close
            (num_stripes confPool)
            (idle_time confPool)
            (max_resources confPool)
    case pathHead of
        "news" ->
            newsAndCommentsRouter
                hLogger
                (news_and_comments_handle operations)
                pool
                token_lifetime
                req >>=
            respond
        "login" -> login hLogger (users_handle operations) pool req >>= respond
        "registration" ->
            registration hLogger (users_handle operations) pool req >>= respond
        "deleteUser" ->
            deleteUser hLogger (users_handle operations) pool token_lifetime req >>=
            respond
        "categories" ->
            categoriesRouter
                hLogger
                (categories_handle operations)
                pool
                token_lifetime
                req >>=
            respond
        "profile" ->
            profile hLogger (users_handle operations) pool token_lifetime req >>=
            respond
        "drafts" ->
            draftsRouter
                hLogger
                (drafts_handle operations)
                pool
                token_lifetime
                req >>=
            respond
        "new_draft" ->
            createDraft
                hLogger
                (drafts_handle operations)
                pool
                token_lifetime
                req >>=
            respond
        "tags" ->
            tagsRouter hLogger (tags_handle operations) pool token_lifetime req >>=
            respond
        "image" ->
            imagesRouter hLogger (images_handle operations) pool req >>= respond
        "initDb" ->
            initDb
                hLogger
                (init_db_handle operations)
                pool
                db_server_address
                req >>=
            respond
        "authors" ->
            authorsRouter
                hLogger
                (authors_handle operations)
                pool
                token_lifetime
                req >>=
            respond
        _ -> respond $ responseNotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathHead = head pathElems


-------------------------------}
routes' ::
       MonadIO m
    => Handle m
    -> DatabaseAddress
    -> DatabaseAddress
    -> TokenLifeTime
    -> ConfigModules
    -> OperationsHandle m
    -> Request
    -> m Response
routes' hLogger db_address db_server_address token_lifetime confPool operations req = do
    pool <-
        liftIO $
        createPool
            (connectPostgreSQL db_address)
            close
            (num_stripes confPool)
            (idle_time confPool)
            (max_resources confPool)
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
        "initDb" ->
            initDb
                hLogger
                (init_db_handle operations)
                pool
                db_server_address
                req
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
    -> DatabaseAddress
    -> DatabaseAddress
    -> TokenLifeTime
    -> ConfigModules
    -> OperationsHandle IO
    -> Application
routes hLogger db_address db_server_address token_lifetime confPool operations req respond = do
    resp <-
        routes'
            hLogger
            db_address
            db_server_address
            token_lifetime
            confPool
            operations
            req
    respond resp
