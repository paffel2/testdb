{-# LANGUAGE OverloadedStrings #-}

module Router where

import ControllersHandle
    ( ControllersHandle(authors_hanlder, categories_handler,
                  delete_user_handler, draft_handler, image_handler, initDb_handler,
                  login_handler, new_draft_handler, news_and_comments_handler,
                  profile_handler, registration_handler, tags_handler)
    )

import qualified Data.ByteString.Char8 as BC
import Data.Pool (createPool)
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Logger (Handle)
import Network.Wai (Application, Request(rawPathInfo))
import Responses ( responseNotFound )
import Types (DatabaseAddress, TokenLifeTime)

routes ::
       Handle
    -> DatabaseAddress
    -> DatabaseAddress
    -> TokenLifeTime
    -> ControllersHandle
    -> Application
routes hLogger db_address db_server_address token_lifetime methods req respond = do
    pool <- createPool (connectPostgreSQL db_address) close 1 10 10
    case pathHead of
        "news" ->
            news_and_comments_handler methods hLogger pool token_lifetime req >>=
            respond
        "login" -> login_handler methods hLogger pool req >>= respond
        "registration" ->
            registration_handler methods hLogger pool req >>= respond
        "deleteUser" ->
            delete_user_handler methods hLogger pool token_lifetime req >>=
            respond
        "categories" ->
            categories_handler methods hLogger pool token_lifetime req >>=
            respond
        "profile" ->
            profile_handler methods hLogger pool token_lifetime req >>= respond
        "drafts" ->
            draft_handler methods hLogger pool token_lifetime req >>= respond
        "new_draft" ->
            new_draft_handler methods hLogger pool token_lifetime req >>=
            respond
        "tags" ->
            tags_handler methods hLogger pool token_lifetime req >>= respond
        "image" -> image_handler methods hLogger pool req >>= respond
        "initDb" ->
            initDb_handler methods hLogger pool db_server_address req >>=
            respond
        "authors" ->
            authors_hanlder methods hLogger pool token_lifetime req >>= respond
        {-"new_author" ->
            new_author_handler methods hLogger pool token_lifetime req >>= respond
        "delete_author" -> 
            delete_author_handler methods hLogger pool token_lifetime req >>= respond-}
        _ -> respond $ responseNotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathHead = head pathElems
    --badUrlRespond = do
        --respond $ responseBadRequest "bad url"
