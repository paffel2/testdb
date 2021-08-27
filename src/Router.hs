{-# LANGUAGE OverloadedStrings #-}

module Router where

import qualified Data.ByteString.Char8 as BC
import Data.Pool (createPool)
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Images
import Logger (Handle)
import MethodHandle
    ( MethodHandle(categories_handler, delete_user_handler,
             draft_handler, image_handler, login_handler, new_draft_handler,
             news_and_comments_handler, profile_handler, registration_handler,
             tags_handler)
    )
import Network.Wai (Application, Request(rawPathInfo))
import Responses (responseBadRequest)
import Types (DatabaseAddress, TokenLifeTime)

routes ::
       Handle -> DatabaseAddress -> TokenLifeTime -> MethodHandle -> Application
routes hLogger db_address token_lifetime methods req respond = do
    pool <- createPool (connectPostgreSQL db_address) close 1 5 10
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
        _ -> badUrlRespond
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathHead = head pathElems
    badUrlRespond = do
        respond $ responseBadRequest "bad url"
