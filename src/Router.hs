{-# LANGUAGE OverloadedStrings #-}

module Router where

import ControllersHandle

import qualified Data.ByteString.Char8 as BC
import Data.Pool (createPool)
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Logger (Handle)
import Network.Wai (Application, Request(rawPathInfo))
import Responses (responseBadRequest)
import Types (DatabaseAddress, TokenLifeTime)
--import Controllers.InitDb

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
        "initDb" -> initDb_handler methods hLogger pool db_server_address req >>= respond
        _ -> badUrlRespond
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathHead = head pathElems
    badUrlRespond = do
        respond $ responseBadRequest "bad url"
