{-# LANGUAGE OverloadedStrings #-}

module Router where

import Categories (categoriesBlock)
import qualified Data.ByteString.Char8 as BC
import Data.Pool (createPool)
import Database.PostgreSQL.Simple (close, connectPostgreSQL)
import Drafts (createDraft, draftsBlock)
import Logger (Handle)
import Network.Wai (Application, Request(rawPathInfo))
import NewsAndComments (newsMethodBlock)
import Responses (responseBadRequest)
import Tags (tagsBlock)
import Types (DatabaseAddress, TokenLifeTime)
import Users (deleteUser, login, profile, registration)

routes :: Handle -> DatabaseAddress -> TokenLifeTime -> Application
routes hLogger db_address token_lifetime req respond = do
    pool <- createPool (connectPostgreSQL db_address) close 1 5 10
    case pathHead of
        "news" ->
            newsMethodBlock hLogger pool token_lifetime pathElems req >>=
            respond
        "login" -> login hLogger pool req >>= respond
        "registration" -> registration hLogger pool req >>= respond
        "deleteUser" -> deleteUser hLogger pool token_lifetime req >>= respond
        "categories" ->
            categoriesBlock hLogger pool token_lifetime pathElems req >>=
            respond
        "profile" -> profile hLogger pool token_lifetime req >>= respond
        "drafts" ->
            draftsBlock hLogger pool token_lifetime pathElems req >>= respond
        "new_draft" -> createDraft hLogger pool token_lifetime req >>= respond
        "tags" ->
            tagsBlock hLogger pool token_lifetime pathElems req >>= respond
        _ -> badUrlRespond
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathHead = head pathElems
    badUrlRespond
            --logError hLogger "Bad url"
     = do
        respond $ responseBadRequest "bad url"
