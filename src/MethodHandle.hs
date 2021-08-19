module MethodHandle where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Logger (Handle)
import Network.Wai (Request, Response)
import Types (TokenLifeTime)

data MethodHandle =
    MethodHandle
        { news_and_comments_handler :: Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
        , login_handler :: Handle -> Pool Connection -> Request -> IO Response
        , registration_handler :: Handle -> Pool Connection -> Request -> IO Response
        , delete_user_handler :: Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
        , categories_handler :: Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
        , profile_handler :: Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
        , draft_handler :: Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
        , new_draft_handler :: Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
        , tags_handler :: Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
        }
