module ControllersHandle where

import Controllers.Authors (authorsBlock)
import Controllers.Categories (categoriesBlock)
import Controllers.Drafts (createDraft, draftsBlock)
import Controllers.Images (imageBlock)
import Controllers.InitDb (initDbBlock)
import Controllers.NewsAndComments (newsMethodBlock)
import Controllers.Tags (tagsBlock)
import Controllers.Users (deleteUser, login, profile, registration)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Logger (Handle)
import Network.Wai (Request, Response)
import Types (DatabaseAddress, TokenLifeTime)

data ControllersHandle =
    ControllersHandle
        { news_and_comments_handler :: Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
        , login_handler :: Handle -> Pool Connection -> Request -> IO Response
        , registration_handler :: Handle -> Pool Connection -> Request -> IO Response
        , delete_user_handler :: Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
        , categories_handler :: Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
        , profile_handler :: Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
        , draft_handler :: Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
        , new_draft_handler :: Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
        , tags_handler :: Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
        , image_handler :: Handle -> Pool Connection -> Request -> IO Response
        , initDb_handler :: Handle -> Pool Connection -> DatabaseAddress -> Request -> IO Response
        --, new_author_handler :: Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
        --, delete_author_handler :: Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
        , authors_hanlder :: Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response
        }

handler :: ControllersHandle
handler =
    ControllersHandle
        { news_and_comments_handler = newsMethodBlock
        , login_handler = login
        , registration_handler = registration
        , delete_user_handler = deleteUser
        , categories_handler = categoriesBlock
        , profile_handler = profile
        , draft_handler = draftsBlock
        , new_draft_handler = createDraft
        , tags_handler = tagsBlock
        , image_handler = imageBlock
        , initDb_handler = initDbBlock
        --, new_author_handler = newAuthor
        --, delete_author_handler = deleteAuthor
        , authors_hanlder = authorsBlock
        }
