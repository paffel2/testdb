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

data ControllersHandle m =
    ControllersHandle
        { news_and_comments_handler :: Handle m -> Pool Connection -> TokenLifeTime -> Request -> m Response
        , login_handler :: Handle m -> Pool Connection -> Request -> m Response
        , registration_handler :: Handle m -> Pool Connection -> Request -> m Response
        , delete_user_handler :: Handle m -> Pool Connection -> TokenLifeTime -> Request -> m Response
        , categories_handler :: Handle m -> Pool Connection -> TokenLifeTime -> Request -> m Response
        , profile_handler :: Handle m -> Pool Connection -> TokenLifeTime -> Request -> m Response
        , draft_handler :: Handle m -> Pool Connection -> TokenLifeTime -> Request -> m Response
        , new_draft_handler :: Handle m -> Pool Connection -> TokenLifeTime -> Request -> m Response
        , tags_handler :: Handle m -> Pool Connection -> TokenLifeTime -> Request -> m Response
        , image_handler :: Handle m -> Pool Connection -> Request -> m Response
        , initDb_handler :: Handle m -> Pool Connection -> DatabaseAddress -> Request -> m Response
        , authors_hanlder :: Handle m -> Pool Connection -> TokenLifeTime -> Request -> m Response
        }

handler :: ControllersHandle IO
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
        , authors_hanlder = authorsBlock
        }
