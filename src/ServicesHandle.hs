module ServicesHandle where

import Categories (categoriesBlock)
import Drafts (createDraft, draftsBlock)
import MethodHandle (MethodHandle(..))
import NewsAndComments (newsMethodBlock)
import Tags (tagsBlock)
import Users (deleteUser, login, profile, registration)

handler :: MethodHandle
handler =
    MethodHandle
        { news_and_comments_handler = newsMethodBlock
        , login_handler = login
        , registration_handler = registration
        , delete_user_handler = deleteUser
        , categories_handler = categoriesBlock
        , profile_handler = profile
        , draft_handler = draftsBlock
        , new_draft_handler = createDraft
        , tags_handler = tagsBlock
        }
