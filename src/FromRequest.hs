{-# LANGUAGE OverloadedStrings #-}

module FromRequest where

import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Time (getCurrentTime)
import Database.PostgreSQL.Simple.Types (Binary(Binary))
import HelpFunction
    ( readByteStringListInt
    , readByteStringToDay
    , readByteStringToId
    , readByteStringToInt
    )
import Network.Wai (Request(queryString))
import Network.Wai.Parse
    ( FileInfo(fileContent, fileContentType, fileName)
    , Param
    )
import Types.Authors
    ( AuthorLogin(AuthorLogin)
    , CreateAuthor(..)
    , EditAuthor(..)
    )
import Types.Categories
    ( CategoryName(CategoryName)
    , CreateCategory(..)
    , EditCategory(..)
    )
import Types.Drafts (DraftInf(..), DraftTags(DraftTags))
import Types.Images (Image(Image))
import Types.NewsAndComments
    ( AfterDateFilterParam(AfterDateFilterParam)
    , AuthorFilterParam(AuthorFilterParam)
    , BeforeDateFilterParam(BeforeDateFilterParam)
    , CategoryFilterParam(CategoryFilterParam)
    , CommentText(CommentText)
    , ContentFilterParam(ContentFilterParam)
    , DateFilterParam(DateFilterParam)
    , Sort(Sort)
    , TagAllFilterParam(TagAllFilterParam)
    , TagFilterParam(TagFilterParam)
    , TagInFilterParam(TagInFilterParam)
    , TitleFilterParam(TitleFilterParam)
    )
import Types.Other (Id, Page(Page), Token(Token))
import Types.Tags (EditTag(..), TagName(TagName))
import Types.Users (CreateUser(..), Login(Login), Password(Password))

takeToken :: Request -> Maybe Token
takeToken req =
    Token . E.decodeUtf8 <$>
    fromMaybe Nothing (lookup "token" $ queryString req)

toImage :: FileInfo LBS.ByteString -> Image
toImage file_info =
    Image
        (fileName file_info)
        (fileContentType file_info)
        (Binary $ fileContent file_info)

toPage :: Request -> Maybe Page
toPage req =
    Page <$>
    (fromMaybe Nothing (lookup "page" $ queryString req) >>= readByteStringToInt)

toEditAuthor :: [Param] -> EditAuthor
toEditAuthor params =
    EditAuthor
        { edit_author_description =
              E.decodeUtf8 <$> lookup "new_description" params
        , edit_author_id = lookup "author_id" params >>= readByteStringToInt
        }

toAuthorLogin :: [Param] -> Maybe AuthorLogin
toAuthorLogin params =
    AuthorLogin . E.decodeUtf8 <$> lookup "author_login" params

toCreateAuthor :: [Param] -> CreateAuthor
toCreateAuthor params =
    CreateAuthor
        { create_author_login = E.decodeUtf8 <$> lookup "author_login" params
        , create_author_description =
              E.decodeUtf8 <$> lookup "description" params
        }

toTagName :: Request -> Maybe TagName
toTagName req =
    TagName . T.toLower . E.decodeUtf8 <$>
    fromMaybe Nothing (lookup "tag_name" $ queryString req)

toEditTag :: [Param] -> EditTag
toEditTag params =
    EditTag
        { edit_tag_new_name =
              TagName . T.toLower . E.decodeUtf8 <$>
              lookup "new_tag_name" params
        , edit_tag_old_name =
              TagName . T.toLower . E.decodeUtf8 <$>
              lookup "old_tag_name" params
        }

toCategoryName :: [Param] -> Maybe CategoryName
toCategoryName params =
    CategoryName . T.toLower . E.decodeUtf8 <$> lookup "category_name" params

toCreateCategory :: [Param] -> CreateCategory
toCreateCategory params =
    CreateCategory
        { create_categrory_name =
              CategoryName . T.toLower . E.decodeUtf8 <$>
              lookup "category_name" params
        , create_categrory_maternal_category =
              CategoryName . T.toLower . E.decodeUtf8 <$>
              lookup "maternal_category_name" params
        }

toEditCategory :: [Param] -> EditCategory
toEditCategory params =
    EditCategory
        { edit_category_name =
              CategoryName . T.toLower . E.decodeUtf8 <$>
              lookup "category_name" params
        , edit_category_new_name =
              CategoryName . T.toLower . E.decodeUtf8 <$>
              lookup "new_name" params
        , edit_category_new_maternal =
              CategoryName . T.toLower . E.decodeUtf8 <$>
              lookup "new_maternal" params
        }

toPassword :: [Param] -> Maybe Password
toPassword params = Password . E.decodeUtf8 <$> lookup "user_password" params

toLogin :: [Param] -> Maybe Login
toLogin params = Login . E.decodeUtf8 <$> lookup "login" params

toCreateUser ::
       MonadIO m
    => [(BC.ByteString, BC.ByteString)]
    -> [FileInfo LBS.ByteString]
    -> m CreateUser
toCreateUser params file = do
    now <- liftIO getCurrentTime
    if null file
        then do
            return
                CreateUser
                    { avatar_file_name = Nothing
                    , avatar_content = Nothing
                    , avatar_content_type = Nothing
                    , first_name = E.decodeUtf8 <$> lookup "f_name" params
                    , last_name = E.decodeUtf8 <$> lookup "l_name" params
                    , user_login = toLogin params
                    , user_password =
                          Password . E.decodeUtf8 <$> lookup "password" params
                    , creation_date = now
                    , admin_mark = False
                    }
        else return
                 CreateUser
                     { avatar_file_name = Just . fileName . head $ file
                     , avatar_content =
                           Just . Binary . fileContent . head $ file
                     , avatar_content_type =
                           Just . fileContentType . head $ file
                     , first_name = E.decodeUtf8 <$> lookup "f_name" params
                     , last_name = E.decodeUtf8 <$> lookup "l_name" params
                     , user_login = toLogin params
                     , user_password =
                           Password . E.decodeUtf8 <$> lookup "password" params
                     , creation_date = now
                     , admin_mark = False
                     }

toCommentId :: Request -> Maybe Id
toCommentId req =
    join (lookup "comment_id" $ queryString req) >>= readByteStringToId

toDraftId :: Request -> Maybe Id
toDraftId req =
    join (lookup "draft_id" $ queryString req) >>= readByteStringToId

toSort :: Request -> Sort
toSort req = Sort param
  where
    p = fromMaybe "" . join $ lookup "sort" (queryString req)
    param =
        case p of
            "author_name" -> "author_name"
            "date_creation" -> "date_creation"
            "category_name" -> "category_name"
            "short_title" -> "short_title"
            "news_id" -> "news_id"
            "news_text" -> "news_text"
            _ -> ""

toCommentText :: [Param] -> Maybe CommentText
toCommentText params =
    CommentText . E.decodeUtf8 <$> lookup "comment_text" params

class FilterParam a where
    toFilterParam :: Request -> Maybe a

instance FilterParam TagInFilterParam where
    toFilterParam req =
        TagInFilterParam <$>
        (readByteStringListInt =<<
         fromMaybe Nothing (lookup "tag_in" $ queryString req))

instance FilterParam CategoryFilterParam where
    toFilterParam req =
        CategoryFilterParam <$>
        (join (lookup "category" $ queryString req) >>= readByteStringToId)

instance FilterParam TagFilterParam where
    toFilterParam req =
        TagFilterParam <$>
        (join (lookup "tag" $ queryString req) >>= readByteStringToId)

instance FilterParam TagAllFilterParam where
    toFilterParam req =
        TagAllFilterParam <$>
        (readByteStringListInt =<<
         fromMaybe Nothing (lookup "tag_all" $ queryString req))

instance FilterParam TitleFilterParam where
    toFilterParam req =
        TitleFilterParam . E.decodeUtf8 <$>
        fromMaybe Nothing (lookup "title" $ queryString req)

instance FilterParam ContentFilterParam where
    toFilterParam req =
        ContentFilterParam . E.decodeUtf8 <$>
        fromMaybe Nothing (lookup "content" $ queryString req)

instance FilterParam DateFilterParam where
    toFilterParam req =
        DateFilterParam <$>
        (join (lookup "date" $ queryString req) >>= readByteStringToDay)

instance FilterParam BeforeDateFilterParam where
    toFilterParam req =
        BeforeDateFilterParam <$>
        (join (lookup "before_date" $ queryString req) >>= readByteStringToDay)

instance FilterParam AfterDateFilterParam where
    toFilterParam req =
        AfterDateFilterParam <$>
        (join (lookup "after_date" $ queryString req) >>= readByteStringToDay)

instance FilterParam AuthorFilterParam where
    toFilterParam req =
        AuthorFilterParam . E.decodeUtf8 <$>
        fromMaybe Nothing (lookup "author" $ queryString req)

toDraftInf :: Request -> [Param] -> DraftInf
toDraftInf req params =
    DraftInf
        { draft_inf_token = takeToken req
        , draft_inf_category = E.decodeUtf8 <$> lookup "category" params
        , draft_inf_title = E.decodeUtf8 <$> lookup "short_title" params
        , draft_inf_text = E.decodeUtf8 <$> lookup "news_text" params
        }

toDraftTags :: [Param] -> Maybe DraftTags
toDraftTags params = DraftTags <$> lookup "tags" params
