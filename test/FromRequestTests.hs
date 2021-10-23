{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module FromRequestTests where

import Data.Time.Calendar (fromGregorian)
import Database.PostgreSQL.Simple.Types (Binary(Binary))
import FromRequest
    ( FilterParam(toFilterParam)
    , checkNotImage
    , checkNotImageMaybe
    , checkNotImages
    , takeToken
    , toAuthorLogin
    , toCategoryName
    , toCommentId
    , toCommentText
    , toCreateAuthor
    , toCreateCategory
    , toDraftId
    , toDraftInf
    , toDraftTags
    , toEditAuthor
    , toEditCategory
    , toEditTag
    , toImage
    , toLogin
    , toPage
    , toPassword
    , toSort
    , toTagName
    )
import Network.Wai.Internal (Request(Request, queryString))
import Network.Wai.Parse (FileInfo(FileInfo))
import Test.Hspec (describe, hspec, it, shouldBe)
import Types.Authors
    ( AuthorLogin(AuthorLogin)
    , CreateAuthor(CreateAuthor)
    , EditAuthor(EditAuthor)
    )
import Types.Categories
    ( CategoryName(CategoryName)
    , CreateCategory(CreateCategory)
    , EditCategory(EditCategory)
    )
import Types.Drafts (DraftInf(DraftInf), DraftTags(DraftTags))
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
import Types.Other (Id(Id), Page(Page), Token(Token))
import Types.Tags (EditTag(EditTag), TagName(TagName))
import Types.Users (Login(Login), Password(Password))

fromRequestTests :: IO ()
fromRequestTests =
    hspec $ do
        describe "FromRequest" $ do
            describe "takeToken" $ do
                it "take token from request" $
                    takeToken
                        (Request {queryString = [("token", Just "qwerty")]}) `shouldBe`
                    Just (Token "qwerty")
                it "not found token" $
                    takeToken
                        (Request {queryString = [("token1", Just "qwerty")]}) `shouldBe`
                    Nothing
            describe "toImage" $ do
                it "convert FileInfo to Image" $
                    toImage (FileInfo "image.jpg" "image/jpeg" "some binary") `shouldBe`
                    Image "image.jpg" "image/jpeg" (Binary "some binary")
            describe "toPage" $ do
                it "get page parameter from request" $
                    toPage (Request {queryString = [("page", Just "2")]}) `shouldBe`
                    Just (Page 2)
                it "no page parameter" $
                    toPage (Request {queryString = [("page", Just "a")]}) `shouldBe`
                    Nothing
            describe "toEditAuthor" $ do
                it "get parameters for editing author's information" $
                    toEditAuthor
                        [ ("new_description", "author description")
                        , ("author_id", "1")
                        ] `shouldBe`
                    EditAuthor (Just "author description") (Just 1)
                it "no parameters" $
                    toEditAuthor [] `shouldBe` EditAuthor Nothing Nothing
                it "bad id parameter" $
                    toEditAuthor
                        [ ("new_description", "author description")
                        , ("author_id", "aaa")
                        ] `shouldBe`
                    EditAuthor (Just "author description") Nothing
            describe "toAuthorLogin" $ do
                it "get author's login parameter" $
                    toAuthorLogin [("author_login", "login")] `shouldBe`
                    Just (AuthorLogin "login")
                it "no author login parameter" $
                    toAuthorLogin [] `shouldBe` Nothing
            describe "toCreateAuthor" $ do
                it "get information for creating author" $
                    toCreateAuthor
                        [ ("author_login", "login")
                        , ("description", "new_description")
                        ] `shouldBe`
                    CreateAuthor (Just "login") (Just "new_description")
                it "no information for creating author" $
                    toCreateAuthor [] `shouldBe` CreateAuthor Nothing Nothing
            describe "toTagName" $ do
                it "get tag information from request" $
                    toTagName
                        (Request
                             {queryString = [("tag_name", Just "something")]}) `shouldBe`
                    Just (TagName "something")
                it "no tag_name parameter" $
                    toTagName (Request {queryString = []}) `shouldBe` Nothing
            describe "toEditTag" $ do
                it "get tag information for editing" $
                    toEditTag [("new_tag_name", "123"), ("old_tag_name", "aaa")] `shouldBe`
                    EditTag (Just $ TagName "123") (Just $ TagName "aaa")
                it "no inforamtion for tag editing" $
                    toEditTag [] `shouldBe` EditTag Nothing Nothing
            describe "toCategoryName" $ do
                it "get category name" $
                    toCategoryName [("category_name", "name")] `shouldBe`
                    Just (CategoryName "name")
                it "no category name" $ toCategoryName [] `shouldBe` Nothing
            describe "toCreateCategory" $ do
                it "get parameters for creating category" $
                    toCreateCategory
                        [ ("category_name", "Category")
                        , ("maternal_category_name", "Maternal")
                        ] `shouldBe`
                    CreateCategory
                        (Just $ CategoryName "category")
                        (Just $ CategoryName "maternal")
                it "no parameters for creating" $
                    toCreateCategory [] `shouldBe`
                    CreateCategory Nothing Nothing
                it "no category_name parameter for creating category" $
                    toCreateCategory [("maternal_category_name", "Maternal")] `shouldBe`
                    CreateCategory Nothing (Just $ CategoryName "maternal")
                it "no maternal_category_name parameter for creating category" $
                    toCreateCategory [("category_name", "Category")] `shouldBe`
                    CreateCategory (Just $ CategoryName "category") Nothing
            describe "toEditCategory" $ do
                it "get parameters for editing category" $
                    toEditCategory
                        [ ("category_name", "Category")
                        , ("new_name", "New name")
                        , ("new_maternal", "New maternal")
                        ] `shouldBe`
                    EditCategory
                        (Just $ CategoryName "category")
                        (Just $ CategoryName "new name")
                        (Just $ CategoryName "new maternal")
                it "no category_name parameter for editing category" $
                    toEditCategory
                        [ ("new_name", "New name")
                        , ("new_maternal", "New maternal")
                        ] `shouldBe`
                    EditCategory
                        Nothing
                        (Just $ CategoryName "new name")
                        (Just $ CategoryName "new maternal")
                it "no new_name parameter for editing category" $
                    toEditCategory
                        [ ("category_name", "Category")
                        , ("new_maternal", "New maternal")
                        ] `shouldBe`
                    EditCategory
                        (Just $ CategoryName "category")
                        Nothing
                        (Just $ CategoryName "new maternal")
                it "no new_maternal parameter for editing category" $
                    toEditCategory
                        [ ("category_name", "Category")
                        , ("new_name", "New name")
                        ] `shouldBe`
                    EditCategory
                        (Just $ CategoryName "category")
                        (Just $ CategoryName "new name")
                        Nothing
                it "no parameters for editing category" $
                    toEditCategory [] `shouldBe`
                    EditCategory Nothing Nothing Nothing
            describe "toPassword" $ do
                it "get password from request parameters" $
                    toPassword [("user_password", "123")] `shouldBe`
                    Just (Password "123")
                it "no password parameter" $ toPassword [] `shouldBe` Nothing
            describe "toLogin" $ do
                it "get login from request parameters" $
                    toLogin [("login", "123")] `shouldBe` Just (Login "123")
                it "no login parameter" $ toPassword [] `shouldBe` Nothing
            describe "toCommentId" $ do
                it "get CommentId parameter from request" $
                    toCommentId
                        (Request {queryString = [("comment_id", Just "2")]}) `shouldBe`
                    Just (Id 2)
                it "no CommentId parameter" $
                    toCommentId (Request {queryString = []}) `shouldBe` Nothing
            describe "toDraftId" $ do
                it "get DraftId parameter from request" $
                    toDraftId (Request {queryString = [("draft_id", Just "2")]}) `shouldBe`
                    Just (Id 2)
                it "no DraftId parameter" $
                    toDraftId (Request {queryString = []}) `shouldBe` Nothing
            describe "toSort" $ do
                it "get sort parameter by author_name from request" $
                    toSort
                        (Request {queryString = [("sort", Just "author_name")]}) `shouldBe`
                    Sort "author_name"
                it "get sort parameter by date_creation from request" $
                    toSort
                        (Request
                             {queryString = [("sort", Just "date_creation")]}) `shouldBe`
                    Sort "date_creation"
                it "get sort parameter by category_name from request" $
                    toSort
                        (Request
                             {queryString = [("sort", Just "category_name")]}) `shouldBe`
                    Sort "category_name"
                it "get sort parameter by short_title from request" $
                    toSort
                        (Request {queryString = [("sort", Just "short_title")]}) `shouldBe`
                    Sort "short_title"
                it "get sort parameter by news_id from request" $
                    toSort (Request {queryString = [("sort", Just "news_id")]}) `shouldBe`
                    Sort "news_id"
                it "get sort parameter by news_text from request" $
                    toSort
                        (Request {queryString = [("sort", Just "news_text")]}) `shouldBe`
                    Sort "news_text"
                it "get wrong sort parameter from request" $
                    toSort
                        (Request {queryString = [("sort", Just "something")]}) `shouldBe`
                    Sort ""
                it "no sort parameter" $
                    toSort (Request {queryString = []}) `shouldBe` Sort ""
            describe "toCommentText" $ do
                it "get commentary text from request parameters" $
                    toCommentText [("comment_text", "something")] `shouldBe`
                    Just (CommentText "something")
                it "no commentary text" $ toCommentText [] `shouldBe` Nothing
            describe "toFilterParam" $ do
                it "get tag_in filter param from request" $
                    toFilterParam
                        (Request {queryString = [("tag_in", Just "[1,2,3]")]}) `shouldBe`
                    Just (TagInFilterParam [1, 2, 3])
                it "no tag_in filter param from request" $
                    toFilterParam (Request {queryString = []}) `shouldBe`
                    (Nothing :: Maybe TagInFilterParam)
                it "get category filter param from request" $
                    toFilterParam
                        (Request {queryString = [("category", Just "1")]}) `shouldBe`
                    Just (CategoryFilterParam (Id 1))
                it "no category filter param from request" $
                    toFilterParam (Request {queryString = []}) `shouldBe`
                    (Nothing :: Maybe CategoryFilterParam)
                it "get tag filter param from request" $
                    toFilterParam (Request {queryString = [("tag", Just "1")]}) `shouldBe`
                    Just (TagFilterParam (Id 1))
                it "no tag filter param from request" $
                    toFilterParam (Request {queryString = []}) `shouldBe`
                    (Nothing :: Maybe TagFilterParam)
                it "get tag_all filter param from request" $
                    toFilterParam
                        (Request {queryString = [("tag_all", Just "[1,2,3]")]}) `shouldBe`
                    Just (TagAllFilterParam [1, 2, 3])
                it "no tag_all filter param from request" $
                    toFilterParam (Request {queryString = []}) `shouldBe`
                    (Nothing :: Maybe TagAllFilterParam)
                it "get title filter param from request" $
                    toFilterParam
                        (Request {queryString = [("title", Just "something")]}) `shouldBe`
                    Just (TitleFilterParam "something")
                it "no title filter param from request" $
                    toFilterParam (Request {queryString = []}) `shouldBe`
                    (Nothing :: Maybe TitleFilterParam)
                it "get content filter param from request" $
                    toFilterParam
                        (Request {queryString = [("content", Just "something")]}) `shouldBe`
                    Just (ContentFilterParam "something")
                it "no content filter param from request" $
                    toFilterParam (Request {queryString = []}) `shouldBe`
                    (Nothing :: Maybe ContentFilterParam)
                it "get author filter param from request" $
                    toFilterParam
                        (Request {queryString = [("author", Just "someone")]}) `shouldBe`
                    Just (AuthorFilterParam "someone")
                it "no author filter param from request" $
                    toFilterParam (Request {queryString = []}) `shouldBe`
                    (Nothing :: Maybe AuthorFilterParam)
                it "get date filter param from request" $
                    toFilterParam
                        (Request {queryString = [("date", Just "2021-09-01")]}) `shouldBe`
                    Just (DateFilterParam (fromGregorian 2021 09 01))
                it "no date filter param from request" $
                    toFilterParam (Request {queryString = []}) `shouldBe`
                    (Nothing :: Maybe DateFilterParam)
                it "get before_date filter param from request" $
                    toFilterParam
                        (Request
                             { queryString =
                                   [("before_date", Just "2021-09-01")]
                             }) `shouldBe`
                    Just (BeforeDateFilterParam (fromGregorian 2021 09 01))
                it "no before_date filter param from request" $
                    toFilterParam (Request {queryString = []}) `shouldBe`
                    (Nothing :: Maybe BeforeDateFilterParam)
                it "get after_date filter param from request" $
                    toFilterParam
                        (Request
                             {queryString = [("after_date", Just "2021-09-01")]}) `shouldBe`
                    Just (AfterDateFilterParam (fromGregorian 2021 09 01))
                it "no after_date filter param from request" $
                    toFilterParam (Request {queryString = []}) `shouldBe`
                    (Nothing :: Maybe AfterDateFilterParam)
            describe "toDraftTags" $ do
                it "get tags for draft creating from request parameters" $
                    toDraftTags [("tags", "tag1,tag2")] `shouldBe`
                    Just (DraftTags "tag1,tag2")
                it "no tags for draft creating" $
                    toDraftTags [] `shouldBe` Nothing
            describe "toDraftInf" $ do
                it "get information for creating draft from request" $
                    toDraftInf
                        (Request {queryString = [("token", Just "123")]})
                        [ ("category", "draft_category")
                        , ("short_title", "title")
                        , ("news_text", "text")
                        ] `shouldBe`
                    DraftInf
                        (Just (Token "123"))
                        (Just "draft_category")
                        (Just "title")
                        (Just "text")
                it "no token" $
                    toDraftInf
                        (Request {queryString = []})
                        [ ("category", "draft_category")
                        , ("short_title", "title")
                        , ("news_text", "text")
                        ] `shouldBe`
                    DraftInf
                        Nothing
                        (Just "draft_category")
                        (Just "title")
                        (Just "text")
                it "no category" $
                    toDraftInf
                        (Request {queryString = [("token", Just "123")]})
                        [("short_title", "title"), ("news_text", "text")] `shouldBe`
                    DraftInf
                        (Just (Token "123"))
                        Nothing
                        (Just "title")
                        (Just "text")
                it "no short_title" $
                    toDraftInf
                        (Request {queryString = [("token", Just "123")]})
                        [("category", "draft_category"), ("news_text", "text")] `shouldBe`
                    DraftInf
                        (Just (Token "123"))
                        (Just "draft_category")
                        Nothing
                        (Just "text")
                it "no text" $
                    toDraftInf
                        (Request {queryString = [("token", Just "123")]})
                        [ ("category", "draft_category")
                        , ("short_title", "title")
                        ] `shouldBe`
                    DraftInf
                        (Just (Token "123"))
                        (Just "draft_category")
                        (Just "title")
                        Nothing
                it "no information for draft creating" $
                    toDraftInf (Request {queryString = []}) [] `shouldBe`
                    DraftInf Nothing Nothing Nothing Nothing
            describe "checkNotImage" $ do
                it "return true, because image without name" $
                    checkNotImage (Image "" "image/jpeg" (Binary "something")) `shouldBe`
                    True
                it "return true, because image without content type" $
                    checkNotImage (Image "something" "" (Binary "something")) `shouldBe`
                    True
                it "return true, because image without content" $
                    checkNotImage (Image "something" "image/jpeg" (Binary "")) `shouldBe`
                    True
                it "return true, because image content type not image" $
                    checkNotImage
                        (Image "something" "text" (Binary "something")) `shouldBe`
                    True
                it "return false, because image just image" $
                    checkNotImage
                        (Image "something" "image/jpeg" (Binary "something")) `shouldBe`
                    False
            describe "checkNotImageMaybe" $ do
                it "return true, because image without name" $
                    checkNotImageMaybe
                        (Just (Image "" "image/jpeg" (Binary "something"))) `shouldBe`
                    True
                it "return true, because image without content type" $
                    checkNotImageMaybe
                        (Just (Image "something" "" (Binary "something"))) `shouldBe`
                    True
                it "return true, because image without content" $
                    checkNotImageMaybe
                        (Just (Image "something" "image/jpeg" (Binary ""))) `shouldBe`
                    True
                it "return true, because image content type not image" $
                    checkNotImageMaybe
                        (Just (Image "something" "text" (Binary "something"))) `shouldBe`
                    True
                it "return false, because image just image" $
                    checkNotImageMaybe
                        (Just
                             (Image
                                  "something"
                                  "image/jpeg"
                                  (Binary "something"))) `shouldBe`
                    False
                it "return false, because image, not exist (it's normal)" $
                    checkNotImageMaybe Nothing `shouldBe` False
            describe "checkNotImages" $ do
                it "return true, because one image without name" $
                    checkNotImages
                        (Just
                             [ Image "" "image/jpeg" (Binary "something")
                             , Image
                                   "something"
                                   "image/jpeg"
                                   (Binary "something")
                             ]) `shouldBe`
                    True
                it "return true, because one image without content type" $
                    checkNotImages
                        (Just
                             [ Image "something" "" (Binary "something")
                             , Image
                                   "something"
                                   "image/jpeg"
                                   (Binary "something")
                             ]) `shouldBe`
                    True
                it "return true, because one image without content" $
                    checkNotImages
                        (Just
                             [ Image "something" "image/jpeg" (Binary "")
                             , Image
                                   "something"
                                   "image/jpeg"
                                   (Binary "something")
                             ]) `shouldBe`
                    True
                it "return true, because one image content type not image" $
                    checkNotImages
                        (Just
                             [ Image "something" "text" (Binary "something")
                             , Image
                                   "something"
                                   "image/jpeg"
                                   (Binary "something")
                             ]) `shouldBe`
                    True
                it "return false, because images just images" $
                    checkNotImages
                        (Just
                             [ Image
                                   "something1"
                                   "image/jpeg"
                                   (Binary "something1")
                             , Image
                                   "something"
                                   "image/jpeg"
                                   (Binary "something")
                             ]) `shouldBe`
                    False
                it "return false, because images not exist (it's normal)" $
                    checkNotImages Nothing `shouldBe` False
