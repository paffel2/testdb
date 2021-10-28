{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module NewsTests where

import           Data.Functor.Identity            (Identity)
import           Data.Time                        (Day)
import           Logger                           (LoggerHandle (..),
                                                   Priority (Debug))
import           Network.HTTP.Types               (methodGet, methodPut)
import           Network.Wai                      (Request (queryString, rawPathInfo, requestMethod),
                                                   defaultRequest)
import           OperationsHandle                 (NewsAndCommentsHandle (..),
                                                   OperationsHandle (..))
import           Router                           (routes)
import           Test.Hspec                       (describe, hspec, it,
                                                   shouldBe)

import           Database.PostgreSQL.Simple.Types (PGArray (PGArray))
import           Types.NewsAndComments            (GetNews (GetNews, gn_author_name, gn_category_name, gn_date_creation, gn_news_id, gn_news_main_image, gn_news_other_images, gn_news_tags, gn_news_text, gn_short_title),
                                                   NewsArray (NewsArray))
import           Types.Other                      (ResponseErrorMessage (BadRequest, InternalServerError, MethodNotAllowed, NotFound),
                                                   ResponseOkMessage (OkJSON),
                                                   SomeError (DatabaseError, OtherError))

hLogger :: LoggerHandle Identity
hLogger =
    LoggerHandle {priority = Debug, Logger.log = \prior message -> return ()}

newsHandler :: NewsAndCommentsHandle Identity
newsHandler =
    NewsAndCommentsHandle
        { get_news_by_id_from_db =
              \news_id -> return $ Left $ OtherError "ErrorMessage"
        , get_news_filter_by_tag_in_from_db =
              \tag_in_filter_param page ->
                  return $ Left $ OtherError "ErrorMessage"
        , get_news_filter_by_category_id_from_db =
              \category_id_filter_param page sort ->
                  return $ Left $ OtherError "ErrorMessage"
        , get_news_filter_by_title_from_db =
              \title_filter_param page sort ->
                  return $ Left $ OtherError "ErrorMessage"
        , get_news_filter_by_author_name_from_db =
              \author_filter_param page sort ->
                  return $ Left $ OtherError "ErrorMessage"
        , get_news_filter_by_date_from_db =
              \date_filter_param page sort ->
                  return $ Left $ OtherError "ErrorMessage"
        , get_news_filter_by_tag_all_from_db =
              \tag_all_filter_param page sort ->
                  return $ Left $ OtherError "ErrorMessage"
        , get_news_filter_by_content_from_db =
              \content_filter_param page sort ->
                  return $ Left $ OtherError "ErrorMessage"
        , get_news_filter_by_after_date_from_db =
              \after_date_filter_param page sort ->
                  return $ Left $ OtherError "ErrorMessage"
        , get_news_filter_by_before_date_from_db =
              \before_date_filter_param page sort ->
                  return $ Left $ OtherError "ErrorMessage"
        , get_news_filter_by_tag_id_from_db =
              \tag_filter_param page sort ->
                  return $ Left $ OtherError "ErrorMessage"
        , get_news_from_db =
              \sort page -> return $ Left $ OtherError "ErrorMessage"
        , news_logger = hLogger
        , news_parse_request_body = \request -> return ([], [])
        }

operationsHandler :: OperationsHandle Identity
operationsHandler = OperationsHandle {news_and_comments_handle = newsHandler}

tstGetNewsListReq :: Request
tstGetNewsListReq =
    defaultRequest {rawPathInfo = "/news", requestMethod = methodGet}

tstGetNewsByIdReq :: Request
tstGetNewsByIdReq =
    defaultRequest {rawPathInfo = "/news/1", requestMethod = methodGet}

tstGetNewsFilterdByTagIdReq :: Request
tstGetNewsFilterdByTagIdReq =
    defaultRequest
        { rawPathInfo = "/news"
        , requestMethod = methodGet
        , queryString = [("tag", Just "1")]
        }

tstGetNewsFilterdByTagInReq :: Request
tstGetNewsFilterdByTagInReq =
    defaultRequest
        { rawPathInfo = "/news"
        , requestMethod = methodGet
        , queryString = [("tag_in", Just "[1,2,3]")]
        }

tstGetNewsFilterdByCatIdReq :: Request
tstGetNewsFilterdByCatIdReq =
    defaultRequest
        { rawPathInfo = "/news"
        , requestMethod = methodGet
        , queryString = [("category", Just "1")]
        }

tstGetNewsFilterdByTitleReq :: Request
tstGetNewsFilterdByTitleReq =
    defaultRequest
        { rawPathInfo = "/news"
        , requestMethod = methodGet
        , queryString = [("title", Just "title")]
        }

tstGetNewsFilterdByAuthorReq :: Request
tstGetNewsFilterdByAuthorReq =
    defaultRequest
        { rawPathInfo = "/news"
        , requestMethod = methodGet
        , queryString = [("author", Just "name")]
        }

tstGetNewsFilterdByDateReq :: Request
tstGetNewsFilterdByDateReq =
    defaultRequest
        { rawPathInfo = "/news"
        , requestMethod = methodGet
        , queryString = [("date", Just "2021-11-19")]
        }

tstGetNewsFilterdByTagAllReq :: Request
tstGetNewsFilterdByTagAllReq =
    defaultRequest
        { rawPathInfo = "/news"
        , requestMethod = methodGet
        , queryString = [("tag_all", Just "[1,2,3]")]
        }

tstGetNewsFilterdByContentReq :: Request
tstGetNewsFilterdByContentReq =
    defaultRequest
        { rawPathInfo = "/news"
        , requestMethod = methodGet
        , queryString = [("content", Just "something")]
        }

tstGetNewsFilterdByAfterDateReq :: Request
tstGetNewsFilterdByAfterDateReq =
    defaultRequest
        { rawPathInfo = "/news"
        , requestMethod = methodGet
        , queryString = [("after_date", Just "2021-11-19")]
        }

tstGetNewsFilterdByBeforeDateReq :: Request
tstGetNewsFilterdByBeforeDateReq =
    defaultRequest
        { rawPathInfo = "/news"
        , requestMethod = methodGet
        , queryString = [("before_date", Just "2021-11-19")]
        }

tstNews =
    GetNews
        { gn_news_id = 1
        , gn_short_title = "title"
        , gn_date_creation = read "2021-11-19" :: Day
        , gn_author_name = "name"
        , gn_category_name = "category"
        , gn_news_text = "news text"
        , gn_news_main_image = Just 1
        , gn_news_other_images = Just (PGArray [2])
        , gn_news_tags = Just (PGArray ["tag"])
        }

newsTests :: IO ()
newsTests =
    hspec $ do
        describe "testing news functions" $ do
            describe "testing get_news_from_db" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstGetNewsListReq `shouldBe`
                    return (Left $ BadRequest "News not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        (tstGetNewsListReq {requestMethod = methodPut}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstGetNewsListReq {rawPathInfo = "/newss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of news, because all is good" $
                    routes
                        (operationsHandler
                             { news_and_comments_handle =
                                   newsHandler
                                       { get_news_from_db =
                                             \_ _ ->
                                                 return $ Right (NewsArray [])
                                       }
                             })
                        tstGetNewsListReq `shouldBe`
                    return (Right $ OkJSON "{\"news\":[]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { news_and_comments_handle =
                                   newsHandler
                                       { get_news_from_db =
                                             \_ _ -> return $ Left DatabaseError
                                       }
                             })
                        tstGetNewsListReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "News not sended. Database Error."))
{-
                    GET NEWS FILTERED BY TAG TESTS
-}
            describe "testing get_news_filter_by_tag_id_from_db" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstGetNewsFilterdByTagIdReq `shouldBe`
                    return (Left $ BadRequest "News not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        (tstGetNewsFilterdByTagIdReq {requestMethod = methodPut}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstGetNewsFilterdByTagIdReq {rawPathInfo = "/newss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of news, because all is good" $
                    routes
                        (operationsHandler
                             { news_and_comments_handle =
                                   newsHandler
                                       { get_news_filter_by_tag_id_from_db =
                                             \_ _ _ ->
                                                 return $ Right (NewsArray [])
                                       }
                             })
                        tstGetNewsFilterdByTagIdReq `shouldBe`
                    return (Right $ OkJSON "{\"news\":[]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { news_and_comments_handle =
                                   newsHandler
                                       { get_news_filter_by_tag_id_from_db =
                                             \_ _ _ ->
                                                 return $ Left DatabaseError
                                       }
                             })
                        tstGetNewsFilterdByTagIdReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "News not sended. Database Error."))
{-
                    GET NEWS FILTERED BY TAG_IN TESTS
-}
            describe "testing get_news_filter_by_tag_in_from_db" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstGetNewsFilterdByTagInReq `shouldBe`
                    return (Left $ BadRequest "News not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        (tstGetNewsFilterdByTagInReq {requestMethod = methodPut}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstGetNewsFilterdByTagInReq {rawPathInfo = "/newss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of news, because all is good" $
                    routes
                        (operationsHandler
                             { news_and_comments_handle =
                                   newsHandler
                                       { get_news_filter_by_tag_in_from_db =
                                             \_ _ ->
                                                 return $ Right (NewsArray [])
                                       }
                             })
                        tstGetNewsFilterdByTagInReq `shouldBe`
                    return (Right $ OkJSON "{\"news\":[]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { news_and_comments_handle =
                                   newsHandler
                                       { get_news_filter_by_tag_in_from_db =
                                             \_ _ -> return $ Left DatabaseError
                                       }
                             })
                        tstGetNewsFilterdByTagInReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "News not sended. Database Error."))
{-
                    GET NEWS FILTERED BY CATEGORY ID TESTS
-}
            describe "testing get_news_filter_by_category_id_from_db" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstGetNewsFilterdByCatIdReq `shouldBe`
                    return (Left $ BadRequest "News not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        (tstGetNewsFilterdByCatIdReq {requestMethod = methodPut}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstGetNewsFilterdByCatIdReq {rawPathInfo = "/newss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of news, because all is good" $
                    routes
                        (operationsHandler
                             { news_and_comments_handle =
                                   newsHandler
                                       { get_news_filter_by_category_id_from_db =
                                             \_ _ _ ->
                                                 return $ Right (NewsArray [])
                                       }
                             })
                        tstGetNewsFilterdByCatIdReq `shouldBe`
                    return (Right $ OkJSON "{\"news\":[]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { news_and_comments_handle =
                                   newsHandler
                                       { get_news_filter_by_category_id_from_db =
                                             \_ _ _ ->
                                                 return $ Left DatabaseError
                                       }
                             })
                        tstGetNewsFilterdByCatIdReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "News not sended. Database Error."))
{-
                    GET NEWS FILTERED BY TITLE TESTS
-}
            describe "testing get_news_filter_by_category_id_from_db" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstGetNewsFilterdByTitleReq `shouldBe`
                    return (Left $ BadRequest "News not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        (tstGetNewsFilterdByTitleReq {requestMethod = methodPut}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstGetNewsFilterdByTitleReq {rawPathInfo = "/newss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of news, because all is good" $
                    routes
                        (operationsHandler
                             { news_and_comments_handle =
                                   newsHandler
                                       { get_news_filter_by_title_from_db =
                                             \_ _ _ ->
                                                 return $ Right (NewsArray [])
                                       }
                             })
                        tstGetNewsFilterdByTitleReq `shouldBe`
                    return (Right $ OkJSON "{\"news\":[]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { news_and_comments_handle =
                                   newsHandler
                                       { get_news_filter_by_title_from_db =
                                             \_ _ _ ->
                                                 return $ Left DatabaseError
                                       }
                             })
                        tstGetNewsFilterdByTitleReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "News not sended. Database Error."))
{-
                    GET NEWS FILTERED BY AUTHOR TESTS
-}
            describe "testing get_news_filter_by_author_name_from_db" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstGetNewsFilterdByAuthorReq `shouldBe`
                    return (Left $ BadRequest "News not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        (tstGetNewsFilterdByAuthorReq
                             {requestMethod = methodPut}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstGetNewsFilterdByAuthorReq {rawPathInfo = "/newss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of news, because all is good" $
                    routes
                        (operationsHandler
                             { news_and_comments_handle =
                                   newsHandler
                                       { get_news_filter_by_author_name_from_db =
                                             \_ _ _ ->
                                                 return $ Right (NewsArray [])
                                       }
                             })
                        tstGetNewsFilterdByAuthorReq `shouldBe`
                    return (Right $ OkJSON "{\"news\":[]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { news_and_comments_handle =
                                   newsHandler
                                       { get_news_filter_by_author_name_from_db =
                                             \_ _ _ ->
                                                 return $ Left DatabaseError
                                       }
                             })
                        tstGetNewsFilterdByAuthorReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "News not sended. Database Error."))
{-
                    GET NEWS FILTERED BY DATE TESTS
-}
            describe "testing get_news_filter_by_date_from_db" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstGetNewsFilterdByDateReq `shouldBe`
                    return (Left $ BadRequest "News not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        (tstGetNewsFilterdByDateReq {requestMethod = methodPut}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstGetNewsFilterdByDateReq {rawPathInfo = "/newss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of news, because all is good" $
                    routes
                        (operationsHandler
                             { news_and_comments_handle =
                                   newsHandler
                                       { get_news_filter_by_date_from_db =
                                             \_ _ _ ->
                                                 return $ Right (NewsArray [])
                                       }
                             })
                        tstGetNewsFilterdByDateReq `shouldBe`
                    return (Right $ OkJSON "{\"news\":[]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { news_and_comments_handle =
                                   newsHandler
                                       { get_news_filter_by_date_from_db =
                                             \_ _ _ ->
                                                 return $ Left DatabaseError
                                       }
                             })
                        tstGetNewsFilterdByDateReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "News not sended. Database Error."))
{-
                    GET NEWS FILTERED BY TAG_ALL TESTS
-}
            describe "testing get_news_filter_by_tag_all_from_db" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstGetNewsFilterdByTagAllReq `shouldBe`
                    return (Left $ BadRequest "News not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        (tstGetNewsFilterdByTagAllReq
                             {requestMethod = methodPut}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstGetNewsFilterdByTagAllReq {rawPathInfo = "/newss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of news, because all is good" $
                    routes
                        (operationsHandler
                             { news_and_comments_handle =
                                   newsHandler
                                       { get_news_filter_by_tag_all_from_db =
                                             \_ _ _ ->
                                                 return $ Right (NewsArray [])
                                       }
                             })
                        tstGetNewsFilterdByTagAllReq `shouldBe`
                    return (Right $ OkJSON "{\"news\":[]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { news_and_comments_handle =
                                   newsHandler
                                       { get_news_filter_by_tag_all_from_db =
                                             \_ _ _ ->
                                                 return $ Left DatabaseError
                                       }
                             })
                        tstGetNewsFilterdByTagAllReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "News not sended. Database Error."))
{-
                    GET NEWS FILTERED BY TAG_ALL TESTS
-}
            describe "testing get_news_filter_by_content_from_db" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstGetNewsFilterdByContentReq `shouldBe`
                    return (Left $ BadRequest "News not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        (tstGetNewsFilterdByContentReq
                             {requestMethod = methodPut}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstGetNewsFilterdByContentReq {rawPathInfo = "/newss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of news, because all is good" $
                    routes
                        (operationsHandler
                             { news_and_comments_handle =
                                   newsHandler
                                       { get_news_filter_by_content_from_db =
                                             \_ _ _ ->
                                                 return $ Right (NewsArray [])
                                       }
                             })
                        tstGetNewsFilterdByContentReq `shouldBe`
                    return (Right $ OkJSON "{\"news\":[]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { news_and_comments_handle =
                                   newsHandler
                                       { get_news_filter_by_content_from_db =
                                             \_ _ _ ->
                                                 return $ Left DatabaseError
                                       }
                             })
                        tstGetNewsFilterdByContentReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "News not sended. Database Error."))
{-
                    GET NEWS FILTERED BY AFTER_DATE TESTS
-}
            describe "testing get_news_filter_by_after_date_from_db" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstGetNewsFilterdByAfterDateReq `shouldBe`
                    return (Left $ BadRequest "News not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        (tstGetNewsFilterdByAfterDateReq
                             {requestMethod = methodPut}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstGetNewsFilterdByAfterDateReq
                             {rawPathInfo = "/newss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of news, because all is good" $
                    routes
                        (operationsHandler
                             { news_and_comments_handle =
                                   newsHandler
                                       { get_news_filter_by_after_date_from_db =
                                             \_ _ _ ->
                                                 return $ Right (NewsArray [])
                                       }
                             })
                        tstGetNewsFilterdByAfterDateReq `shouldBe`
                    return (Right $ OkJSON "{\"news\":[]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { news_and_comments_handle =
                                   newsHandler
                                       { get_news_filter_by_after_date_from_db =
                                             \_ _ _ ->
                                                 return $ Left DatabaseError
                                       }
                             })
                        tstGetNewsFilterdByAfterDateReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "News not sended. Database Error."))
{-
                    GET NEWS FILTERED BY BEFORE_DATE TESTS
-}
            describe "testing get_news_filter_by_before_date_from_db" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstGetNewsFilterdByBeforeDateReq `shouldBe`
                    return (Left $ BadRequest "News not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        (tstGetNewsFilterdByBeforeDateReq
                             {requestMethod = methodPut}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstGetNewsFilterdByBeforeDateReq
                             {rawPathInfo = "/newss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of news, because all is good" $
                    routes
                        (operationsHandler
                             { news_and_comments_handle =
                                   newsHandler
                                       { get_news_filter_by_before_date_from_db =
                                             \_ _ _ ->
                                                 return $ Right (NewsArray [])
                                       }
                             })
                        tstGetNewsFilterdByBeforeDateReq `shouldBe`
                    return (Right $ OkJSON "{\"news\":[]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { news_and_comments_handle =
                                   newsHandler
                                       { get_news_filter_by_before_date_from_db =
                                             \_ _ _ ->
                                                 return $ Left DatabaseError
                                       }
                             })
                        tstGetNewsFilterdByBeforeDateReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "News not sended. Database Error."))
{-
                    GET NEWS BY ID TESTS
-}
            describe "testing get_news_by_id_from_db" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstGetNewsByIdReq `shouldBe`
                    return (Left $ BadRequest "News not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        (tstGetNewsByIdReq {requestMethod = methodPut}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return list of news, because all is good" $
                    routes
                        (operationsHandler
                             { news_and_comments_handle =
                                   newsHandler
                                       { get_news_by_id_from_db =
                                             \_ -> return $ Right tstNews
                                       }
                             })
                        tstGetNewsByIdReq `shouldBe`
                    return
                        (Right $
                         OkJSON
                             "{\"date_creation\":\"2021-11-19\",\"short_title\":\"title\",\"author_name\":\"name\",\"news_text\":\"news text\",\"category_name\":\"category\",\"news_main_image\":1,\"news_other_images\":[2],\"news_id\":1,\"news_tags\":[\"tag\"]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { news_and_comments_handle =
                                   newsHandler
                                       { get_news_by_id_from_db =
                                             \_ -> return $ Left DatabaseError
                                       }
                             })
                        tstGetNewsByIdReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "News not sended. Database Error."))
