{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

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

import           Control.Monad.Except
import           Database.PostgreSQL.Simple.Types (PGArray (PGArray))
import           Types.NewsAndComments            (GetNews (..),
                                                   NewsArray (NewsArray))
import           Types.Other                      (ResponseErrorMessage (BadRequest, InternalServerError, MethodNotAllowed, NotFound),
                                                   ResponseOkMessage (OkJSON),
                                                   SomeError (DatabaseError, OtherError))

instance MonadIO Identity

hLogger :: LoggerHandle Identity
hLogger =
    LoggerHandle {priority = Debug, Logger.log = \prior message -> return ()}

newsHandler :: NewsAndCommentsHandle (ExceptT SomeError Identity)
newsHandler =
    NewsAndCommentsHandle
        { nchGetNewsByIdFromDb =
              \news_id -> throwError $ OtherError "ErrorMessage"
        , nchGetNewsFilterByTagInFromDb =
              \tag_in_filter_param page ->
                  throwError $ OtherError "ErrorMessage"
        , nchGetNewsFilterByCategoryIdFromDb =
              \category_id_filter_param page sort ->
                  throwError $ OtherError "ErrorMessage"
        , nchGetNewsFilterByTitleFromDb =
              \title_filter_param page sort ->
                  throwError $ OtherError "ErrorMessage"
        , nchGetgetNewsFilterByAuthorNameFromDb =
              \author_filter_param page sort ->
                  throwError $ OtherError "ErrorMessage"
        , nchGetNewsFilterByDateFromDb =
              \date_filter_param page sort ->
                  throwError $ OtherError "ErrorMessage"
        , nchGetNewsFilterByTagAllFromDb =
              \tag_all_filter_param page sort ->
                  throwError $ OtherError "ErrorMessage"
        , nchGetNewsFilterByContentFromDb =
              \content_filter_param page sort ->
                  throwError $ OtherError "ErrorMessage"
        , nchGetNewsFilterByAfterDateFromDb =
              \after_date_filter_param page sort ->
                  throwError $ OtherError "ErrorMessage"
        , nchGetNewsFilterByBeforeDateFromDb =
              \before_date_filter_param page sort ->
                  throwError $ OtherError "ErrorMessage"
        , nchGetNewsFilterByTagIdFromDb =
              \tag_filter_param page sort ->
                  throwError $ OtherError "ErrorMessage"
        , nchGetNewsFromDb =
              \sort page -> throwError $ OtherError "ErrorMessage"
        --, nchLogger = hLogger
        , nchParseRequestBody = \request -> return ([], [])
        }

operationsHandler :: OperationsHandle (ExceptT SomeError Identity)
operationsHandler = OperationsHandle {newsAndCommentsHandle = newsHandler}

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

tstNews :: GetNews
tstNews =
    GetNews
        { gnNewsId = 1
        , gnShortTitle = "title"
        , gnDateCreation = read "2021-11-19" :: Day
        , gnAuthorName = "name"
        , gnCategoryName = "category"
        , gnNewsText = "news text"
        , gnNewsMainImage = Just 1
        , gnNewsOtherImages = Just (PGArray [2])
        , gnNewsTags = Just (PGArray ["tag"])
        }

newsTests :: IO ()
newsTests =
    hspec $ do
        describe "testing news functions" $ do
            describe "testing nchGetNewsFromDb" $ do
                it "server should return error because something happend" $
                    routes operationsHandler hLogger tstGetNewsListReq `shouldBe`
                    return (Left $ BadRequest "News not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetNewsListReq {requestMethod = methodPut}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed "News not sended. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetNewsListReq {rawPathInfo = "/newss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of news, because all is good" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   newsHandler
                                       { nchGetNewsFromDb =
                                             \_ _ -> return (NewsArray [])
                                       }
                             })
                        hLogger
                        tstGetNewsListReq `shouldBe`
                    return (Right $ OkJSON "{\"news\":[]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   newsHandler
                                       { nchGetNewsFromDb =
                                             \_ _ ->
                                                 throwError $ DatabaseError 0
                                       }
                             })
                        hLogger
                        tstGetNewsListReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "News not sended. Database Error 0."))
{-
                    GET NEWS FILTERED BY TAG TESTS
-}
            describe "testing nchGetNewsFilterByTagIdFromDb" $ do
                it "server should return error because something happend" $
                    routes operationsHandler hLogger tstGetNewsFilterdByTagIdReq `shouldBe`
                    return (Left $ BadRequest "News not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetNewsFilterdByTagIdReq {requestMethod = methodPut}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed "News not sended. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetNewsFilterdByTagIdReq {rawPathInfo = "/newss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of news, because all is good" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   newsHandler
                                       { nchGetNewsFilterByTagIdFromDb =
                                             \_ _ _ -> return (NewsArray [])
                                       }
                             })
                        hLogger
                        tstGetNewsFilterdByTagIdReq `shouldBe`
                    return (Right $ OkJSON "{\"news\":[]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   newsHandler
                                       { nchGetNewsFilterByTagIdFromDb =
                                             \_ _ _ ->
                                                 throwError $ DatabaseError 0
                                       }
                             })
                        hLogger
                        tstGetNewsFilterdByTagIdReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "News not sended. Database Error 0."))
{-
                    GET NEWS FILTERED BY TAG_IN TESTS
-}
            describe "testing nchGetNewsFilterByTagInFromDb" $ do
                it "server should return error because something happend" $
                    routes operationsHandler hLogger tstGetNewsFilterdByTagInReq `shouldBe`
                    return (Left $ BadRequest "News not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetNewsFilterdByTagInReq {requestMethod = methodPut}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed "News not sended. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetNewsFilterdByTagInReq {rawPathInfo = "/newss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of news, because all is good" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   newsHandler
                                       { nchGetNewsFilterByTagInFromDb =
                                             \_ _ -> return (NewsArray [])
                                       }
                             })
                        hLogger
                        tstGetNewsFilterdByTagInReq `shouldBe`
                    return (Right $ OkJSON "{\"news\":[]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   newsHandler
                                       { nchGetNewsFilterByTagInFromDb =
                                             \_ _ ->
                                                 throwError $ DatabaseError 0
                                       }
                             })
                        hLogger
                        tstGetNewsFilterdByTagInReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "News not sended. Database Error 0."))
{-
                    GET NEWS FILTERED BY CATEGORY ID TESTS
-}
            describe "testing nchGetNewsFilterByCategoryIdFromDb" $ do
                it "server should return error because something happend" $
                    routes operationsHandler hLogger tstGetNewsFilterdByCatIdReq `shouldBe`
                    return (Left $ BadRequest "News not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetNewsFilterdByCatIdReq {requestMethod = methodPut}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed "News not sended. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetNewsFilterdByCatIdReq {rawPathInfo = "/newss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of news, because all is good" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   newsHandler
                                       { nchGetNewsFilterByCategoryIdFromDb =
                                             \_ _ _ -> return (NewsArray [])
                                       }
                             })
                        hLogger
                        tstGetNewsFilterdByCatIdReq `shouldBe`
                    return (Right $ OkJSON "{\"news\":[]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   newsHandler
                                       { nchGetNewsFilterByCategoryIdFromDb =
                                             \_ _ _ ->
                                                 throwError $ DatabaseError 0
                                       }
                             })
                        hLogger
                        tstGetNewsFilterdByCatIdReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "News not sended. Database Error 0."))
{-
                    GET NEWS FILTERED BY TITLE TESTS
-}
            describe "testing nchGetNewsFilterByTitleFromDb" $ do
                it "server should return error because something happend" $
                    routes operationsHandler hLogger tstGetNewsFilterdByTitleReq `shouldBe`
                    return (Left $ BadRequest "News not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetNewsFilterdByTitleReq {requestMethod = methodPut}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed "News not sended. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetNewsFilterdByTitleReq {rawPathInfo = "/newss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of news, because all is good" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   newsHandler
                                       { nchGetNewsFilterByTitleFromDb =
                                             \_ _ _ -> return (NewsArray [])
                                       }
                             })
                        hLogger
                        tstGetNewsFilterdByTitleReq `shouldBe`
                    return (Right $ OkJSON "{\"news\":[]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   newsHandler
                                       { nchGetNewsFilterByTitleFromDb =
                                             \_ _ _ ->
                                                 throwError $ DatabaseError 0
                                       }
                             })
                        hLogger
                        tstGetNewsFilterdByTitleReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "News not sended. Database Error 0."))
{-
                    GET NEWS FILTERED BY AUTHOR TESTS
-}
            describe "testing nchGetgetNewsFilterByAuthorNameFromDb" $ do
                it "server should return error because something happend" $
                    routes
                        operationsHandler
                        hLogger
                        tstGetNewsFilterdByAuthorReq `shouldBe`
                    return (Left $ BadRequest "News not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetNewsFilterdByAuthorReq
                             {requestMethod = methodPut}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed "News not sended. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetNewsFilterdByAuthorReq {rawPathInfo = "/newss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of news, because all is good" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   newsHandler
                                       { nchGetgetNewsFilterByAuthorNameFromDb =
                                             \_ _ _ -> return (NewsArray [])
                                       }
                             })
                        hLogger
                        tstGetNewsFilterdByAuthorReq `shouldBe`
                    return (Right $ OkJSON "{\"news\":[]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   newsHandler
                                       { nchGetgetNewsFilterByAuthorNameFromDb =
                                             \_ _ _ ->
                                                 throwError $ DatabaseError 0
                                       }
                             })
                        hLogger
                        tstGetNewsFilterdByAuthorReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "News not sended. Database Error 0."))
{-
                    GET NEWS FILTERED BY DATE TESTS
-}
            describe "testing nchGetNewsFilterByDateFromDb" $ do
                it "server should return error because something happend" $
                    routes operationsHandler hLogger tstGetNewsFilterdByDateReq `shouldBe`
                    return (Left $ BadRequest "News not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetNewsFilterdByDateReq {requestMethod = methodPut}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed "News not sended. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetNewsFilterdByDateReq {rawPathInfo = "/newss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of news, because all is good" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   newsHandler
                                       { nchGetNewsFilterByDateFromDb =
                                             \_ _ _ -> return (NewsArray [])
                                       }
                             })
                        hLogger
                        tstGetNewsFilterdByDateReq `shouldBe`
                    return (Right $ OkJSON "{\"news\":[]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   newsHandler
                                       { nchGetNewsFilterByDateFromDb =
                                             \_ _ _ ->
                                                 throwError $ DatabaseError 0
                                       }
                             })
                        hLogger
                        tstGetNewsFilterdByDateReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "News not sended. Database Error 0."))
{-
                    GET NEWS FILTERED BY TAG_ALL TESTS
-}
            describe "testing nchGetNewsFilterByTagAllFromDb" $ do
                it "server should return error because something happend" $
                    routes
                        operationsHandler
                        hLogger
                        tstGetNewsFilterdByTagAllReq `shouldBe`
                    return (Left $ BadRequest "News not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetNewsFilterdByTagAllReq
                             {requestMethod = methodPut}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed "News not sended. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetNewsFilterdByTagAllReq {rawPathInfo = "/newss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of news, because all is good" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   newsHandler
                                       { nchGetNewsFilterByTagAllFromDb =
                                             \_ _ _ -> return (NewsArray [])
                                       }
                             })
                        hLogger
                        tstGetNewsFilterdByTagAllReq `shouldBe`
                    return (Right $ OkJSON "{\"news\":[]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   newsHandler
                                       { nchGetNewsFilterByTagAllFromDb =
                                             \_ _ _ ->
                                                 throwError $ DatabaseError 0
                                       }
                             })
                        hLogger
                        tstGetNewsFilterdByTagAllReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "News not sended. Database Error 0."))
{-
                    GET NEWS FILTERED BY TAG_ALL TESTS
-}
            describe "testing nchGetNewsFilterByContentFromDb" $ do
                it "server should return error because something happend" $
                    routes
                        operationsHandler
                        hLogger
                        tstGetNewsFilterdByContentReq `shouldBe`
                    return (Left $ BadRequest "News not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetNewsFilterdByContentReq
                             {requestMethod = methodPut}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed "News not sended. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetNewsFilterdByContentReq {rawPathInfo = "/newss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of news, because all is good" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   newsHandler
                                       { nchGetNewsFilterByContentFromDb =
                                             \_ _ _ -> return (NewsArray [])
                                       }
                             })
                        hLogger
                        tstGetNewsFilterdByContentReq `shouldBe`
                    return (Right $ OkJSON "{\"news\":[]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   newsHandler
                                       { nchGetNewsFilterByContentFromDb =
                                             \_ _ _ ->
                                                 throwError $ DatabaseError 0
                                       }
                             })
                        hLogger
                        tstGetNewsFilterdByContentReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "News not sended. Database Error 0."))
{-
                    GET NEWS FILTERED BY AFTER_DATE TESTS
-}
            describe "testing nchGetNewsFilterByAfterDateFromDb" $ do
                it "server should return error because something happend" $
                    routes
                        operationsHandler
                        hLogger
                        tstGetNewsFilterdByAfterDateReq `shouldBe`
                    return (Left $ BadRequest "News not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetNewsFilterdByAfterDateReq
                             {requestMethod = methodPut}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed "News not sended. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetNewsFilterdByAfterDateReq
                             {rawPathInfo = "/newss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of news, because all is good" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   newsHandler
                                       { nchGetNewsFilterByAfterDateFromDb =
                                             \_ _ _ -> return (NewsArray [])
                                       }
                             })
                        hLogger
                        tstGetNewsFilterdByAfterDateReq `shouldBe`
                    return (Right $ OkJSON "{\"news\":[]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   newsHandler
                                       { nchGetNewsFilterByAfterDateFromDb =
                                             \_ _ _ ->
                                                 throwError $ DatabaseError 0
                                       }
                             })
                        hLogger
                        tstGetNewsFilterdByAfterDateReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "News not sended. Database Error 0."))
{-
                    GET NEWS FILTERED BY BEFORE_DATE TESTS
-}
            describe "testing nchGetNewsFilterByBeforeDateFromDb" $ do
                it "server should return error because something happend" $
                    routes
                        operationsHandler
                        hLogger
                        tstGetNewsFilterdByBeforeDateReq `shouldBe`
                    return (Left $ BadRequest "News not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetNewsFilterdByBeforeDateReq
                             {requestMethod = methodPut}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed "News not sended. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetNewsFilterdByBeforeDateReq
                             {rawPathInfo = "/newss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of news, because all is good" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   newsHandler
                                       { nchGetNewsFilterByBeforeDateFromDb =
                                             \_ _ _ -> return (NewsArray [])
                                       }
                             })
                        hLogger
                        tstGetNewsFilterdByBeforeDateReq `shouldBe`
                    return (Right $ OkJSON "{\"news\":[]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   newsHandler
                                       { nchGetNewsFilterByBeforeDateFromDb =
                                             \_ _ _ ->
                                                 throwError $ DatabaseError 0
                                       }
                             })
                        hLogger
                        tstGetNewsFilterdByBeforeDateReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "News not sended. Database Error 0."))
{-
                    GET NEWS BY ID TESTS
-}
            describe "testing nchGetNewsByIdFromDb" $ do
                it "server should return error because something happend" $
                    routes operationsHandler hLogger tstGetNewsByIdReq `shouldBe`
                    return (Left $ BadRequest "News not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetNewsByIdReq {requestMethod = methodPut}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed "News not sended. Bad method request.")
                it "server should return list of news, because all is good" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   newsHandler
                                       { nchGetNewsByIdFromDb =
                                             \_ -> return tstNews
                                       }
                             })
                        hLogger
                        tstGetNewsByIdReq `shouldBe`
                    return
                        (Right $
                         OkJSON
                             "{\"date_creation\":\"2021-11-19\",\"short_title\":\"title\",\"author_name\":\"name\",\"news_text\":\"news text\",\"category_name\":\"category\",\"news_main_image\":1,\"news_other_images\":[2],\"news_id\":1,\"news_tags\":[\"tag\"]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   newsHandler
                                       { nchGetNewsByIdFromDb =
                                             \_ -> throwError $ DatabaseError 0
                                       }
                             })
                        hLogger
                        tstGetNewsByIdReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "News not sended. Database Error 0."))
