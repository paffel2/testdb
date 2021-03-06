{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module CommentsTests where

import           Control.Monad.Except
import           Data.Functor.Identity (Identity)
import           Logger                (LoggerHandle (..), Priority (Debug))
import           Network.HTTP.Types    (methodDelete, methodGet, methodPost,
                                        methodPut)
import           Network.Wai           (Request (rawPathInfo, requestMethod),
                                        defaultRequest)
import           OperationsHandle      (NewsAndCommentsHandle (..),
                                        OperationsHandle (..))
import           Router                (routes)
import           Test.Hspec            (describe, hspec, it, shouldBe)

import           Types.NewsAndComments (CommentArray (CommentArray))
import           Types.Other           (ResponseErrorMessage (BadRequest, Forbidden, InternalServerError, MethodNotAllowed, NotFound),
                                        ResponseOkMessage (Created, OkJSON, OkMessage),
                                        SomeError (BadToken, DatabaseError, NotAdmin, OtherError))

instance MonadIO Identity

hLogger :: LoggerHandle Identity
hLogger =
    LoggerHandle {priority = Debug, Logger.log = \prior message -> return ()}

commentsHandler :: NewsAndCommentsHandle (ExceptT SomeError Identity)
commentsHandler =
    NewsAndCommentsHandle
        { nchAddCommentToDb =
              \comment_information -> throwError $ OtherError "ErrorMessage"
        , nchDeleteCommentFromDb =
              \token comment_id -> throwError $ OtherError "ErrorMessage"
        , nchGetCommentsByNewsIdFromDb =
              \news_id page -> throwError $ OtherError "ErrorMessage"
        --, nchLogger = hLogger
        , nchParseRequestBody = \request -> return ([], [])
        }

operationsHandler :: OperationsHandle (ExceptT SomeError Identity)
operationsHandler = OperationsHandle {newsAndCommentsHandle = commentsHandler}

tstGetCommentsListReq :: Request
tstGetCommentsListReq =
    defaultRequest {rawPathInfo = "/news/1/comments", requestMethod = methodGet}

tstPostCommentReq :: Request
tstPostCommentReq =
    defaultRequest
        { rawPathInfo = "/news/1/comments/add_comment"
        , requestMethod = methodPost
        }

tstDeleteCommentReq :: Request
tstDeleteCommentReq =
    defaultRequest
        { rawPathInfo = "/news/1/comments/delete_comment"
        , requestMethod = methodDelete
        }

commentsTests :: IO ()
commentsTests =
    hspec $ do
        describe "testing comments functions" $ do
            describe "testing nchGetCommentsByNewsIdFromDb" $ do
                it "server should return error because something happend" $
                    routes operationsHandler hLogger tstGetCommentsListReq `shouldBe`
                    return
                        (Left $
                         BadRequest "Commentaries not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetCommentsListReq {requestMethod = methodPut}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed
                             "Commentaries not sended. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetCommentsListReq
                             {rawPathInfo = "/news/1/commentsssss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of comments, because all is good" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   commentsHandler
                                       { nchGetCommentsByNewsIdFromDb =
                                             \_ _ -> return (CommentArray [])
                                       }
                             })
                        hLogger
                        tstGetCommentsListReq `shouldBe`
                    return (Right $ OkJSON "{\"comments\":[]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   commentsHandler
                                       { nchGetCommentsByNewsIdFromDb =
                                             \_ _ ->
                                                 throwError $ DatabaseError 0
                                       }
                             })
                        hLogger
                        tstGetCommentsListReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "Commentaries not sended. Database Error 0."))
{-
                                CREATE COMMENT TESTS
-}
            describe "testing nchAddCommentToDb" $ do
                it "server should return error because something happend" $
                    routes operationsHandler hLogger tstPostCommentReq `shouldBe`
                    return
                        (Left $ BadRequest "Commentary not added. ErrorMessage")
                it
                    "server should return error, because request sended with bad requestMethod" $
                    routes
                        operationsHandler
                        hLogger
                        (tstPostCommentReq {requestMethod = methodGet}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed
                             "Commentary not added. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstPostCommentReq
                             {rawPathInfo = "/news/1/comments/add_comments"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it
                    "server should return message about successful adding commentary, because all is good" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   commentsHandler
                                       {nchAddCommentToDb = \_ -> return ()}
                             })
                        hLogger
                        tstPostCommentReq `shouldBe`
                    return (Right (Created "Commentary added."))
                it "server should return error, because token is bad" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   commentsHandler
                                       { nchAddCommentToDb =
                                             \_ -> throwError BadToken
                                       }
                             })
                        hLogger
                        tstPostCommentReq `shouldBe`
                    return (Left (Forbidden "Commentary not added. Bad Token."))
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   commentsHandler
                                       { nchAddCommentToDb =
                                             \_ -> throwError $ DatabaseError 0
                                       }
                             })
                        hLogger
                        tstPostCommentReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "Commentary not added. Database Error 0."))
{-
                                DELETE COMMENT TESTS
-}
            describe "testing nchDeleteCommentFromDb" $ do
                it "server should return error because something happend" $
                    routes operationsHandler hLogger tstDeleteCommentReq `shouldBe`
                    return
                        (Left $
                         BadRequest "Commentary not deleted. ErrorMessage")
                it
                    "server should return error, because request sended with bad requestMethod" $
                    routes
                        operationsHandler
                        hLogger
                        (tstDeleteCommentReq {requestMethod = methodGet}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed
                             "Commentary not deleted. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstDeleteCommentReq
                             {rawPathInfo = "/news/1/comments/delete_comments"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return message about successful deleting" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   commentsHandler
                                       { nchDeleteCommentFromDb =
                                             \_ _ -> return ()
                                       }
                             })
                        hLogger
                        tstDeleteCommentReq `shouldBe`
                    return (Right (OkMessage "Commentary deleted."))
                it "server should return error, because token is not admin" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   commentsHandler
                                       { nchDeleteCommentFromDb =
                                             \_ _ -> throwError NotAdmin
                                       }
                             })
                        hLogger
                        tstDeleteCommentReq `shouldBe`
                    return
                        (Left (Forbidden "Commentary not deleted. Not Admin."))
                it "server should return error, because token is bad" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   commentsHandler
                                       { nchDeleteCommentFromDb =
                                             \_ _ -> throwError BadToken
                                       }
                             })
                        hLogger
                        tstDeleteCommentReq `shouldBe`
                    return
                        (Left (Forbidden "Commentary not deleted. Bad Token."))
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { newsAndCommentsHandle =
                                   commentsHandler
                                       { nchDeleteCommentFromDb =
                                             \_ _ ->
                                                 throwError $ DatabaseError 0
                                       }
                             })
                        hLogger
                        tstDeleteCommentReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "Commentary not deleted. Database Error 0."))
