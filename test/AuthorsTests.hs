{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module AuthorsTests where

import           Data.Functor.Identity (Identity)

import           Logger                (LoggerHandle (..), Priority (Debug))
import           Network.HTTP.Types    (methodDelete, methodGet, methodPost,
                                        methodPut)
import           Network.Wai           (Request (rawPathInfo, requestMethod),
                                        defaultRequest)
import           OperationsHandle      (AuthorsHandle (..),
                                        OperationsHandle (..))
import           Router                (routes)
import           Test.Hspec            (describe, hspec, it, shouldBe)
import           Types.Authors         (AuthorsList (AuthorsList))

import           Control.Monad.Except
import           Types.Other           (ResponseErrorMessage (BadRequest, Forbidden, InternalServerError, MethodNotAllowed, NotFound),
                                        ResponseOkMessage (Created, OkJSON, OkMessage),
                                        SomeError (BadToken, DatabaseError, NotAdmin, OtherError))

instance MonadIO Identity

hLogger :: LoggerHandle Identity
hLogger =
    LoggerHandle {priority = Debug, Logger.log = \prior message -> return ()}

authorsHandler :: AuthorsHandle (ExceptT SomeError Identity)
authorsHandler =
    AuthorsHandle
        { ahCreateAuthorInDb =
              \token create_author -> throwError $ OtherError "ErrorMessage"
        , ahDeleteAuthorInDb =
              \token author_login -> throwError $ OtherError "ErrorMessage"
        , ahGetAuthorsList = \page -> throwError $ OtherError "ErrorMessage"
        , ahEditAuthorInDb =
              \token edit_author -> throwError $ OtherError "ErrorMessage"
        , ahParseRequestBody = \request -> return ([], [])
        }

operationsHandler :: OperationsHandle (ExceptT SomeError Identity)
operationsHandler = OperationsHandle {authorsHandle = authorsHandler}

tstGetAuthorsListReq :: Request
tstGetAuthorsListReq =
    defaultRequest {rawPathInfo = "/authors", requestMethod = methodGet}

tstPostAuthorReq :: Request
tstPostAuthorReq =
    defaultRequest
        {rawPathInfo = "/authors/create_author", requestMethod = methodPost}

tstDeleteAuthorReq :: Request
tstDeleteAuthorReq =
    defaultRequest
        {rawPathInfo = "/authors/delete_author", requestMethod = methodDelete}

tstUpdateAuthorReq :: Request
tstUpdateAuthorReq =
    defaultRequest
        {rawPathInfo = "/authors/edit_author", requestMethod = methodPut}

authorsTests :: IO ()
authorsTests =
    hspec $ do
        describe "testing authors functions" $ do
            describe "testing ahGetAuthorsList" $ do
                it "server should return error because something happend" $
                    routes operationsHandler hLogger tstGetAuthorsListReq `shouldBe`
                    return
                        (Left $
                         BadRequest "List of authors not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetAuthorsListReq {requestMethod = methodPut}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed
                             "List of authors not sended. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetAuthorsListReq {rawPathInfo = "/authorsss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of authors, because all is good" $
                    routes
                        (operationsHandler
                             { authorsHandle =
                                   authorsHandler
                                       { ahGetAuthorsList =
                                             \_ -> return (AuthorsList [])
                                       }
                             })
                        hLogger
                        tstGetAuthorsListReq `shouldBe`
                    return (Right $ OkJSON "{\"authors\":[]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { authorsHandle =
                                   authorsHandler
                                       { ahGetAuthorsList =
                                             \_ -> throwError DatabaseError
                                       }
                             })
                        hLogger
                        tstGetAuthorsListReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "List of authors not sended. Database Error."))
{-
                                CREATE AUTHOR TESTS
-}
            describe "testing ahCreateAuthorInDb" $ do
                it "server should return error because something happend" $
                    routes operationsHandler hLogger tstPostAuthorReq `shouldBe`
                    return
                        (Left $ BadRequest "Author not created. ErrorMessage")
                it
                    "server should return error, because request sended with bad requestMethod" $
                    routes
                        operationsHandler
                        hLogger
                        (tstPostAuthorReq {requestMethod = methodGet}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed
                             "Author not created. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstPostAuthorReq {rawPathInfo = "/authors/asdasdasd"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return authors id, because all is good" $
                    routes
                        (operationsHandler
                             { authorsHandle =
                                   authorsHandler
                                       {ahCreateAuthorInDb = \_ _ -> return 1}
                             })
                        hLogger
                        tstPostAuthorReq `shouldBe`
                    return (Right (Created "1"))
                it "server should return error, because token is not admin" $
                    routes
                        (operationsHandler
                             { authorsHandle =
                                   authorsHandler
                                       { ahCreateAuthorInDb =
                                             \_ _ -> throwError NotAdmin
                                       }
                             })
                        hLogger
                        tstPostAuthorReq `shouldBe`
                    return (Left (Forbidden "Author not created. Not Admin."))
                it "server should return error, because token is bad" $
                    routes
                        (operationsHandler
                             { authorsHandle =
                                   authorsHandler
                                       { ahCreateAuthorInDb =
                                             \_ _ -> throwError BadToken
                                       }
                             })
                        hLogger
                        tstPostAuthorReq `shouldBe`
                    return (Left (Forbidden "Author not created. Bad Token."))
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { authorsHandle =
                                   authorsHandler
                                       { ahCreateAuthorInDb =
                                             \_ _ -> throwError DatabaseError
                                       }
                             })
                        hLogger
                        tstPostAuthorReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "Author not created. Database Error."))
{-
                                DELETE AUTHOR TESTS
-}
            describe "testing ahDeleteAuthorInDb" $ do
                it "server should return error because something happend" $
                    routes operationsHandler hLogger tstDeleteAuthorReq `shouldBe`
                    return
                        (Left $ BadRequest "Author not deleted. ErrorMessage")
                it
                    "server should return error, because request sended with bad requestMethod" $
                    routes
                        operationsHandler
                        hLogger
                        (tstDeleteAuthorReq {requestMethod = methodGet}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed
                             "Author not deleted. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstDeleteAuthorReq
                             {rawPathInfo = "/authors/delete_aythor"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return message about successful delting" $
                    routes
                        (operationsHandler
                             { authorsHandle =
                                   authorsHandler
                                       {ahDeleteAuthorInDb = \_ _ -> return ()}
                             })
                        hLogger
                        tstDeleteAuthorReq `shouldBe`
                    return (Right (OkMessage "Author deleted."))
                it "server should return error, because token is not admin" $
                    routes
                        (operationsHandler
                             { authorsHandle =
                                   authorsHandler
                                       { ahDeleteAuthorInDb =
                                             \_ _ -> throwError NotAdmin
                                       }
                             })
                        hLogger
                        tstDeleteAuthorReq `shouldBe`
                    return (Left (Forbidden "Author not deleted. Not Admin."))
                it "server should return error, because token is bad" $
                    routes
                        (operationsHandler
                             { authorsHandle =
                                   authorsHandler
                                       { ahDeleteAuthorInDb =
                                             \_ _ -> throwError BadToken
                                       }
                             })
                        hLogger
                        tstDeleteAuthorReq `shouldBe`
                    return (Left (Forbidden "Author not deleted. Bad Token."))
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { authorsHandle =
                                   authorsHandler
                                       { ahDeleteAuthorInDb =
                                             \_ _ -> throwError DatabaseError
                                       }
                             })
                        hLogger
                        tstDeleteAuthorReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "Author not deleted. Database Error."))
{-
                                EDIT AUTHOR TESTS
-}
            describe "testing ahEditAuthorInDb" $ do
                it "server should return error because something happend" $
                    routes operationsHandler hLogger tstUpdateAuthorReq `shouldBe`
                    return (Left $ BadRequest "Author not edited. ErrorMessage")
                it
                    "server should return error, because request sended with bad requestMethod" $
                    routes
                        operationsHandler
                        hLogger
                        (tstUpdateAuthorReq {requestMethod = methodGet}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed
                             "Author not edited. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstUpdateAuthorReq
                             {rawPathInfo = "/authors/edit_aythor"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return message about successful editing" $
                    routes
                        (operationsHandler
                             { authorsHandle =
                                   authorsHandler
                                       {ahEditAuthorInDb = \_ _ -> return ()}
                             })
                        hLogger
                        tstUpdateAuthorReq `shouldBe`
                    return (Right (OkMessage "Author edited."))
                it "server should return error, because token is not admin" $
                    routes
                        (operationsHandler
                             { authorsHandle =
                                   authorsHandler
                                       { ahEditAuthorInDb =
                                             \_ _ -> throwError NotAdmin
                                       }
                             })
                        hLogger
                        tstUpdateAuthorReq `shouldBe`
                    return (Left (Forbidden "Author not edited. Not Admin."))
                it "server should return error, because token is bad" $
                    routes
                        (operationsHandler
                             { authorsHandle =
                                   authorsHandler
                                       { ahEditAuthorInDb =
                                             \_ _ -> throwError BadToken
                                       }
                             })
                        hLogger
                        tstUpdateAuthorReq `shouldBe`
                    return (Left (Forbidden "Author not edited. Bad Token."))
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { authorsHandle =
                                   authorsHandler
                                       { ahEditAuthorInDb =
                                             \_ _ -> throwError DatabaseError
                                       }
                             })
                        hLogger
                        tstUpdateAuthorReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "Author not edited. Database Error."))
