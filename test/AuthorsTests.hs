{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module AuthorsTests where

import           Data.Functor.Identity (Identity)

import           Logger                (LoggerHandle (..), Priority (Debug))
import           Network.HTTP.Types    (methodDelete, methodGet, methodPost,
                                        methodPut)
import           Network.Wai           (Request (queryString, rawPathInfo, requestBody, requestMethod),
                                        defaultRequest)
import           OperationsHandle      (AuthorsHandle (..),
                                        CategoriesHandle (..),
                                        DraftsHandle (..), ImagesHandle (..),
                                        NewsAndCommentsHandle (..),
                                        OperationsHandle (..), TagsHandle (..),
                                        UsersHandle (..))
import           Router                (routes)
import           Test.Hspec            (describe, hspec, it, shouldBe)
import           Types.Authors         (AuthorsList (AuthorsList))

import           Types.Other           (ResponseErrorMessage (BadRequest, Forbidden, InternalServerError, MethodNotAllowed, NotFound),
                                        ResponseOkMessage (Created, OkJSON, OkMessage),
                                        SomeError (BadToken, DatabaseError, NotAdmin, OtherError))

hLogger :: LoggerHandle Identity
hLogger =
    LoggerHandle {priority = Debug, Logger.log = \prior message -> return ()}

authorsHandler :: AuthorsHandle Identity
authorsHandler =
    AuthorsHandle
        { create_author_in_db =
              \token create_author -> return $ Left $ OtherError "ErrorMessage"
        , delete_author_in_db =
              \token author_login -> return $ Left $ OtherError "ErrorMessage"
        , get_authors_list = \page -> return $ Left $ OtherError "ErrorMessage"
        , edit_author_in_db =
              \token edit_author -> return $ Left $ OtherError "ErrorMessage"
        , authors_logger = hLogger
        , authors_parse_request_body = \request -> return ([], [])
        }

operationsHandler :: OperationsHandle Identity
operationsHandler = OperationsHandle {authors_handle = authorsHandler}

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
            describe "testing get_authors_list" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstGetAuthorsListReq `shouldBe`
                    return
                        (Left $
                         BadRequest "List of authors not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        (tstGetAuthorsListReq {requestMethod = methodPut}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstGetAuthorsListReq {rawPathInfo = "/authorsss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of authors, because all is good" $
                    routes
                        (operationsHandler
                             { authors_handle =
                                   authorsHandler
                                       { get_authors_list =
                                             \_ ->
                                                 return $ Right (AuthorsList [])
                                       }
                             })
                        tstGetAuthorsListReq `shouldBe`
                    return (Right $ OkJSON "{\"authors\":[]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { authors_handle =
                                   authorsHandler
                                       { get_authors_list =
                                             \_ -> return $ Left DatabaseError
                                       }
                             })
                        tstGetAuthorsListReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "List of authors not sended. Database Error."))
{-
                                CREATE AUTHOR TESTS
-}
            describe "testing create_author_in_db" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstPostAuthorReq `shouldBe`
                    return
                        (Left $ BadRequest "Author not created. ErrorMessage")
                it
                    "server should return error, because request sended with bad requestMethod" $
                    routes
                        operationsHandler
                        (tstPostAuthorReq {requestMethod = methodGet}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstPostAuthorReq {rawPathInfo = "/authors/asdasdasd"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return authors id, because all is good" $
                    routes
                        (operationsHandler
                             { authors_handle =
                                   authorsHandler
                                       { create_author_in_db =
                                             \_ _ -> return $ Right 1
                                       }
                             })
                        tstPostAuthorReq `shouldBe`
                    return (Right (Created "1"))
                it "server should return error, because token is not admin" $
                    routes
                        (operationsHandler
                             { authors_handle =
                                   authorsHandler
                                       { create_author_in_db =
                                             \_ _ -> return $ Left NotAdmin
                                       }
                             })
                        tstPostAuthorReq `shouldBe`
                    return (Left (Forbidden "Author not created. Not Admin."))
                it "server should return error, because token is bad" $
                    routes
                        (operationsHandler
                             { authors_handle =
                                   authorsHandler
                                       { create_author_in_db =
                                             \_ _ -> return $ Left BadToken
                                       }
                             })
                        tstPostAuthorReq `shouldBe`
                    return (Left (Forbidden "Author not created. Bad Token."))
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { authors_handle =
                                   authorsHandler
                                       { create_author_in_db =
                                             \_ _ -> return $ Left DatabaseError
                                       }
                             })
                        tstPostAuthorReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "Author not created. Database Error."))
{-
                                DELETE AUTHOR TESTS
-}
            describe "testing delete_author_in_db" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstDeleteAuthorReq `shouldBe`
                    return
                        (Left $ BadRequest "Author not deleted. ErrorMessage")
                it
                    "server should return error, because request sended with bad requestMethod" $
                    routes
                        operationsHandler
                        (tstDeleteAuthorReq {requestMethod = methodGet}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstDeleteAuthorReq
                             {rawPathInfo = "/authors/delete_aythor"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return message about successful delting" $
                    routes
                        (operationsHandler
                             { authors_handle =
                                   authorsHandler
                                       { delete_author_in_db =
                                             \_ _ -> return $ Right ()
                                       }
                             })
                        tstDeleteAuthorReq `shouldBe`
                    return (Right (OkMessage "Author deleted."))
                it "server should return error, because token is not admin" $
                    routes
                        (operationsHandler
                             { authors_handle =
                                   authorsHandler
                                       { delete_author_in_db =
                                             \_ _ -> return $ Left NotAdmin
                                       }
                             })
                        tstDeleteAuthorReq `shouldBe`
                    return (Left (Forbidden "Author not deleted. Not Admin."))
                it "server should return error, because token is bad" $
                    routes
                        (operationsHandler
                             { authors_handle =
                                   authorsHandler
                                       { delete_author_in_db =
                                             \_ _ -> return $ Left BadToken
                                       }
                             })
                        tstDeleteAuthorReq `shouldBe`
                    return (Left (Forbidden "Author not deleted. Bad Token."))
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { authors_handle =
                                   authorsHandler
                                       { delete_author_in_db =
                                             \_ _ -> return $ Left DatabaseError
                                       }
                             })
                        tstDeleteAuthorReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "Author not deleted. Database Error."))
{-
                                EDIT AUTHOR TESTS
-}
            describe "testing edit_author_in_db" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstUpdateAuthorReq `shouldBe`
                    return (Left $ BadRequest "Author not edited. ErrorMessage")
                it
                    "server should return error, because request sended with bad requestMethod" $
                    routes
                        operationsHandler
                        (tstUpdateAuthorReq {requestMethod = methodGet}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstUpdateAuthorReq
                             {rawPathInfo = "/authors/edit_aythor"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return message about successful delting" $
                    routes
                        (operationsHandler
                             { authors_handle =
                                   authorsHandler
                                       { edit_author_in_db =
                                             \_ _ -> return $ Right ()
                                       }
                             })
                        tstUpdateAuthorReq `shouldBe`
                    return (Right (OkMessage "Author edited."))
                it "server should return error, because token is not admin" $
                    routes
                        (operationsHandler
                             { authors_handle =
                                   authorsHandler
                                       { edit_author_in_db =
                                             \_ _ -> return $ Left NotAdmin
                                       }
                             })
                        tstUpdateAuthorReq `shouldBe`
                    return (Left (Forbidden "Author not edited. Not Admin."))
                it "server should return error, because token is bad" $
                    routes
                        (operationsHandler
                             { authors_handle =
                                   authorsHandler
                                       { edit_author_in_db =
                                             \_ _ -> return $ Left BadToken
                                       }
                             })
                        tstUpdateAuthorReq `shouldBe`
                    return (Left (Forbidden "Author not edited. Bad Token."))
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { authors_handle =
                                   authorsHandler
                                       { edit_author_in_db =
                                             \_ _ -> return $ Left DatabaseError
                                       }
                             })
                        tstUpdateAuthorReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "Author not edited. Database Error."))
