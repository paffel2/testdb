{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module CategoriesTests where

import           Data.Functor.Identity (Identity)
import           Logger                (LoggerHandle (..), Priority (Debug))

import           Control.Monad.Except  (ExceptT, MonadError (throwError),
                                        MonadIO)
import           Network.HTTP.Types    (methodDelete, methodGet, methodPost,
                                        methodPut)
import           Network.Wai           (Request (rawPathInfo, requestMethod),
                                        defaultRequest)
import           OperationsHandle      (CategoriesHandle (..),
                                        OperationsHandle (..))
import           Router                (routes)
import           Test.Hspec            (describe, hspec, it, shouldBe)
import           Types.Categories      (ListOfCategories (ListOfCategories))
import           Types.Other           (ResponseErrorMessage (BadRequest, Forbidden, InternalServerError, MethodNotAllowed, NotFound),
                                        ResponseOkMessage (Created, OkJSON, OkMessage),
                                        SomeError (BadToken, DatabaseError, NotAdmin, OtherError))

instance MonadIO Identity

hLogger :: LoggerHandle Identity
hLogger =
    LoggerHandle {priority = Debug, Logger.log = \prior message -> return ()}

categoriesHandler :: CategoriesHandle (ExceptT SomeError Identity)
categoriesHandler =
    CategoriesHandle
        { chGetCategoriesListFromDb =
              \page -> throwError $ OtherError "ErrorMessage"
        , chCreateCategoryOnDb =
              \token create_category -> throwError $ OtherError "ErrorMessage"
        , chDeleteCategoryFromDb =
              \token category_name -> throwError $ OtherError "ErrorMessage"
        , chEditCategoryOnDb =
              \token edit_category -> throwError $ OtherError "ErrorMessage"
        , chParseRequestBody = \_ -> return ([], [])
        }

operationsHandler :: OperationsHandle (ExceptT SomeError Identity)
operationsHandler = OperationsHandle {categoriesHandle = categoriesHandler}

tstGetCategoriesListReq :: Request
tstGetCategoriesListReq =
    defaultRequest {rawPathInfo = "/categories", requestMethod = methodGet}

tstPostCategoryReq :: Request
tstPostCategoryReq =
    defaultRequest
        { rawPathInfo = "/categories/create_category"
        , requestMethod = methodPost
        }

tstDeleteCategoryReq :: Request
tstDeleteCategoryReq =
    defaultRequest
        { rawPathInfo = "/categories/delete_category"
        , requestMethod = methodDelete
        }

tstUpdateCategoryReq :: Request
tstUpdateCategoryReq =
    defaultRequest
        {rawPathInfo = "/categories/edit_category", requestMethod = methodPut}

categoriesTests :: IO ()
categoriesTests =
    hspec $ do
        describe "testing categories functions" $ do
            describe "testing chGetCategoriesListFromDb" $ do
                it "server should return error because something happend" $
                    routes operationsHandler hLogger tstGetCategoriesListReq `shouldBe`
                    return
                        (Left $
                         BadRequest
                             "List of categories not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetCategoriesListReq {requestMethod = methodPut}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed
                             "List of categories not sended. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetCategoriesListReq {rawPathInfo = "/authorsss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of authors, because all is good" $
                    routes
                        (operationsHandler
                             { categoriesHandle =
                                   categoriesHandler
                                       { chGetCategoriesListFromDb =
                                             \_ -> return (ListOfCategories [])
                                       }
                             })
                        hLogger
                        tstGetCategoriesListReq `shouldBe`
                    return (Right $ OkJSON "{\"list_of_categories\":[]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { categoriesHandle =
                                   categoriesHandler
                                       { chGetCategoriesListFromDb =
                                             \_ -> throwError DatabaseError
                                       }
                             })
                        hLogger
                        tstGetCategoriesListReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "List of categories not sended. Database Error."))
{-
                                CREATE CATEGORIES TESTS
-}
            describe "testing chCreateCategoryOnDb" $ do
                it "server should return error because something happend" $
                    routes operationsHandler hLogger tstPostCategoryReq `shouldBe`
                    return
                        (Left $ BadRequest "Category not created. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        hLogger
                        (tstPostCategoryReq {requestMethod = methodGet}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed
                             "Category not created. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstPostCategoryReq
                             { rawPathInfo =
                                   "/categories/create_categoryasdasda"
                             }) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return category id, because all is good" $
                    routes
                        (operationsHandler
                             { categoriesHandle =
                                   categoriesHandler
                                       {chCreateCategoryOnDb = \_ _ -> return 1}
                             })
                        hLogger
                        tstPostCategoryReq `shouldBe`
                    return (Right (Created "1"))
                it "server should return error, because token is not admin" $
                    routes
                        (operationsHandler
                             { categoriesHandle =
                                   categoriesHandler
                                       { chCreateCategoryOnDb =
                                             \_ _ -> throwError NotAdmin
                                       }
                             })
                        hLogger
                        tstPostCategoryReq `shouldBe`
                    return (Left (Forbidden "Category not created. Not Admin."))
                it "server should return error, because token is bad" $
                    routes
                        (operationsHandler
                             { categoriesHandle =
                                   categoriesHandler
                                       { chCreateCategoryOnDb =
                                             \_ _ -> throwError BadToken
                                       }
                             })
                        hLogger
                        tstPostCategoryReq `shouldBe`
                    return (Left (Forbidden "Category not created. Bad Token."))
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { categoriesHandle =
                                   categoriesHandler
                                       { chCreateCategoryOnDb =
                                             \_ _ -> throwError DatabaseError
                                       }
                             })
                        hLogger
                        tstPostCategoryReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "Category not created. Database Error."))
{-
                                DELETE CATEGORY TESTS
-}
            describe "testing chDeleteCategoryFromDb" $ do
                it "server should return error because something happend" $
                    routes operationsHandler hLogger tstDeleteCategoryReq `shouldBe`
                    return
                        (Left $ BadRequest "Category not deleted. ErrorMessage")
                it
                    "server should return error, because request sended with bad requestMethod" $
                    routes
                        operationsHandler
                        hLogger
                        (tstDeleteCategoryReq {requestMethod = methodGet}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed
                             "Category not deleted. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstDeleteCategoryReq
                             {rawPathInfo = "/categories/delete_aythor"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return message about successful deleting" $
                    routes
                        (operationsHandler
                             { categoriesHandle =
                                   categoriesHandler
                                       { chDeleteCategoryFromDb =
                                             \_ _ -> return ()
                                       }
                             })
                        hLogger
                        tstDeleteCategoryReq `shouldBe`
                    return (Right (OkMessage "Category deleted."))
                it "server should return error, because token is not admin" $
                    routes
                        (operationsHandler
                             { categoriesHandle =
                                   categoriesHandler
                                       { chDeleteCategoryFromDb =
                                             \_ _ -> throwError NotAdmin
                                       }
                             })
                        hLogger
                        tstDeleteCategoryReq `shouldBe`
                    return (Left (Forbidden "Category not deleted. Not Admin."))
                it "server should return error, because token is bad" $
                    routes
                        (operationsHandler
                             { categoriesHandle =
                                   categoriesHandler
                                       { chDeleteCategoryFromDb =
                                             \_ _ -> throwError BadToken
                                       }
                             })
                        hLogger
                        tstDeleteCategoryReq `shouldBe`
                    return (Left (Forbidden "Category not deleted. Bad Token."))
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { categoriesHandle =
                                   categoriesHandler
                                       { chDeleteCategoryFromDb =
                                             \_ _ -> throwError DatabaseError
                                       }
                             })
                        hLogger
                        tstDeleteCategoryReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "Category not deleted. Database Error."))
{-
                                EDIT CATEGORY TESTS
-}
            describe "testing chEditCategoryOnDb" $ do
                it "server should return error because something happend" $
                    routes operationsHandler hLogger tstUpdateCategoryReq `shouldBe`
                    return
                        (Left $ BadRequest "Category not edited. ErrorMessage")
                it
                    "server should return error, because request sended with bad requestMethod" $
                    routes
                        operationsHandler
                        hLogger
                        (tstUpdateCategoryReq {requestMethod = methodGet}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed
                             "Category not edited. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstUpdateCategoryReq
                             {rawPathInfo = "/categories/edit_aythor"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return message about successful delting" $
                    routes
                        (operationsHandler
                             { categoriesHandle =
                                   categoriesHandler
                                       {chEditCategoryOnDb = \_ _ -> return ()}
                             })
                        hLogger
                        tstUpdateCategoryReq `shouldBe`
                    return (Right (OkMessage "Category edited."))
                it "server should return error, because token is not admin" $
                    routes
                        (operationsHandler
                             { categoriesHandle =
                                   categoriesHandler
                                       { chEditCategoryOnDb =
                                             \_ _ -> throwError NotAdmin
                                       }
                             })
                        hLogger
                        tstUpdateCategoryReq `shouldBe`
                    return (Left (Forbidden "Category not edited. Not Admin."))
                it "server should return error, because token is bad" $
                    routes
                        (operationsHandler
                             { categoriesHandle =
                                   categoriesHandler
                                       { chEditCategoryOnDb =
                                             \_ _ -> throwError BadToken
                                       }
                             })
                        hLogger
                        tstUpdateCategoryReq `shouldBe`
                    return (Left (Forbidden "Category not edited. Bad Token."))
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { categoriesHandle =
                                   categoriesHandler
                                       { chEditCategoryOnDb =
                                             \_ _ -> throwError DatabaseError
                                       }
                             })
                        hLogger
                        tstUpdateCategoryReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "Category not edited. Database Error."))
