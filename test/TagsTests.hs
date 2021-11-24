{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module TagsTests where

import           Control.Monad.Except  (ExceptT, MonadError (throwError),
                                        MonadIO)
import           Data.Functor.Identity (Identity)
import           Logger                (LoggerHandle (..), Priority (Debug))
import           Network.HTTP.Types    (methodDelete, methodGet, methodPost,
                                        methodPut)
import           Network.Wai           (Request (rawPathInfo, requestMethod),
                                        defaultRequest)
import           OperationsHandle      (OperationsHandle (..), TagsHandle (..))

import           Router                (routes)
import           Test.Hspec            (describe, hspec, it, shouldBe)
import           Types.Other           (ResponseErrorMessage (BadRequest, Forbidden, InternalServerError, MethodNotAllowed, NotFound),
                                        ResponseOkMessage (Created, OkJSON, OkMessage),
                                        SomeError (BadToken, DatabaseError, NotAdmin, OtherError))
import           Types.Tags            (TagsList (TagsList))

instance MonadIO Identity

hLogger :: LoggerHandle Identity
hLogger =
    LoggerHandle {priority = Debug, Logger.log = \prior message -> return ()}

tagsHandler :: TagsHandle (ExceptT SomeError Identity)
tagsHandler =
    TagsHandle
        { thCreateTagInDb =
              \token tag_name -> throwError $ OtherError "ErrorMessage"
        , thDeleteTagFromDb =
              \token tag_name -> throwError $ OtherError "ErrorMessage"
        , thGetTagsListFromDb = \page -> throwError $ OtherError "ErrorMessage"
        , thEditTagInDb =
              \token edit_tag -> throwError $ OtherError "ErrorMessage"
        , thParseRequestBody = \_ -> return ([], [])
        }

operationsHandler :: OperationsHandle (ExceptT SomeError Identity)
operationsHandler = OperationsHandle {tagsHandle = tagsHandler}

tstGetTagsListReq :: Request
tstGetTagsListReq =
    defaultRequest {rawPathInfo = "/tags", requestMethod = methodGet}

tstPostTagReq :: Request
tstPostTagReq =
    defaultRequest
        {rawPathInfo = "/tags/create_tag", requestMethod = methodPost}

tstDeleteTagReq :: Request
tstDeleteTagReq =
    defaultRequest
        {rawPathInfo = "/tags/delete_tag", requestMethod = methodDelete}

tstUpdateTagReq :: Request
tstUpdateTagReq =
    defaultRequest {rawPathInfo = "/tags/edit_tag", requestMethod = methodPut}

tagsTests :: IO ()
tagsTests =
    hspec $ do
        describe "testing tags functions" $ do
            describe "testing thGetTagsListFromDb" $ do
                it "server should return error because something happend" $
                    routes operationsHandler hLogger tstGetTagsListReq `shouldBe`
                    return
                        (Left $
                         BadRequest "List of tags not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetTagsListReq {requestMethod = methodPut}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed
                             "List of tags not sended. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetTagsListReq {rawPathInfo = "/tagsss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of tags, because all is good" $
                    routes
                        (operationsHandler
                             { tagsHandle =
                                   tagsHandler
                                       { thGetTagsListFromDb =
                                             \_ -> return (TagsList [])
                                       }
                             })
                        hLogger
                        tstGetTagsListReq `shouldBe`
                    return (Right $ OkJSON "{\"tags\":[]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { tagsHandle =
                                   tagsHandler
                                       { thGetTagsListFromDb =
                                             \_ -> throwError DatabaseError
                                       }
                             })
                        hLogger
                        tstGetTagsListReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "List of tags not sended. Database Error."))
{-
                                CREATE TAG TESTS
-}
            describe "testing thCreateTagInDb" $ do
                it "server should return error because something happend" $
                    routes operationsHandler hLogger tstPostTagReq `shouldBe`
                    return (Left $ BadRequest "Tag not created. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        hLogger
                        (tstPostTagReq {requestMethod = methodGet}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed "Tag not created. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstPostTagReq {rawPathInfo = "/tags/create_tagasdasda"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return tag id, because all is good" $
                    routes
                        (operationsHandler
                             { tagsHandle =
                                   tagsHandler
                                       {thCreateTagInDb = \_ _ -> return 1}
                             })
                        hLogger
                        tstPostTagReq `shouldBe`
                    return (Right (Created "1"))
                it "server should return error, because token is not admin" $
                    routes
                        (operationsHandler
                             { tagsHandle =
                                   tagsHandler
                                       { thCreateTagInDb =
                                             \_ _ -> throwError NotAdmin
                                       }
                             })
                        hLogger
                        tstPostTagReq `shouldBe`
                    return (Left (Forbidden "Tag not created. Not Admin."))
                it "server should return error, because token is bad" $
                    routes
                        (operationsHandler
                             { tagsHandle =
                                   tagsHandler
                                       { thCreateTagInDb =
                                             \_ _ -> throwError BadToken
                                       }
                             })
                        hLogger
                        tstPostTagReq `shouldBe`
                    return (Left (Forbidden "Tag not created. Bad Token."))
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { tagsHandle =
                                   tagsHandler
                                       { thCreateTagInDb =
                                             \_ _ -> throwError DatabaseError
                                       }
                             })
                        hLogger
                        tstPostTagReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "Tag not created. Database Error."))
{-
                                DELETE TAG TESTS
-}
            describe "testing thDeleteTagFromDb" $ do
                it "server should return error because something happend" $
                    routes operationsHandler hLogger tstDeleteTagReq `shouldBe`
                    return (Left $ BadRequest "Tag not deleted. ErrorMessage")
                it
                    "server should return error, because request sended with bad requestMethod" $
                    routes
                        operationsHandler
                        hLogger
                        (tstDeleteTagReq {requestMethod = methodGet}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed "Tag not deleted. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstDeleteTagReq {rawPathInfo = "/tags/delete_tagsss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return message about successful deleting" $
                    routes
                        (operationsHandler
                             { tagsHandle =
                                   tagsHandler
                                       {thDeleteTagFromDb = \_ _ -> return ()}
                             })
                        hLogger
                        tstDeleteTagReq `shouldBe`
                    return (Right (OkMessage "Tag deleted."))
                it "server should return error, because token is not admin" $
                    routes
                        (operationsHandler
                             { tagsHandle =
                                   tagsHandler
                                       { thDeleteTagFromDb =
                                             \_ _ -> throwError NotAdmin
                                       }
                             })
                        hLogger
                        tstDeleteTagReq `shouldBe`
                    return (Left (Forbidden "Tag not deleted. Not Admin."))
                it "server should return error, because token is bad" $
                    routes
                        (operationsHandler
                             { tagsHandle =
                                   tagsHandler
                                       { thDeleteTagFromDb =
                                             \_ _ -> throwError BadToken
                                       }
                             })
                        hLogger
                        tstDeleteTagReq `shouldBe`
                    return (Left (Forbidden "Tag not deleted. Bad Token."))
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { tagsHandle =
                                   tagsHandler
                                       { thDeleteTagFromDb =
                                             \_ _ -> throwError DatabaseError
                                       }
                             })
                        hLogger
                        tstDeleteTagReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "Tag not deleted. Database Error."))
{-
                                EDIT AUTHOR TESTS
-}
            describe "testing thEditTagInDb" $ do
                it "server should return error because something happend" $
                    routes operationsHandler hLogger tstUpdateTagReq `shouldBe`
                    return (Left $ BadRequest "Tag not edited. ErrorMessage")
                it
                    "server should return error, because request sended with bad requestMethod" $
                    routes
                        operationsHandler
                        hLogger
                        (tstUpdateTagReq {requestMethod = methodGet}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed "Tag not edited. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstUpdateTagReq {rawPathInfo = "/tags/edit_tagsssss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return message about successful editing" $
                    routes
                        (operationsHandler
                             { tagsHandle =
                                   tagsHandler
                                       {thEditTagInDb = \_ _ -> return ()}
                             })
                        hLogger
                        tstUpdateTagReq `shouldBe`
                    return (Right (OkMessage "Tag edited."))
                it "server should return error, because token is not admin" $
                    routes
                        (operationsHandler
                             { tagsHandle =
                                   tagsHandler
                                       { thEditTagInDb =
                                             \_ _ -> throwError NotAdmin
                                       }
                             })
                        hLogger
                        tstUpdateTagReq `shouldBe`
                    return (Left (Forbidden "Tag not edited. Not Admin."))
                it "server should return error, because token is bad" $
                    routes
                        (operationsHandler
                             { tagsHandle =
                                   tagsHandler
                                       { thEditTagInDb =
                                             \_ _ -> throwError BadToken
                                       }
                             })
                        hLogger
                        tstUpdateTagReq `shouldBe`
                    return (Left (Forbidden "Tag not edited. Bad Token."))
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { tagsHandle =
                                   tagsHandler
                                       { thEditTagInDb =
                                             \_ _ -> throwError DatabaseError
                                       }
                             })
                        hLogger
                        tstUpdateTagReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "Tag not edited. Database Error."))
