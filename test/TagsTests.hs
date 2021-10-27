{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module TagsTests where

import           Data.Functor.Identity      (Identity)
import           Database.PostgreSQL.Simple (Binary (Binary))
import           Logger                     (LoggerHandle (..),
                                             Priority (Debug))
import           Network.HTTP.Types         (methodDelete, methodGet,
                                             methodPost, methodPut)
import           Network.Wai                (Request (rawPathInfo, requestMethod),
                                             defaultRequest)
import           OperationsHandle           (OperationsHandle (..),
                                             TagsHandle (..))

import           Router                     (routes)
import           Test.Hspec                 (describe, hspec, it, shouldBe)
import           Types.Other                (ResponseErrorMessage (BadRequest, Forbidden, InternalServerError, MethodNotAllowed, NotFound),
                                             ResponseOkMessage (Created, OkJSON, OkMessage),
                                             SomeError (BadToken, DatabaseError, NotAdmin, OtherError))
import           Types.Tags                 (TagsList (TagsList))

hLogger :: LoggerHandle Identity
hLogger =
    LoggerHandle {priority = Debug, Logger.log = \prior message -> return ()}

tagsHandler :: TagsHandle Identity
tagsHandler =
    TagsHandle
        { create_tag_in_db =
              \token tag_name -> return $ Left $ OtherError "ErrorMessage"
        , delete_tag_from_db =
              \token tag_name -> return $ Left $ OtherError "ErrorMessage"
        , get_tags_list_from_db =
              \page -> return $ Left $ OtherError "ErrorMessage"
        , edit_tag_in_db =
              \token edit_tag -> return $ Left $ OtherError "ErrorMessage"
        , tags_logger = hLogger
        , tags_parse_request_body = \_ -> return ([], [])
        }

operationsHandler :: OperationsHandle Identity
operationsHandler = OperationsHandle {tags_handle = tagsHandler}

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
            describe "testing get_tags_list_from_db" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstGetTagsListReq `shouldBe`
                    return
                        (Left $
                         BadRequest "List of tags not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        (tstGetTagsListReq {requestMethod = methodPut}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstGetTagsListReq {rawPathInfo = "/tagsss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of tags, because all is good" $
                    routes
                        (operationsHandler
                             { tags_handle =
                                   tagsHandler
                                       { get_tags_list_from_db =
                                             \_ -> return $ Right (TagsList [])
                                       }
                             })
                        tstGetTagsListReq `shouldBe`
                    return (Right $ OkJSON "{\"tags\":[]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { tags_handle =
                                   tagsHandler
                                       { get_tags_list_from_db =
                                             \_ -> return $ Left DatabaseError
                                       }
                             })
                        tstGetTagsListReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "List of tags not sended. Database Error."))
{-
                                CREATE TAG TESTS
-}
            describe "testing create_tag_in_db" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstPostTagReq `shouldBe`
                    return (Left $ BadRequest "Tag not created. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        (tstPostTagReq {requestMethod = methodGet}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstPostTagReq {rawPathInfo = "/tags/create_tagasdasda"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return tag id, because all is good" $
                    routes
                        (operationsHandler
                             { tags_handle =
                                   tagsHandler
                                       { create_tag_in_db =
                                             \_ _ -> return $ Right 1
                                       }
                             })
                        tstPostTagReq `shouldBe`
                    return (Right (Created "1"))
                it "server should return error, because token is not admin" $
                    routes
                        (operationsHandler
                             { tags_handle =
                                   tagsHandler
                                       { create_tag_in_db =
                                             \_ _ -> return $ Left NotAdmin
                                       }
                             })
                        tstPostTagReq `shouldBe`
                    return (Left (Forbidden "Tag not created. Not Admin."))
                it "server should return error, because token is bad" $
                    routes
                        (operationsHandler
                             { tags_handle =
                                   tagsHandler
                                       { create_tag_in_db =
                                             \_ _ -> return $ Left BadToken
                                       }
                             })
                        tstPostTagReq `shouldBe`
                    return (Left (Forbidden "Tag not created. Bad Token."))
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { tags_handle =
                                   tagsHandler
                                       { create_tag_in_db =
                                             \_ _ -> return $ Left DatabaseError
                                       }
                             })
                        tstPostTagReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "Tag not created. Database Error."))
{-
                                DELETE TAG TESTS
-}
            describe "testing delete_tag_from_db" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstDeleteTagReq `shouldBe`
                    return (Left $ BadRequest "Tag not deleted. ErrorMessage")
                it
                    "server should return error, because request sended with bad requestMethod" $
                    routes
                        operationsHandler
                        (tstDeleteTagReq {requestMethod = methodGet}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstDeleteTagReq {rawPathInfo = "/tags/delete_tagsss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return message about successful deleting" $
                    routes
                        (operationsHandler
                             { tags_handle =
                                   tagsHandler
                                       { delete_tag_from_db =
                                             \_ _ -> return $ Right ()
                                       }
                             })
                        tstDeleteTagReq `shouldBe`
                    return (Right (OkMessage "Tag deleted."))
                it "server should return error, because token is not admin" $
                    routes
                        (operationsHandler
                             { tags_handle =
                                   tagsHandler
                                       { delete_tag_from_db =
                                             \_ _ -> return $ Left NotAdmin
                                       }
                             })
                        tstDeleteTagReq `shouldBe`
                    return (Left (Forbidden "Tag not deleted. Not Admin."))
                it "server should return error, because token is bad" $
                    routes
                        (operationsHandler
                             { tags_handle =
                                   tagsHandler
                                       { delete_tag_from_db =
                                             \_ _ -> return $ Left BadToken
                                       }
                             })
                        tstDeleteTagReq `shouldBe`
                    return (Left (Forbidden "Tag not deleted. Bad Token."))
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { tags_handle =
                                   tagsHandler
                                       { delete_tag_from_db =
                                             \_ _ -> return $ Left DatabaseError
                                       }
                             })
                        tstDeleteTagReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "Tag not deleted. Database Error."))
{-
                                EDIT AUTHOR TESTS
-}
            describe "testing edit_tag_in_db" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstUpdateTagReq `shouldBe`
                    return (Left $ BadRequest "Tag not edited. ErrorMessage")
                it
                    "server should return error, because request sended with bad requestMethod" $
                    routes
                        operationsHandler
                        (tstUpdateTagReq {requestMethod = methodGet}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstUpdateTagReq {rawPathInfo = "/tags/edit_tagsssss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return message about successful editing" $
                    routes
                        (operationsHandler
                             { tags_handle =
                                   tagsHandler
                                       { edit_tag_in_db =
                                             \_ _ -> return $ Right ()
                                       }
                             })
                        tstUpdateTagReq `shouldBe`
                    return (Right (OkMessage "Tag edited."))
                it "server should return error, because token is not admin" $
                    routes
                        (operationsHandler
                             { tags_handle =
                                   tagsHandler
                                       { edit_tag_in_db =
                                             \_ _ -> return $ Left NotAdmin
                                       }
                             })
                        tstUpdateTagReq `shouldBe`
                    return (Left (Forbidden "Tag not edited. Not Admin."))
                it "server should return error, because token is bad" $
                    routes
                        (operationsHandler
                             { tags_handle =
                                   tagsHandler
                                       { edit_tag_in_db =
                                             \_ _ -> return $ Left BadToken
                                       }
                             })
                        tstUpdateTagReq `shouldBe`
                    return (Left (Forbidden "Tag not edited. Bad Token."))
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { tags_handle =
                                   tagsHandler
                                       { edit_tag_in_db =
                                             \_ _ -> return $ Left DatabaseError
                                       }
                             })
                        tstUpdateTagReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "Tag not edited. Database Error."))
