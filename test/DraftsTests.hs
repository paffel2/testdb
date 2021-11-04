{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module DraftsTests where

import           Data.Functor.Identity            (Identity)
import           Data.Time.Clock                  (UTCTime)
import           Database.PostgreSQL.Simple.Types (PGArray (PGArray))
import           Logger                           (LoggerHandle (..),
                                                   Priority (Debug))
import           Network.HTTP.Types               (methodDelete, methodGet,
                                                   methodPost, methodPut)
import           Network.Wai                      (Request (rawPathInfo, requestMethod),
                                                   defaultRequest)
import           Network.Wai.Parse                (FileInfo (FileInfo, fileContent, fileContentType, fileName))
import           OperationsHandle                 (DraftsHandle (..),
                                                   OperationsHandle (..))
import           Router                           (routes)
import           Test.Hspec                       (describe, hspec, it,
                                                   shouldBe)
import           Types.Drafts                     (Draft (..),
                                                   DraftArray (DraftArray))
import           Types.Other                      (ResponseErrorMessage (BadRequest, Forbidden, InternalServerError, MethodNotAllowed, NotFound),
                                                   ResponseOkMessage (Created, OkJSON, OkMessage),
                                                   SomeError (BadToken, DatabaseError, OtherError))

hLogger :: LoggerHandle Identity
hLogger =
    LoggerHandle {priority = Debug, Logger.log = \prior message -> return ()}

draftsHandler :: DraftsHandle Identity
draftsHandler =
    DraftsHandle
        { dhGetDraftsByAuthorToken =
              \token -> return $ Left $ OtherError "ErrorMessage"
        , dhDeleteDraftFromDb =
              \token id -> return $ Left $ OtherError "ErrorMessage"
        , dhGetDraftByIdFromDb =
              \token id -> return $ Left $ OtherError "ErrorMessage"
        , dhCreateDraftOnDb =
              \draft_information draft_tags draft_main_image draft_other_images ->
                  return $ Left $ OtherError "ErrorMessage"
        , dhUpdateDraftInDb =
              \draft_information draft_tags draft_main_image draft_other_images draft_id ->
                  return $ Left $ OtherError "ErrorMessage"
        , dhPublicNewsOnDb =
              \token draft_id -> return $ Left $ OtherError "ErrorMessage"
        , dhLogger = hLogger
        , dhParseRequestBody = \_ -> return ([], [])
        }

operationsHandler :: OperationsHandle Identity
operationsHandler = OperationsHandle {draftsHandle = draftsHandler}

tstGetDraftsListReq :: Request
tstGetDraftsListReq =
    defaultRequest {rawPathInfo = "/drafts", requestMethod = methodGet}

tstGetDraftReq :: Request
tstGetDraftReq =
    defaultRequest {rawPathInfo = "/drafts/1", requestMethod = methodGet}

tstPostDraftReq :: Request
tstPostDraftReq =
    defaultRequest {rawPathInfo = "/new_draft", requestMethod = methodPost}

tstDeleteDraftReq :: Request
tstDeleteDraftReq =
    defaultRequest
        {rawPathInfo = "/drafts/delete_draft", requestMethod = methodDelete}

tstUpdateDraftReq :: Request
tstUpdateDraftReq =
    defaultRequest
        {rawPathInfo = "/drafts/1/update_draft", requestMethod = methodPut}

tstPostNewsReq :: Request
tstPostNewsReq =
    defaultRequest
        {rawPathInfo = "/drafts/1/public_news", requestMethod = methodPut}

draftTest :: Draft
draftTest =
    Draft
        { draftShortTitle = "Title"
        , dateOfChanges = read "2021-11-19 18:28:52.607875 UTC" :: UTCTime
        , draftCategoryId = Just 1
        , draftText = Just "text"
        , draftMainImageId = Just 1
        , draftImages = Just (PGArray [1])
        , draftTags = Just (PGArray ["tag"])
        }

tstFile =
    FileInfo {fileName = "something", fileContentType = "", fileContent = ""}

draftsTests :: IO ()
draftsTests =
    hspec $ do
        describe "testing drafts functions" $ do
            describe "testing dhGetDraftsByAuthorToken" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstGetDraftsListReq `shouldBe`
                    return
                        (Left $
                         BadRequest "List of drafts not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        (tstGetDraftsListReq {requestMethod = methodPut}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstGetDraftsListReq {rawPathInfo = "/draftssssss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of drafts, because all is good" $
                    routes
                        (operationsHandler
                             { draftsHandle =
                                   draftsHandler
                                       { dhGetDraftsByAuthorToken =
                                             \_ ->
                                                 return $ Right (DraftArray [])
                                       }
                             })
                        tstGetDraftsListReq `shouldBe`
                    return (Right $ OkJSON "{\"drafts\":[]}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { draftsHandle =
                                   draftsHandler
                                       { dhGetDraftsByAuthorToken =
                                             \_ -> return $ Left DatabaseError
                                       }
                             })
                        tstGetDraftsListReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "List of drafts not sended. Database Error."))
                it "server should return error, because using bad token" $
                    routes
                        (operationsHandler
                             { draftsHandle =
                                   draftsHandler
                                       { dhGetDraftsByAuthorToken =
                                             \_ -> return $ Left BadToken
                                       }
                             })
                        tstGetDraftsListReq `shouldBe`
                    return
                        (Left
                             (Forbidden "List of drafts not sended. Bad Token."))
{-
                        GET DRAFT TESTS
-}
            describe "testing dhGetDraftByIdFromDb" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstGetDraftReq `shouldBe`
                    return (Left $ BadRequest "Draft not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        (tstGetDraftReq {requestMethod = methodPut}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstGetDraftReq {rawPathInfo = "/drafts/adadad"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of drafts, because all is good" $
                    routes
                        (operationsHandler
                             { draftsHandle =
                                   draftsHandler
                                       { dhGetDraftByIdFromDb =
                                             \_ _ -> return $ Right draftTest
                                       }
                             })
                        tstGetDraftReq `shouldBe`
                    return
                        (Right $
                         OkJSON
                             "{\"date_of_changes\":\"2021-11-19T18:28:52.607875Z\",\"draft_tags\":[\"tag\"],\"draft_main_image_id\":1,\"draft_images\":[1],\"draft_short_title\":\"Title\",\"draft_text\":\"text\",\"draft_category_id\":1}")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { draftsHandle =
                                   draftsHandler
                                       { dhGetDraftByIdFromDb =
                                             \_ _ -> return $ Left DatabaseError
                                       }
                             })
                        tstGetDraftReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "Draft not sended. Database Error."))
                it "server should return error, because using bad token" $
                    routes
                        (operationsHandler
                             { draftsHandle =
                                   draftsHandler
                                       { dhGetDraftByIdFromDb =
                                             \_ _ -> return $ Left BadToken
                                       }
                             })
                        tstGetDraftReq `shouldBe`
                    return (Left (Forbidden "Draft not sended. Bad Token."))
{-
                                CREATE DRAFT TESTS
-}
            describe "testing dhCreateDraftOnDb" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstPostDraftReq `shouldBe`
                    return (Left $ BadRequest "Draft not created. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        (tstPostDraftReq {requestMethod = methodGet}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstPostDraftReq {rawPathInfo = "/new_draftsssss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return draft id, because all is good" $
                    routes
                        (operationsHandler
                             { draftsHandle =
                                   draftsHandler
                                       { dhCreateDraftOnDb =
                                             \_ _ _ _ -> return $ Right 1
                                       }
                             })
                        tstPostDraftReq `shouldBe`
                    return (Right (Created "1"))
                it "server should return error, because token is bad" $
                    routes
                        (operationsHandler
                             { draftsHandle =
                                   draftsHandler
                                       { dhCreateDraftOnDb =
                                             \_ _ _ _ -> return $ Left BadToken
                                       }
                             })
                        tstPostDraftReq `shouldBe`
                    return (Left (Forbidden "Draft not created. Bad Token."))
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { draftsHandle =
                                   draftsHandler
                                       { dhCreateDraftOnDb =
                                             \_ _ _ _ ->
                                                 return $ Left DatabaseError
                                       }
                             })
                        tstPostDraftReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "Draft not created. Database Error."))
                it "server should return error, because using bad image file" $
                    routes
                        (operationsHandler
                             { draftsHandle =
                                   draftsHandler
                                       { dhParseRequestBody =
                                             \_ ->
                                                 return
                                                     ( []
                                                     , [("main_image", tstFile)])
                                       }
                             })
                        tstPostDraftReq `shouldBe`
                    return (Left (BadRequest "Bad image file"))
{-
                                UPDATE DRAFT TESTS
-}
            describe "testing dhUpdateDraftInDb" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstUpdateDraftReq `shouldBe`
                    return (Left $ BadRequest "Draft not updated. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        (tstUpdateDraftReq {requestMethod = methodGet}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstUpdateDraftReq
                             {rawPathInfo = "/drafts/1/update_draftsssss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it
                    "server should return message about successful updating, because all is good" $
                    routes
                        (operationsHandler
                             { draftsHandle =
                                   draftsHandler
                                       { dhUpdateDraftInDb =
                                             \_ _ _ _ _ -> return $ Right ()
                                       }
                             })
                        tstUpdateDraftReq `shouldBe`
                    return (Right $ OkMessage "Draft updated")
                it "server should return error, because token is bad" $
                    routes
                        (operationsHandler
                             { draftsHandle =
                                   draftsHandler
                                       { dhUpdateDraftInDb =
                                             \_ _ _ _ _ ->
                                                 return $ Left BadToken
                                       }
                             })
                        tstUpdateDraftReq `shouldBe`
                    return (Left (Forbidden "Draft not updated. Bad Token."))
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { draftsHandle =
                                   draftsHandler
                                       { dhUpdateDraftInDb =
                                             \_ _ _ _ _ ->
                                                 return $ Left DatabaseError
                                       }
                             })
                        tstUpdateDraftReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "Draft not updated. Database Error."))
                it "server should return error, because using bad image file" $
                    routes
                        (operationsHandler
                             { draftsHandle =
                                   draftsHandler
                                       { dhParseRequestBody =
                                             \_ ->
                                                 return
                                                     ( []
                                                     , [("main_image", tstFile)])
                                       }
                             })
                        tstUpdateDraftReq `shouldBe`
                    return (Left (BadRequest "Bad image file"))
                it
                    "server should return error, because something draft id is bad" $
                    routes
                        operationsHandler
                        (tstUpdateDraftReq
                             {rawPathInfo = "/drafts/oh/update_draft"}) `shouldBe`
                    return (Left (BadRequest "Bad draft id"))
{-
                                DELETE TAG TESTS
-}
            describe "testing dhDeleteDraftFromDb" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstDeleteDraftReq `shouldBe`
                    return (Left $ BadRequest "Draft not deleted. ErrorMessage")
                it
                    "server should return error, because request sended with bad requestMethod" $
                    routes
                        operationsHandler
                        (tstDeleteDraftReq {requestMethod = methodGet}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstDeleteDraftReq
                             {rawPathInfo = "/drafts/1/delete_draftss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return message about successful deleting" $
                    routes
                        (operationsHandler
                             { draftsHandle =
                                   draftsHandler
                                       { dhDeleteDraftFromDb =
                                             \_ _ -> return $ Right ()
                                       }
                             })
                        tstDeleteDraftReq `shouldBe`
                    return (Right (OkMessage "Draft deleted."))
                it "server should return error, because token is bad" $
                    routes
                        (operationsHandler
                             { draftsHandle =
                                   draftsHandler
                                       { dhDeleteDraftFromDb =
                                             \_ _ -> return $ Left BadToken
                                       }
                             })
                        tstDeleteDraftReq `shouldBe`
                    return (Left (Forbidden "Draft not deleted. Bad Token."))
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { draftsHandle =
                                   draftsHandler
                                       { dhDeleteDraftFromDb =
                                             \_ _ -> return $ Left DatabaseError
                                       }
                             })
                        tstDeleteDraftReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "Draft not deleted. Database Error."))
{-
                                PUBLIC NEWS TESTS
-}
            describe "testing dhPublicNewsOnDb" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstPostNewsReq `shouldBe`
                    return (Left $ BadRequest "News not created. ErrorMessage")
                it
                    "server should return error, because request sended with bad requestMethod" $
                    routes
                        operationsHandler
                        (tstPostNewsReq {requestMethod = methodGet}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstPostNewsReq {rawPathInfo = "/drafts/1/public_newss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return message about successful posting" $
                    routes
                        (operationsHandler
                             { draftsHandle =
                                   draftsHandler
                                       { dhPublicNewsOnDb =
                                             \_ _ -> return $ Right 1
                                       }
                             })
                        tstPostNewsReq `shouldBe`
                    return (Right (Created "1"))
                it "server should return error, because token is bad" $
                    routes
                        (operationsHandler
                             { draftsHandle =
                                   draftsHandler
                                       { dhPublicNewsOnDb =
                                             \_ _ -> return $ Left BadToken
                                       }
                             })
                        tstPostNewsReq `shouldBe`
                    return (Left (Forbidden "News not created. Bad Token."))
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { draftsHandle =
                                   draftsHandler
                                       { dhPublicNewsOnDb =
                                             \_ _ -> return $ Left DatabaseError
                                       }
                             })
                        tstPostNewsReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "News not created. Database Error."))
