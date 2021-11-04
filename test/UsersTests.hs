{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module UsersTests where
import           Data.Functor.Identity (Identity)
import           Logger                (LoggerHandle (..), Priority (Debug))
import           Network.HTTP.Types    (methodDelete, methodGet, methodPost,
                                        methodPut)
import           Network.Wai           (Request (rawPathInfo, requestMethod),
                                        defaultRequest)
import           OperationsHandle      (OperationsHandle (..), UsersHandle (..))
import           Router                (routes)
import           Test.Hspec            (describe, hspec, it, shouldBe)
import           Types.Other           (ResponseErrorMessage (BadRequest, Forbidden, InternalServerError, MethodNotAllowed, NotFound),
                                        ResponseOkMessage (Created, OkJSON, OkMessage),
                                        SomeError (BadToken, DatabaseError, NotAdmin, OtherError),
                                        Token (Token))
import           Types.Users           (Profile (Profile))

hLogger :: LoggerHandle Identity
hLogger =
    LoggerHandle {priority = Debug, Logger.log = \prior message -> return ()}

usersHandler :: UsersHandle Identity
usersHandler =
    UsersHandle
        { uhAuth = \login password -> return $ Left $ OtherError "ErrorMessage"
        , uhCreateUserInDb =
              \create_user -> return $ Left $ OtherError "ErrorMessage"
        , uhDeleteUserFromDb =
              \token login -> return $ Left $ OtherError "ErrorMessage"
        , uhProfileOnDb = \token -> return $ Left $ OtherError "ErrorMessage"
        , uhLogger = hLogger
        , uhParseRequestBody = \_ -> return ([], [])
        }

operationsHandler :: OperationsHandle Identity
operationsHandler = OperationsHandle {usersHandle = usersHandler}

tstProfileReq :: Request
tstProfileReq =
    defaultRequest {rawPathInfo = "/profile", requestMethod = methodGet}

tstRegistrationReq :: Request
tstRegistrationReq =
    defaultRequest {rawPathInfo = "/registration", requestMethod = methodPost}

tstDeleteUserReq :: Request
tstDeleteUserReq =
    defaultRequest {rawPathInfo = "/deleteUser", requestMethod = methodDelete}

tstLoginReq :: Request
tstLoginReq = defaultRequest {rawPathInfo = "/login", requestMethod = methodGet}

usersTests :: IO ()
usersTests =
    hspec $ do
        describe "testing users functions" $ do
            describe "testing uhAuth" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstLoginReq `shouldBe`
                    return (Left $ BadRequest "Bad authorization. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        (tstLoginReq {requestMethod = methodPut}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstLoginReq {rawPathInfo = "/loging"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return token, because all is good" $
                    routes
                        (operationsHandler
                             { usersHandle =
                                   usersHandler
                                       { uhAuth =
                                             \_ _ ->
                                                 return $ Right (Token "token")
                                       }
                             })
                        tstLoginReq `shouldBe`
                    return (Right $ OkMessage "token")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { usersHandle =
                                   usersHandler
                                       { uhAuth =
                                             \_ _ -> return $ Left DatabaseError
                                       }
                             })
                        tstLoginReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "Bad authorization. Database Error."))
{-
                                REGISTRATION TESTS
-}
            describe "testing uhCreateUserInDb" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstRegistrationReq `shouldBe`
                    return
                        (Left $ BadRequest "User not registered. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        (tstRegistrationReq {requestMethod = methodGet}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstRegistrationReq
                             {rawPathInfo = "/registrationadasdasd"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return token, because all is good" $
                    routes
                        (operationsHandler
                             { usersHandle =
                                   usersHandler
                                       { uhCreateUserInDb =
                                             \_ ->
                                                 return $ Right $ Token "token"
                                       }
                             })
                        tstRegistrationReq `shouldBe`
                    return (Right (Created "token"))
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { usersHandle =
                                   usersHandler
                                       { uhCreateUserInDb =
                                             \_ -> return $ Left DatabaseError
                                       }
                             })
                        tstRegistrationReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "User not registered. Database Error."))
{-
                                DELETE USER TESTS
-}
            describe "testing uhDeleteUserFromDb" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstDeleteUserReq `shouldBe`
                    return (Left $ BadRequest "User not deleted. ErrorMessage")
                it
                    "server should return error, because request sended with bad requestMethod" $
                    routes
                        operationsHandler
                        (tstDeleteUserReq {requestMethod = methodGet}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstDeleteUserReq {rawPathInfo = "/delete_userssssss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return message about successful deleting" $
                    routes
                        (operationsHandler
                             { usersHandle =
                                   usersHandler
                                       { uhDeleteUserFromDb =
                                             \_ _ -> return $ Right ()
                                       }
                             })
                        tstDeleteUserReq `shouldBe`
                    return (Right (OkMessage "User deleted."))
                it "server should return error, because token is not admin" $
                    routes
                        (operationsHandler
                             { usersHandle =
                                   usersHandler
                                       { uhDeleteUserFromDb =
                                             \_ _ -> return $ Left NotAdmin
                                       }
                             })
                        tstDeleteUserReq `shouldBe`
                    return (Left (Forbidden "User not deleted. Not Admin."))
                it "server should return error, because token is bad" $
                    routes
                        (operationsHandler
                             { usersHandle =
                                   usersHandler
                                       { uhDeleteUserFromDb =
                                             \_ _ -> return $ Left BadToken
                                       }
                             })
                        tstDeleteUserReq `shouldBe`
                    return (Left (Forbidden "User not deleted. Bad Token."))
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { usersHandle =
                                   usersHandler
                                       { uhDeleteUserFromDb =
                                             \_ _ -> return $ Left DatabaseError
                                       }
                             })
                        tstDeleteUserReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "User not deleted. Database Error."))
{-
                                PROFILE TESTS
-}
            describe "testing uhProfileOnDb" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstProfileReq `shouldBe`
                    return
                        (Left $
                         BadRequest
                             "Profile inforamtion not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad requestMethod" $
                    routes
                        operationsHandler
                        (tstProfileReq {requestMethod = methodPost}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstProfileReq {rawPathInfo = "/profiles"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return message about profile information" $
                    routes
                        (operationsHandler
                             { usersHandle =
                                   usersHandler
                                       { uhProfileOnDb =
                                             \_ ->
                                                 return $
                                                 Right $
                                                 Profile
                                                     (Just "first name")
                                                     (Just "last name")
                                                     (Just 1)
                                       }
                             })
                        tstProfileReq `shouldBe`
                    return
                        (Right
                             (OkJSON
                                  "{\"profile_last_name\":\"last name\",\"profile_avatar\":1,\"profile_first_name\":\"first name\"}"))
                it "server should return error, because token is bad" $
                    routes
                        (operationsHandler
                             { usersHandle =
                                   usersHandler
                                       { uhProfileOnDb =
                                             \_ -> return $ Left BadToken
                                       }
                             })
                        tstProfileReq `shouldBe`
                    return
                        (Left
                             (Forbidden
                                  "Profile inforamtion not sended. Bad Token."))
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { usersHandle =
                                   usersHandler
                                       { uhProfileOnDb =
                                             \_ -> return $ Left DatabaseError
                                       }
                             })
                        tstProfileReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "Profile inforamtion not sended. Database Error."))
