{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module UsersTests where

import           Control.Monad.Except
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

instance MonadIO Identity

hLogger :: LoggerHandle Identity
hLogger =
    LoggerHandle {priority = Debug, Logger.log = \prior message -> return ()}

usersHandler :: UsersHandle (ExceptT SomeError Identity)
usersHandler =
    UsersHandle
        { uhAuth = \login password -> throwError $ OtherError "ErrorMessage"
        , uhCreateUserInDb =
              \create_user -> throwError $ OtherError "ErrorMessage"
        , uhDeleteUserFromDb =
              \token login -> throwError $ OtherError "ErrorMessage"
        , uhProfileOnDb = \token -> throwError $ OtherError "ErrorMessage"
        , uhParseRequestBody = \_ -> return ([], [])
        }

operationsHandler :: OperationsHandle (ExceptT SomeError Identity)
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
                    routes operationsHandler hLogger tstLoginReq `shouldBe`
                    return (Left $ BadRequest "Bad authorization. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        hLogger
                        (tstLoginReq {requestMethod = methodPut}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed
                             "Bad authorization. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstLoginReq {rawPathInfo = "/loging"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return token, because all is good" $
                    routes
                        (operationsHandler
                             { usersHandle =
                                   usersHandler
                                       {uhAuth = \_ _ -> return (Token "token")}
                             })
                        hLogger
                        tstLoginReq `shouldBe`
                    return (Right $ OkMessage "token")
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { usersHandle =
                                   usersHandler
                                       { uhAuth =
                                             \_ _ -> throwError DatabaseError
                                       }
                             })
                        hLogger
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
                    routes operationsHandler hLogger tstRegistrationReq `shouldBe`
                    return
                        (Left $ BadRequest "User not registered. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        hLogger
                        (tstRegistrationReq {requestMethod = methodGet}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed
                             "User not registered. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstRegistrationReq
                             {rawPathInfo = "/registrationadasdasd"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return token, because all is good" $
                    routes
                        (operationsHandler
                             { usersHandle =
                                   usersHandler
                                       { uhCreateUserInDb =
                                             \_ -> return $ Token "token"
                                       }
                             })
                        hLogger
                        tstRegistrationReq `shouldBe`
                    return (Right (Created "token"))
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { usersHandle =
                                   usersHandler
                                       { uhCreateUserInDb =
                                             \_ -> throwError DatabaseError
                                       }
                             })
                        hLogger
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
                    routes operationsHandler hLogger tstDeleteUserReq `shouldBe`
                    return (Left $ BadRequest "User not deleted. ErrorMessage")
                it
                    "server should return error, because request sended with bad requestMethod" $
                    routes
                        operationsHandler
                        hLogger
                        (tstDeleteUserReq {requestMethod = methodGet}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed
                             "User not deleted. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstDeleteUserReq {rawPathInfo = "/delete_userssssss"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return message about successful deleting" $
                    routes
                        (operationsHandler
                             { usersHandle =
                                   usersHandler
                                       {uhDeleteUserFromDb = \_ _ -> return ()}
                             })
                        hLogger
                        tstDeleteUserReq `shouldBe`
                    return (Right (OkMessage "User deleted."))
                it "server should return error, because token is not admin" $
                    routes
                        (operationsHandler
                             { usersHandle =
                                   usersHandler
                                       { uhDeleteUserFromDb =
                                             \_ _ -> throwError NotAdmin
                                       }
                             })
                        hLogger
                        tstDeleteUserReq `shouldBe`
                    return (Left (Forbidden "User not deleted. Not Admin."))
                it "server should return error, because token is bad" $
                    routes
                        (operationsHandler
                             { usersHandle =
                                   usersHandler
                                       { uhDeleteUserFromDb =
                                             \_ _ -> throwError BadToken
                                       }
                             })
                        hLogger
                        tstDeleteUserReq `shouldBe`
                    return (Left (Forbidden "User not deleted. Bad Token."))
                it
                    "server should return error, because something wrong with database" $
                    routes
                        (operationsHandler
                             { usersHandle =
                                   usersHandler
                                       { uhDeleteUserFromDb =
                                             \_ _ -> throwError DatabaseError
                                       }
                             })
                        hLogger
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
                    routes operationsHandler hLogger tstProfileReq `shouldBe`
                    return
                        (Left $
                         BadRequest
                             "Profile inforamtion not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad requestMethod" $
                    routes
                        operationsHandler
                        hLogger
                        (tstProfileReq {requestMethod = methodPost}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed
                             "Profile inforamtion not sended. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
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
                                                 Profile
                                                     (Just "first name")
                                                     (Just "last name")
                                                     (Just 1)
                                       }
                             })
                        hLogger
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
                                             \_ -> throwError BadToken
                                       }
                             })
                        hLogger
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
                                             \_ -> throwError DatabaseError
                                       }
                             })
                        hLogger
                        tstProfileReq `shouldBe`
                    return
                        (Left
                             (InternalServerError
                                  "Profile inforamtion not sended. Database Error."))
