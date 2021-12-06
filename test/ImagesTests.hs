{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module ImagesTests where

import           Control.Monad.Except       (ExceptT, MonadError (throwError),
                                             MonadIO)
import           Data.Functor.Identity      (Identity)
import           Database.PostgreSQL.Simple (Binary (Binary))
import           Logger                     (LoggerHandle (..),
                                             Priority (Debug))
import           Network.HTTP.Types         (methodGet, methodPut)
import           Network.Wai                (Request (rawPathInfo, requestMethod),
                                             defaultRequest)
import           OperationsHandle           (ImagesHandle (..),
                                             OperationsHandle (..))
import           Router                     (routes)
import           Test.Hspec                 (describe, hspec, it, shouldBe)
import           Types.Images               (ImageArray (ImageArray),
                                             ImageB (ImageB))
import           Types.Other                (ResponseErrorMessage (BadRequest, InternalServerError, MethodNotAllowed, NotFound),
                                             ResponseOkMessage (OkImage, OkJSON),
                                             SomeError (OtherError))

instance MonadIO Identity

hLogger :: LoggerHandle Identity
hLogger =
    LoggerHandle {priority = Debug, Logger.log = \prior message -> return ()}

imagesHandler :: ImagesHandle (ExceptT SomeError Identity)
imagesHandler =
    ImagesHandle
        { ihGetPhoto = \photo_id -> throwError $ OtherError "ErrorMessage"
        , ihGetPhotoList = \page -> throwError $ OtherError "ErrorMessage"
        }

operationsHandler :: OperationsHandle (ExceptT SomeError Identity)
operationsHandler = OperationsHandle {imagesHandle = imagesHandler}

tstGetPhotoListReq :: Request
tstGetPhotoListReq =
    defaultRequest {rawPathInfo = "/image", requestMethod = methodGet}

tstGetPhotoReq :: Request
tstGetPhotoReq =
    defaultRequest {rawPathInfo = "/image/1", requestMethod = methodGet}

imagesTests :: IO ()
imagesTests =
    hspec $ do
        describe "testing images functions" $ do
            describe "testing ihGetPhotoList" $ do
                it "server should return error because something happend" $
                    routes operationsHandler hLogger tstGetPhotoListReq `shouldBe`
                    return
                        (Left $
                         BadRequest "List of images not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetPhotoListReq {requestMethod = methodPut}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed
                             "List of images not sended. Bad method request.")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetPhotoListReq {rawPathInfo = "/imagewertyuio"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of images, because all is good" $
                    routes
                        (operationsHandler
                             { imagesHandle =
                                   imagesHandler
                                       { ihGetPhotoList =
                                             \_ -> return (ImageArray [])
                                       }
                             })
                        hLogger
                        (tstGetPhotoListReq {rawPathInfo = "/image"}) `shouldBe`
                    return (Right $ OkJSON "{\"images\":[]}")
            describe "testing ihGetPhoto" $ do
                it "server should return error  because something happend" $
                    routes operationsHandler hLogger tstGetPhotoReq `shouldBe`
                    return (Left $ BadRequest "Image not sended. ErrorMessage")
                it
                    "server should return status error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetPhotoReq {requestMethod = methodPut}) `shouldBe`
                    return
                        (Left $
                         MethodNotAllowed
                             "Image not sended. Bad method request.")
                it "server should return error, because image id is wrong" $
                    routes
                        operationsHandler
                        hLogger
                        (tstGetPhotoReq {rawPathInfo = "/image/wertyuio"}) `shouldBe`
                    return (Left $ BadRequest "Image not sended. Bad image id")
                it "server should return image, because all is good" $
                    routes
                        (operationsHandler
                             { imagesHandle =
                                   imagesHandler
                                       { ihGetPhoto =
                                             \_ ->
                                                 return (ImageB (Binary "") "")
                                       }
                             })
                        hLogger
                        (tstGetPhotoReq {rawPathInfo = "/image/1"}) `shouldBe`
                    return (Right $ OkImage $ ImageB (Binary "") "")
