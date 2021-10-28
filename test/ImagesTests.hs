{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module ImagesTests where

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
import           Types.Other                (ResponseErrorMessage (BadRequest, MethodNotAllowed, NotFound),
                                             ResponseOkMessage (OkImage, OkJSON),
                                             SomeError (OtherError))

hLogger :: LoggerHandle Identity
hLogger =
    LoggerHandle {priority = Debug, Logger.log = \prior message -> return ()}

imagesHandler :: ImagesHandle Identity
imagesHandler =
    ImagesHandle
        { get_photo = \photo_id -> return $ Left $ OtherError "ErrorMessage"
        , get_photo_list = \page -> return $ Left $ OtherError "ErrorMessage"
        , photos_logger = hLogger
        }

operationsHandler :: OperationsHandle Identity
operationsHandler = OperationsHandle {images_handle = imagesHandler}

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
            describe "testing get_photo_list" $ do
                it "server should return error because something happend" $
                    routes operationsHandler tstGetPhotoListReq `shouldBe`
                    return
                        (Left $
                         BadRequest "List of images not sended. ErrorMessage")
                it
                    "server should return error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        (tstGetPhotoListReq {requestMethod = methodPut}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because path is wrong" $
                    routes
                        operationsHandler
                        (tstGetPhotoListReq {rawPathInfo = "/imagewertyuio"}) `shouldBe`
                    return (Left $ NotFound "Not Found")
                it "server should return list of images, because all is good" $
                    routes
                        (operationsHandler
                             { images_handle =
                                   imagesHandler
                                       { get_photo_list =
                                             \_ ->
                                                 return $ Right (ImageArray [])
                                       }
                             })
                        (tstGetPhotoListReq {rawPathInfo = "/image"}) `shouldBe`
                    return (Right $ OkJSON "{\"images\":[]}")
            describe "testing get_photo" $ do
                it "server should return error  because something happend" $
                    routes operationsHandler tstGetPhotoReq `shouldBe`
                    return (Left $ BadRequest "Image not sended. ErrorMessage")
                it
                    "server should return status error, because request sended with bad request method" $
                    routes
                        operationsHandler
                        (tstGetPhotoReq {requestMethod = methodPut}) `shouldBe`
                    return (Left $ MethodNotAllowed "Bad request method")
                it "server should return error, because image id is wrong" $
                    routes
                        operationsHandler
                        (tstGetPhotoReq {rawPathInfo = "/image/wertyuio"}) `shouldBe`
                    return (Left $ BadRequest "Bad image id")
                it "server should return image, because all is good" $
                    routes
                        (operationsHandler
                             { images_handle =
                                   imagesHandler
                                       { get_photo =
                                             \_ ->
                                                 return $
                                                 Right (ImageB (Binary "") "")
                                       }
                             })
                        (tstGetPhotoReq {rawPathInfo = "/image/1"}) `shouldBe`
                    return (Right $ OkImage $ ImageB (Binary "") "")
