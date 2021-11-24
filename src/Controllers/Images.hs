{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Images where

import qualified Data.ByteString.Char8 as BC

import           Answer                (answer'')
import           Answers.Images        (getImageHandle, getImagesListHandle)
import           Control.Monad.Except  (ExceptT, MonadIO, runExceptT)
import           Data.Aeson            (encode)
import           Logger                (LoggerHandle, logInfo)
import           Network.Wai           (Request (rawPathInfo))
import           OperationsHandle      (ImagesHandle)
import           Responses             (toResponseErrorMessage')
import           Types.Images          (ImageArray, ImageB)
import           Types.Other           (ResponseErrorMessage (NotFound),
                                        ResponseOkMessage (OkImage, OkJSON),
                                        SomeError)

imagesRouter ::
       MonadIO m
    => ImagesHandle (ExceptT SomeError m)
    -> LoggerHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
imagesRouter operations hLogger req
    | pathElemsC == 1 =
        imagesListSendResult hLogger $
        answer'' req (getImagesListHandle operations)
    | pathElemsC == 2 =
        getImageSendResult hLogger $ answer'' req (getImageHandle operations)
    | otherwise = return $ Left $ NotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems

imagesListSendResult ::
       Monad m
    => LoggerHandle m
    -> ExceptT SomeError m ImageArray
    -> m (Either ResponseErrorMessage ResponseOkMessage)
imagesListSendResult hLogger result = do
    a <- runExceptT result
    case a of
        Left someError ->
            Left <$>
            toResponseErrorMessage'
                hLogger
                "List of images not sended."
                someError
        Right imagesArray -> do
            logInfo hLogger "List of images sended."
            return $ Right $ OkJSON $ encode imagesArray

getImageSendResult ::
       Monad m
    => LoggerHandle m
    -> ExceptT SomeError m ImageB
    -> m (Either ResponseErrorMessage ResponseOkMessage)
getImageSendResult hLogger result = do
    a <- runExceptT result
    case a of
        Left someError ->
            Left <$>
            toResponseErrorMessage' hLogger "Image not sended." someError
        Right image -> do
            logInfo hLogger "Image sended."
            return $ Right $ OkImage image
