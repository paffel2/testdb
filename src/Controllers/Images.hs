{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Controllers.Images where

import           Answer                (answer)
import           Answers.Images        (getImageHandle, getImagesListHandle)
import           Control.Monad.Except  (ExceptT, MonadIO)
import           Data.Aeson            (encode)
import qualified Data.ByteString.Char8 as BC
import           HelpFunction          (sendResult)
import           Logger                (LoggerHandle)
import           Network.Wai           (Request (rawPathInfo))
import           OperationsHandle      (ImagesHandle)
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
        sendResult hLogger "List of images not sended." imagesListOk $
        answer req (getImagesListHandle operations)
    | pathElemsC == 2 =
        sendResult hLogger "Image not sended." OkImage $
        answer req (getImageHandle operations)
    | otherwise = return $ Left $ NotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
    imagesListOk imagesList = OkJSON $ encode imagesList
