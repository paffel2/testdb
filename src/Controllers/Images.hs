{-# LANGUAGE OverloadedStrings #-}

module Controllers.Images where

import qualified Data.ByteString.Char8 as BC

import           Answer                (answer')
import           Answers.Images        (getImageHandle, getImagesListHandle)
import           Network.Wai           (Request (rawPathInfo))
import           OperationsHandle      (ImagesHandle)
import           Types.Other           (MonadWithError,
                                        ResponseErrorMessage (NotFound),
                                        ResponseOkMessage)

imagesRouter ::
       ImagesHandle MonadWithError IO
    -> Request
    -> IO (Either ResponseErrorMessage ResponseOkMessage)
imagesRouter operations req
    | pathElemsC == 1 = answer' req (getImagesListHandle operations)
    | pathElemsC == 2 = answer' req (getImageHandle operations)
    | otherwise = return $ Left $ NotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
