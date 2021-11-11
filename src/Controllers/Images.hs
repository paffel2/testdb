{-# LANGUAGE OverloadedStrings #-}

module Controllers.Images where

import qualified Data.ByteString.Char8 as BC

import           Answer                (answer)
import           Answwers.Images       (getImageHandle, getImagesListHandle)
import           Network.Wai           (Request (rawPathInfo))
import           OperationsHandle      (ImagesHandle)
import           Types.Other           (ResponseErrorMessage (NotFound),
                                        ResponseOkMessage)

imagesRouter ::
       Monad m
    => ImagesHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
imagesRouter operations req
    | pathElemsC == 1 = answer req (getImagesListHandle operations)
    | pathElemsC == 2 = answer req (getImageHandle operations)
    | otherwise = return $ Left $ NotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
