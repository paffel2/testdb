{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Answers.Images where

import           Answer                    (AnswerHandle (..))
import           Control.Monad.Except      (MonadError (throwError))
import qualified Data.ByteString.Char8     as BC
import           FromRequest               (toPage)
import           HelpFunction              (readByteStringToId)
import           Network.HTTP.Types.Method (methodGet)
import           Network.Wai               (Request (rawPathInfo, requestMethod))
import           OperationsHandle          (ImagesHandle (ihGetPhoto, ihGetPhotoList))
import           Types.Images              (ImageArray, ImageB)
import           Types.Other               (Id, MonadIOWithError, Page,
                                            SomeError (BadMethod, OtherError))

-------------------------------------------------------------------------------------------------
imagesListParseInformation ::
       MonadIOWithError m => ImagesHandle m -> Request -> m (Maybe Page)
imagesListParseInformation _ request =
    if requestMethod request /= methodGet
        then throwError BadMethod
        else return $ toPage request

imagesListDatabaseOperation ::
       MonadIOWithError m => ImagesHandle m -> Maybe Page -> m ImageArray
imagesListDatabaseOperation = ihGetPhotoList

getImagesListHandle ::
       MonadIOWithError m
    => ImagesHandle m
    -> AnswerHandle m (Maybe Page) ImageArray
getImagesListHandle imagesHandle =
    AnswerHandle
        { parseInformation = imagesListParseInformation imagesHandle
        , databaseOperation = imagesListDatabaseOperation imagesHandle
        }

-----------------------------------------------------------------------------------------
getImageParseInformation ::
       MonadIOWithError m => ImagesHandle m -> Request -> m Id
getImageParseInformation _ request = do
    if requestMethod request /= methodGet
        then throwError BadMethod
        else case readByteStringToId $ last pathElems of
                 Nothing -> throwError $ OtherError "Bad image id"
                 Just n  -> return n
  where
    path = BC.tail $ rawPathInfo request
    pathElems = BC.split '/' path

getImageDatabaseOperation ::
       MonadIOWithError m => ImagesHandle m -> Id -> m ImageB
getImageDatabaseOperation = ihGetPhoto

getImageHandle ::
       MonadIOWithError m => ImagesHandle m -> AnswerHandle m Id ImageB
getImageHandle imagesHandle =
    AnswerHandle
        { parseInformation = getImageParseInformation imagesHandle
        , databaseOperation = getImageDatabaseOperation imagesHandle
        }
