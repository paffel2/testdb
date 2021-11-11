{-# LANGUAGE OverloadedStrings #-}

module Controllers.Tags where

import           Answer                (answer)
import           Answers.Tags          (createTagHandle, deleteTagHandle,
                                        getTagsListHandle, updateTagHandle)
import qualified Data.ByteString.Char8 as BC
import           Network.Wai           (Request (rawPathInfo))
import           OperationsHandle      (TagsHandle)
import           Types.Other           (ResponseErrorMessage (NotFound),
                                        ResponseOkMessage)

tagsRouter ::
       Monad m
    => TagsHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
tagsRouter operations req
    | pathElemsC == 1 = answer req (getTagsListHandle operations)
    | pathElemsC == 2 =
        case last pathElems of
            "create_tag" -> answer req (createTagHandle operations)
            "delete_tag" -> answer req (deleteTagHandle operations)
            "edit_tag"   -> answer req (updateTagHandle operations)
            _            -> return $ Left $ NotFound "Not Found"
    | otherwise = return $ Left $ NotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
