{-# LANGUAGE OverloadedStrings #-}

module Controllers.Drafts where

import           Answer                (answer)
import           Answers.Drafts        (deleteDraftHandle, getDraftByIdHandle,
                                        getDraftsHandle, postNewsHandle,
                                        updateDraftHandle)
import qualified Data.ByteString.Char8 as BC
import           HelpFunction          (readByteStringToId)
import           Network.Wai           (Request (rawPathInfo))
import           OperationsHandle      (DraftsHandle)
import           Types.Other           (ResponseErrorMessage (BadRequest, NotFound),
                                        ResponseOkMessage)

draftsRouter ::
       Monad m
    => DraftsHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
draftsRouter operations req
    | pathElemsC == 1 = answer req (getDraftsHandle operations)
    | pathElemsC == 2 =
        case readByteStringToId $ last pathElems of
            Just _ -> answer req (getDraftByIdHandle operations)
            Nothing ->
                case last pathElems of
                    "delete_draft" -> answer req (deleteDraftHandle operations)
                    _              -> return $ Left $ NotFound "Not Found"
    | pathElemsC == 3 =
        case readByteStringToId $ head $ tail pathElems of
            Nothing -> return $ Left $ BadRequest "Bad draft id"
            Just _ ->
                case last pathElems of
                    "update_draft" -> answer req (updateDraftHandle operations)
                    "public_news"  -> answer req (postNewsHandle operations)
                    _              -> return $ Left $ NotFound "Not Found"
    | otherwise = return $ Left $ NotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
