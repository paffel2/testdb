{-# LANGUAGE OverloadedStrings #-}

module Controllers.Authors where

import           Answer                (answer)
import           Answers.Authors       (authorsListHandle, createAuthorHandle,
                                        deleteAuthorHandle, updateAuthorHandle)
import qualified Data.ByteString.Char8 as BC
import           Network.Wai           (Request (rawPathInfo))
import           OperationsHandle      (AuthorsHandle)
import           Types.Other           (ResponseErrorMessage (NotFound),
                                        ResponseOkMessage)

authorsRouter ::
       Monad m
    => AuthorsHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
authorsRouter methods req
    | pathElemsC == 1 = answer req (authorsListHandle methods)
    | pathElemsC == 2 =
        case last pathElems of
            "delete_author" -> answer req (deleteAuthorHandle methods)
            "create_author" -> answer req (createAuthorHandle methods)
            "edit_author"   -> answer req (updateAuthorHandle methods)
            _               -> return $ Left $ NotFound "Not Found"
    | otherwise = return $ Left $ NotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
