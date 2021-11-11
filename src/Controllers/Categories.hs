{-# LANGUAGE OverloadedStrings #-}

module Controllers.Categories where

import           Answer                (answer)
import           Answers.Categories    (createCategoryHandle,
                                        deleteCategoryHandle,
                                        getCategoriesListHandle,
                                        updateCategoryHandle)
import qualified Data.ByteString.Char8 as BC
import           Network.Wai           (Request (rawPathInfo))
import           OperationsHandle      (CategoriesHandle)
import           Types.Other           (ResponseErrorMessage (NotFound),
                                        ResponseOkMessage)

categoriesRouter ::
       Monad m
    => CategoriesHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
categoriesRouter operations req
    | pathElemsC == 1 = answer req (getCategoriesListHandle operations)
    | pathElemsC == 2 =
        case last pathElems of
            "delete_category" -> answer req (deleteCategoryHandle operations)
            "create_category" -> answer req (createCategoryHandle operations)
            "edit_category"   -> answer req (updateCategoryHandle operations)
            _                 -> return $ Left $ NotFound "Not Found"
    | otherwise = return $ Left $ NotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
