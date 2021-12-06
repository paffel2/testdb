{-# LANGUAGE OverloadedStrings #-}

module Controllers.Categories where

import           Answer                (answer)
import           Answers.Categories    (createCategoryHandle,
                                        deleteCategoryHandle,
                                        getCategoriesListHandle,
                                        updateCategoryHandle)
import           Control.Monad.Except  (ExceptT, MonadIO)
import           Data.Aeson            (encode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as LBS
import           HelpFunction          (sendResult)
import           Logger                (LoggerHandle)
import           Network.Wai           (Request (rawPathInfo))
import           OperationsHandle      (CategoriesHandle)
import           Types.Other           (ResponseErrorMessage (NotFound),
                                        ResponseOkMessage (Created, OkJSON, OkMessage),
                                        SomeError)

categoriesRouter ::
       MonadIO m
    => CategoriesHandle (ExceptT SomeError m)
    -> LoggerHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
categoriesRouter operations hLogger req
    | pathElemsC == 1 =
        sendResult hLogger "List of categories not sended." categrotiesListOK $
        answer req (getCategoriesListHandle operations)
    | pathElemsC == 2 =
        case last pathElems of
            "delete_category" ->
                sendResult hLogger "Category not deleted." deleteOK $
                answer req (deleteCategoryHandle operations)
            "create_category" ->
                sendResult hLogger "Category not created." createCategoryOk $
                answer req (createCategoryHandle operations)
            "edit_category" ->
                sendResult hLogger "Category not edited." updateOk $
                answer req (updateCategoryHandle operations)
            _ -> return $ Left $ NotFound "Not Found"
    | otherwise = return $ Left $ NotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems
    categrotiesListOK someList = OkJSON $ encode someList
    deleteOK _ = OkMessage "Category deleted."
    createCategoryOk categoryId =
        Created $ LBS.fromStrict $ BC.pack $ show categoryId
    updateOk _ = OkMessage "Category edited."
