{-# LANGUAGE OverloadedStrings #-}

module Controllers.Categories where

import           Answer                (answer)
import           Answers.Categories    (createCategoryHandle,
                                        deleteCategoryHandle,
                                        getCategoriesListHandle,
                                        updateCategoryHandle)
import           Control.Monad.Except  (ExceptT, MonadIO, runExceptT)
import           Data.Aeson            (encode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as LBS
import           Logger                (LoggerHandle, logInfo)
import           Network.Wai           (Request (rawPathInfo))
import           OperationsHandle      (CategoriesHandle)
import           Responses             (toResponseErrorMessage')
import           Types.Categories      (ListOfCategories)
import           Types.Other           (ResponseErrorMessage (NotFound),
                                        ResponseOkMessage (Created, OkJSON, OkMessage),
                                        SendId, SomeError)

categoriesRouter ::
       MonadIO m
    => CategoriesHandle (ExceptT SomeError m)
    -> LoggerHandle m
    -> Request
    -> m (Either ResponseErrorMessage ResponseOkMessage)
categoriesRouter operations hLogger req
    | pathElemsC == 1 =
        getCategoriesListSendResult hLogger $
        answer req (getCategoriesListHandle operations)
    | pathElemsC == 2 =
        case last pathElems of
            "delete_category" ->
                deleteCategorySendResult hLogger $
                answer req (deleteCategoryHandle operations)
            "create_category" ->
                createCategorySendResult hLogger $
                answer req (createCategoryHandle operations)
            "edit_category" ->
                updateCategorySendResult hLogger $
                answer req (updateCategoryHandle operations)
            _ -> return $ Left $ NotFound "Not Found"
    | otherwise = return $ Left $ NotFound "Not Found"
  where
    path = BC.tail $ rawPathInfo req
    pathElems = BC.split '/' path
    pathElemsC = length pathElems

getCategoriesListSendResult ::
       Monad m
    => LoggerHandle m
    -> ExceptT SomeError m ListOfCategories
    -> m (Either ResponseErrorMessage ResponseOkMessage)
getCategoriesListSendResult hLogger result = do
    a <- runExceptT result
    case a of
        Left someError ->
            Left <$>
            toResponseErrorMessage'
                hLogger
                "List of categories not sended."
                someError
        Right someList -> do
            logInfo hLogger "List of categories sended."
            return $ Right $ OkJSON $ encode someList

createCategorySendResult ::
       Monad m
    => LoggerHandle m
    -> ExceptT SomeError m SendId
    -> m (Either ResponseErrorMessage ResponseOkMessage)
createCategorySendResult hLogger result = do
    a <- runExceptT result
    case a of
        Left someError ->
            Left <$>
            toResponseErrorMessage' hLogger "Category not created." someError
        Right categoryId -> do
            logInfo hLogger "Category created."
            return $
                Right $ Created $ LBS.fromStrict $ BC.pack $ show categoryId

deleteCategorySendResult ::
       Monad m
    => LoggerHandle m
    -> ExceptT SomeError m ()
    -> m (Either ResponseErrorMessage ResponseOkMessage)
deleteCategorySendResult hLogger result = do
    a <- runExceptT result
    case a of
        Left someError ->
            Left <$>
            toResponseErrorMessage' hLogger "Category not deleted." someError
        Right _ -> do
            logInfo hLogger "Category deleted."
            return $ Right $ OkMessage "Category deleted."

updateCategorySendResult ::
       Monad m
    => LoggerHandle m
    -> ExceptT SomeError m ()
    -> m (Either ResponseErrorMessage ResponseOkMessage)
updateCategorySendResult hLogger result = do
    a <- runExceptT result
    case a of
        Left someError ->
            Left <$>
            toResponseErrorMessage' hLogger "Category not edited." someError
        Right _ -> do
            logInfo hLogger "Category edited."
            return $ Right $ OkMessage "Category edited."
{-updateCategorySendResult ::
       Monad m
    => Either SomeError ()
    -> m (Either ResponseErrorMessage ResponseOkMessage)
updateCategorySendResult result =
    case result of
        Left someError ->
            return $
            Left $ toResponseErrorMessage "Category not edited." someError
        Right _ -> do
            return $ Right $ OkMessage "Category edited." -}
