{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
module Categorieshandle where
import Network.Wai
--import Network.Wai.Handler.Warp
import Network.Wai.Parse
--import Network.HTTP.Types
--import GHC.Generics
import Data.Aeson

--import Control.Exception
--import qualified Data.Text.Lazy.Encoding as EL
--import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.Encoding as E
import qualified Data.Text as T
--import qualified Data.Text.IO as TIO
--import qualified Data.ByteString as B
--import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BC
--import Types
import Data.Maybe
--import Text.Read
--import Control.Applicative
--import HelpFunction
import Responses
import Logger
--import NewsAndComments
import Databaseoperations
{-import qualified Data.Text.Lazy as T
import qualified Data.Char as T-}


sendCategoriesList :: Handle -> Request -> IO Response
sendCategoriesList hLogger req = do
    result <- getCategoriesListFromDb hLogger pageParam
    case result of
      Left bs -> return $ responseBadRequest bs
      Right loc -> return $ responseOk $ encode loc
    where
        queryParams = queryString req
        pageParam = fromMaybe Nothing (lookup "page" queryParams)


createCategory :: Handle -> Request -> IO Response
createCategory hLogger req = do
    let token = fromMaybe Nothing (lookup "token" $ queryString req)
    ct <- checkAdmin hLogger $ E.decodeUtf8 $ fromMaybe "" token
    case ct of
        (False,bs) -> return $ responseBadRequest bs
        (True,_) -> do
                (i,_) <- parseRequestBody lbsBackEnd req
                let category_name = T.toLower . E.decodeUtf8 <$> lookup "category_name" i
                --let c = T.toLower <$> category_name
                let maternal_category_name = T.toLower . E.decodeUtf8 <$> lookup "maternal_category_name" i
                result <- createCategoryOnDb hLogger category_name maternal_category_name
                case result of
                  Left bs -> return $ responseBadRequest bs
                  Right bs -> return $ responseOk bs


deleteCategory :: Handle -> Request -> IO Response
deleteCategory hLogger req = do
    let token = fromMaybe Nothing (lookup "token" $ queryString req)
    ct <- checkAdmin hLogger $ E.decodeUtf8 $ fromMaybe "" token
    case ct of
        (False,bs) -> return $ responseBadRequest bs
        (True,_) -> do
            (i,_) <- parseRequestBody lbsBackEnd req
            let category_name = E.decodeUtf8 <$> lookup "category_name" i
            result <- deleteCategoryFromDb hLogger category_name
            case result of
              Left bs -> return $ responseBadRequest bs
              Right bs -> return $ responseOk bs


editCategory :: Handle -> Request -> IO Response
editCategory hLogger req = do
    let token = fromMaybe Nothing (lookup "token" $ queryString req)
    ct <- checkAdmin hLogger $ E.decodeUtf8 $ fromMaybe "" token
    case ct of
        (False,bs) -> return $ responseBadRequest bs
        (True,_) -> do
                    (i,_) <- parseRequestBody lbsBackEnd req
                    let category_name = E.decodeUtf8 <$> lookup "category_name" i
                    let new_maternal_parametr = E.decodeUtf8 <$> lookup "new_maternal" i
                    let new_name_parametr = E.decodeUtf8 <$> lookup "new_name" i
                    result <- editCategoryOnDb hLogger category_name new_name_parametr new_maternal_parametr
                    case result of
                      Left bs -> return $ responseBadRequest bs
                      Right bs -> return $ responseOk bs


categoriesBlock :: Handle -> BC.ByteString -> [BC.ByteString] -> Request -> IO Response
categoriesBlock hLogger path pathElems req | pathElemsC == 1 = sendCategoriesList hLogger req
                                   | pathElemsC == 2 =
                                       case last pathElems of
                                           "delete_category" -> deleteCategory hLogger req
                                           "create_category" -> createCategory hLogger req
                                           "edit_category" -> editCategory hLogger req
                                           _ -> return $ responseBadRequest "bad request"
                                   | otherwise = return $ responseBadRequest "bad request"


    where pathElemsC = length pathElems