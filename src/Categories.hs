{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
module Categories where
import Network.Wai
--import Network.Wai.Handler.Warp
import Network.Wai.Parse
--import Network.HTTP.Types
--import GHC.Generics
import Data.Aeson
import Testdatabase
--import Control.Exception
--import qualified Data.Text.Lazy.Encoding as EL
--import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.Encoding as E
--import qualified Data.Text as T
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
--import NewsAndComments



sendCategoriesList :: Request -> IO Response 
sendCategoriesList req = do
    result <- getCategoriesList pageParam
    return $ responseOk $ encode result
    where 
        queryParams = queryString req
        pageParam = fromMaybe Nothing (lookup "page" queryParams)

createCategory' :: Request -> IO Response
createCategory' req = do
    let token = fromMaybe Nothing (lookup "token" $ queryString req)
    ct <- checkAdmin $ E.decodeUtf8 $ fromMaybe "" token
    case ct of 
        (False,bs) -> return $ responseBadRequest bs
        (True,_) -> do
                (i,_) <- parseRequestBody lbsBackEnd req
                let category_name = lookup "category_name" i
                case category_name of
                    Nothing -> return $ responseBadRequest "Bad category name"
                    Just bs -> do
                        ch <- checkCategory $ E.decodeUtf8 bs
                        case ch of
                            Just n -> return $ responseBadRequest "Category already exist"
                            Nothing -> do
                                    let maternal_category_name = lookup "maternal_category_name" i
                                    case maternal_category_name of
                                        Nothing -> do
                                            result <- createCategory (E.decodeUtf8 bs) Nothing
                                            return $ responseOk result

                                        Just bs' -> do
                                            chm <- checkCategory $ E.decodeUtf8 bs'
                                            case chm of
                                              Nothing -> return $ responseBadRequest "Maternal category not exist"
                                              Just n -> do
                                                  result <- createCategory (E.decodeUtf8 bs) (Just n)
                                                  return $ responseOk result
deleteCategory' :: Request -> IO Response 
deleteCategory' req = do
    let token = fromMaybe Nothing (lookup "token" $ queryString req)
    ct <- checkAdmin $ E.decodeUtf8 $ fromMaybe "" token
    case ct of 
        (False,bs) -> return $ responseBadRequest bs
        (True,_) -> do
            (i,_) <- parseRequestBody lbsBackEnd req
            let category_name = lookup "category_name" i
            case category_name of
              Nothing -> return $ responseBadRequest "no categoty_name field"
              Just bs -> do
                  result <- deleteCategory $ E.decodeUtf8 bs
                  return $ responseOk result

editCategory' :: Request -> IO Response
editCategory' req = do
    let token = fromMaybe Nothing (lookup "token" $ queryString req)
    ct <- checkAdmin $ E.decodeUtf8 $ fromMaybe "" token
    case ct of 
        (False,bs) -> return $ responseBadRequest bs
        (True,_) -> do
                    let category_name_parametr = lookup "category_name" $ queryString req
    --print category_name_parametr
                    case category_name_parametr of
                        Nothing -> return $ responseBadRequest "no category name parametr"
                        Just m_bs -> do
                            let category_name = fromMaybe "" m_bs
                            cc <- checkCategory $ E.decodeUtf8 category_name
                            case cc of
                                Nothing -> return $ responseBadRequest "category not exist"
                                _ -> do
                                    let new_maternal_parametr = lookup "new_maternal" $ queryString req
                                    let new_name_parametr = lookup "new_name" $ queryString req
                                    case (new_maternal_parametr,new_name_parametr) of 
                                        (Nothing, Nothing) -> return $ responseBadRequest "no editable parametrs"
                                        (Nothing, Just nnp) -> do 
                                                    result <- editCategoryName category_name nnp
                                                    case result of
                                                            Left bs -> return $ responseBadRequest bs
                                                            Right bs -> return $ responseOk bs

                                        (Just nmp,Nothing) -> do 
                                                    result <-editCategoryMaternal category_name nmp
                                                    case result of
                                                            Left bs -> return $ responseBadRequest bs
                                                            Right bs -> return $ responseOk bs

                                        (Just nmp,Just nnp) -> do
                                                    result <-fullEditCategory category_name nnp nmp
                                                    case result of
                                                            Left bs -> return $ responseBadRequest bs
                                                            Right bs -> return $ responseOk bs


            

categoriesBlock :: BC.ByteString -> [BC.ByteString] -> Request -> IO Response 
categoriesBlock path pathElems req | pathElemsC == 1 = sendCategoriesList req
                                   | pathElemsC == 2 = 
                                       case last pathElems of
                                           "delete_category" -> deleteCategory' req
                                           "create_category" -> createCategory' req
                                           "edit_category" -> editCategory' req
                                           _ -> return $ responseBadRequest "bad request"
                                   | otherwise = return $ responseBadRequest "bad request"


    where pathElemsC = length pathElems