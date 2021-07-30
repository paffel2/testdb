{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
module Usershandle where
import Network.Wai
--import Network.Wai.Handler.Warp
import Network.Wai.Parse
--import Network.HTTP.Types
--import GHC.Generics
--import Data.Aeson
--import Testdatabase
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
--import NewsAndComments
import Logger
import Databaseoperations



login :: Handle -> Request -> IO Response
login hLogger req = do
    (i,_) <- parseRequestBody lbsBackEnd req
    let log = fromMaybe "" (lookup "login" i)
    let pass = fromMaybe "" (lookup "user_password" i)
    check <- authentication hLogger log pass
    case check of
      Left bs -> return $ responseBadRequest bs
      Right bs -> return $ responseOk bs



registration :: Handle -> Request -> IO Response 
registration hLogger req = do
    (i,f) <- parseRequestBody lbsBackEnd req
    let [(_,file)] = f
    let file_contentType = fileContentType file
    --TIO.putStrLn $ E.decodeUtf8 $ fileName file
    let f_name = E.decodeUtf8 $ fromMaybe "" (lookup "f_name" i)
    let l_name = E.decodeUtf8 $ fromMaybe "" (lookup "l_name" i)
    let login = fromMaybe "" (lookup "login" i)
    let password = fromMaybe "" (lookup "password" i)
    if T.length f_name > 50 || T.length l_name > 50 || T.length (E.decodeUtf8 login) > 50 || T.length (E.decodeUtf8 password) > 50
        then do
            logError hLogger "Long parametr for registration"
            return $ responseBadRequest "one or more parameter more then 50 symbols"
        else do
            result <- createUserInDb hLogger login password f_name l_name (fileName file) (fileContentType file) (fileContent file)
            case result of
                Left bs -> return $ responseBadRequest bs
                Right bs -> return $ responseOk bs

deleteUser :: Handle -> Request -> IO Response 
deleteUser hLogger req = do
    let login = fromMaybe Nothing (lookup "login" $ queryString req)
    let token = fromMaybe Nothing (lookup "token" $ queryString req)
    isAdmin <- checkAdmin hLogger $ E.decodeUtf8 $ fromMaybe "" token
    case isAdmin of 
        (False,bs) -> return $ responseBadRequest bs
        (True,_) -> do
                result <- deleteUserFromDb hLogger $ fromMaybe "" login
                case result of                          
                    Left bs' -> return $ responseBadRequest bs'
                    Right bs' -> return $ responseOk bs'
