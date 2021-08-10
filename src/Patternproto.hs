{-# LANGUAGE OverloadedStrings #-}
module Patternproto where
import Network.Wai
import Network.Wai.Handler.Warp
--import Network.Wai.Parse
--import Network.HTTP.Types
--import GHC.Generics
--import Data.Aeson
--import Testdatabase
--import Control.Exception
--import qualified Data.Text.Lazy.Encoding as EL
--import qualified Data.Text.Lazy.IO as TLIO
--import qualified Data.Text.Encoding as E
--import qualified Data.Text as T
--import qualified Data.Text.IO as TIO
--import qualified Data.ByteString as B
--import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BC
import Types
--import Data.Maybe
--import Text.Read
--import Control.Applicative
--import HelpFunction
import Responses
import NewsAndCommentshandle
--import Categories
--import Users
--import Drafts
import qualified Logger as L
import Config
import Categorieshandle
import Usershandle
import Draftshandle
import Data.Pool
import Database.PostgreSQL.Simple



----ДОБАВИТЬ POOL

protoServ :: IO ()
protoServ = do
    hConfig <- newConfigHandle
    confToken <- getTkConfig hConfig
    confLogger <- getLgConfig  hConfig
    confServer <- getSrConfig hConfig
    confDb <- getDbConfig hConfig
    let db_address = BC.concat ["host=", db_host confDb, " port=", db_port confDb, " user='", 
                                db_login confDb,"' password='",db_password confDb,"' dbname='",db_name confDb,"'"]
    let token_lifetime = lifeTime confToken
    let hLogger = L.Handle (log_priority confLogger) L.printLog
    L.logInfo hLogger "Serving"
    --runSettings  (setMaximumBodyFlush (Just 1048576) $ setPort 8000 defaultSettings) $ appProto hLogger
    --runSettings (setMaximumBodyFlush (server_maximum_body_flush confServer) $ setPort (server_port confServer) defaultSettings) $ appProto hLogger confToken confDb
    runSettings (setMaximumBodyFlush (server_maximum_body_flush confServer) $ setPort (server_port confServer) defaultSettings) $ app hLogger db_address token_lifetime
    


{-appProto :: L.Handle -> ConfigModules -> ConfigModules -> Application
appProto hLogger confToken confDb req respond 
    | pathHead == "news" = newsMethodBlock hLogger db_address pathElems req >>= respond
    | pathHead == "login" = login hLogger req >>= respond
    | pathHead == "registration" = registration hLogger req >>= respond
    | pathHead == "deleteUser" = deleteUser hLogger req >>= respond
    | pathHead == "categories" = categoriesBlock hLogger pathElems req >>= respond
    | pathHead == "profile" = profile hLogger req >>= respond
    -- pathHead == "drafts" = draftsBlock hLogger pathElems req >>= respond
    | pathHead == "new_draft" = createDraft hLogger req >>= respond 
    -- pathHead == "tags" = tagsBlock hLogger pool req >>= respond
    | otherwise = badUrlRespond
    where
        path = BC.tail $ rawPathInfo req
        pathElems = BC.split '/' path
        --pathElemC = length pathElems
        pathHead = head pathElems
        db_address = BC.concat ["host=", db_host confDb, " port=", db_port confDb, " user='", 
                                db_login confDb,"' password='",db_password confDb,"' dbname='",db_name confDb,"'"]
        token_lifetime = lifeTime confToken
        badUrlRespond = do
            --logError hLogger "Bad url"
            respond $ responseBadRequest "bad url"-}


app :: L.Handle -> DatabaseAddress -> TokenLifeTime -> Application
app hLogger db_address token_lifetime req respond = do
    pool <- createPool (connectPostgreSQL db_address) close 1 5 10
    case pathHead of
        "news" -> newsMethodBlock hLogger pool token_lifetime pathElems req >>= respond
        "login" -> login hLogger pool req >>= respond
        "registration" -> registration hLogger pool req >>= respond
        "deleteUser" -> deleteUser hLogger pool token_lifetime req >>= respond
        "categories" -> categoriesBlock hLogger pool token_lifetime pathElems req >>= respond
        "profile" -> profile hLogger pool token_lifetime req >>= respond
        "drafts" -> draftsBlock hLogger pool token_lifetime pathElems req >>= respond
        "new_draft" -> createDraft hLogger pool token_lifetime req >>= respond 
        "tags" -> respond $ responseOk "in develop" --tagsBlock hLogger pool req >>= respond
        _ -> badUrlRespond
    where
        path = BC.tail $ rawPathInfo req
        pathElems = BC.split '/' path
        pathHead = head pathElems
        badUrlRespond = do
            L.logError hLogger "Bad url"
            respond $ responseBadRequest "bad url"


{-appPool :: Handle -> ConfigModules -> ConfigModules -> Application
appPool hLogger hToken confDb req respond  = do
    let db_addres = BC.concat ["host=", db_host confDb, " port=", db_port confDb, " user='", 
                                db_login confDb,"' password='",db_password confDb,"' dbname='",db_name confDb,"'"]
    pool <- createPool (connectPostgreSQL db_addres) close 1 5 10
    case pathHead of 
        "news" -> sendAns hLogger pool >>= respond
        _ -> badUrlRespond
    where
        path = BC.tail $ rawPathInfo req
        pathElems = BC.split '/' path
        --pathElemC = length pathElems
        pathHead = head pathElems
        badUrlRespond = do
            --logError hLogger "Bad url"
            respond $ responseBadRequest "bad url"-}