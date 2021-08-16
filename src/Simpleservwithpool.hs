--{-# LANGUAGE OverloadedStrings #-}
module Simpleservwithpool where
{-import Config
import Logger
import Data.Pool
import Network.Wai
import Network.Wai.Handler.Warp
import Database.PostgreSQL.Simple
import qualified Data.ByteString.Char8 as BC
import Responses
import qualified Data.ByteString.Lazy as LBS
import Types
import qualified Data.Text.Encoding as E
import HelpFunction
import Control.Exception
--import Data.Maybe
--import Control.Monad.IO.Class
import Data.Aeson
--import qualified Data.Text as T
import PostgreSqlWithPool


servWithPool :: IO ()
servWithPool = do
    hConfig <- newConfigHandle
    hToken <- getTkConfig hConfig
    confLogger <- getLgConfig  hConfig
    confServer <- getSrConfig hConfig
    confDb <- getDbConfig hConfig
    let hLogger = Handle (log_priority confLogger) printLog
    logInfo hLogger "Serving"
    --runSettings  (setMaximumBodyFlush (Just 1048576) $ setPort 8000 defaultSettings) $ appProto hLogger
    runSettings (setMaximumBodyFlush (server_maximum_body_flush confServer) $ setPort (server_port confServer) defaultSettings) $ appPool hLogger hToken confDb

appPool :: Handle -> ConfigModules -> ConfigModules -> Application
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
            logError hLogger "Bad url"
            respond $ responseBadRequest "bad url"
--"host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
{-Database { db_host :: String,
                 db_port :: String,
                 db_login :: String,
                 db_password :: String,
                 db_name :: String 
                 }-}

---сделать Handle для базы данных



getNewsWithPool :: Handle -> Pool Connection -> IO (Either LBS.ByteString NewsArray')
getNewsWithPool hLogger pool = catch (do 
    logInfo hLogger "Someone try get news list"
    let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name))",
                      " as author_name, take_categories_list(category_id), news_text  ",
                      "from news join authors using (author_id) join users using (user_id)"]
    --rows <- withResource pool $ \conn -> query_ conn q ::IO [GetNews']
    rows <- query_WithPool pool q
    return $ Right (NewsArray' rows)) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        logError hLogger err --"Database error in getting news list"
                                        return $ Left "Database error"
--sendAns :: Either LBS.ByteString NewsArray' -> Response
sendAns :: Handle -> Pool Connection -> IO Response
sendAns hLogger pool =  do
    ans <- getNewsWithPool hLogger pool
    case ans of
      Left bs -> return $ responseBadRequest bs
      Right na -> return $ responseOk $ encode na


{-getNewsFromDb :: Handle -> BC.ByteString -> Maybe BC.ByteString-> IO (Either LBS.ByteString NewsArray')
getNewsFromDb hLogger sortParam pageParam = do
    logInfo hLogger "Someone try get news list"
    let sort' = if sortParam == "" then ""
                else BC.concat [" order by ",sortParam," DESC"]
    let pg = if isNothing pageParam then " limit 10 offset 0"
            else
                BC.concat [" limit 10 offset ", BC.pack $ show $ (fromMaybe 1  (readByteStringToInt (fromMaybe "" pageParam)) - 1)*10 ]
    let q = toQuery $ BC.concat ["select news_id, short_title, date_creation, (concat(first_name, ' ', last_name)) as author_name, take_categories_list(category_id), news_text  from news join authors using (author_id) join users using (user_id)", sort',pg]
    --rows <- query_ conn "select news_id, short_title, date_creation, author_id, category_id, news_text  from news" :: IO [GetNews']
    catch (do
        conn <- connectPostgreSQL "host=localhost port=5432 user='postgres' password='123' dbname='NewsServer'"
        rows <- query_ conn q:: IO [GetNews']
        close conn
        return $ Right (NewsArray' rows)) $ \e -> do
                                        let err = E.decodeUtf8 $ sqlErrorMsg e
                                        logError hLogger err --"Database error in getting news list"
                                        return $ Left "Database error"-}-}