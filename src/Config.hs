{-# LANGUAGE OverloadedStrings #-}
module Config where
--import Network.Wai
--import Network.Wai.Handler.Warp
--import Network.Wai.Parse
--import Network.HTTP.Types
--import GHC.Generics
--import Data.Aeson
--import Testdatabase
--import Control.Exception
--import qualified Data.Text.Lazy.Encoding as EL
--import qualified Data.Text.Lazy.IO as TLIO
--import qualified Data.Text.Encoding as E
import qualified Data.Text as T
--import qualified Data.Text.IO as TIO
--import qualified Data.ByteString as B
--import qualified Data.ByteString.Lazy as LBS
--import qualified Data.ByteString.Char8 as BC
--import Types
--import Data.Maybe
--import Text.Read
--import Control.Applicative
--import HelpFunction
--import Responses
--import Control.Monad
import Logger
import qualified Data.Configurator as C
import qualified Data.ByteString.Char8 as BC

data Modules = TK | DB | LG | SR
{-data ConfigModules = 
    Token { lifeTime :: Int }
    | Database { db_host :: String,
                 db_port :: String,
                 db_login :: String,
                 db_password :: String,
                 db_name :: String 
                 }
    | Log { log_priority :: Priority }
    | Server { server_port :: Int,
               server_maximum_body_flush :: Maybe Int }
    deriving (Show)-}
data ConfigModules = 
    Token { lifeTime :: Int }
    | Database { db_host :: BC.ByteString,
                 db_port :: BC.ByteString,
                 db_login :: BC.ByteString,
                 db_password :: BC.ByteString,
                 db_name :: BC.ByteString 
                 }
    | Log { log_priority :: Priority }
    | Server { server_port :: Int,
               server_maximum_body_flush :: Maybe Int }
    deriving (Show)
{-data ConfigModules = 
    Token { lifeTime :: Int }
    | Database { db_host :: String,
                 db_port :: String,
                 db_login :: String,
                 db_password :: String,
                 db_name :: String 
                 }
    | Log { log_priority :: Priority }
    | Server { server_port :: Int,
               server_maximum_body_flush :: Maybe Int }
    deriving (Show)-}

newtype ConfigHandle = ConfigHandle { getConfig :: Modules -> IO ConfigModules} 

getDbConfig, getTkConfig, getLgConfig, getSrConfig :: ConfigHandle -> IO ConfigModules
getDbConfig = (`getConfig` DB)
getTkConfig = (`getConfig` TK)
getLgConfig = (`getConfig` LG)
getSrConfig = (`getConfig` SR)



newConfigHandle :: IO ConfigHandle
newConfigHandle = return $ ConfigHandle {getConfig = getconfig}

getconfig :: Modules -> IO ConfigModules
getconfig module' = do
    --conf <- C.load [C.Optional "server.conf"]
    conf <- C.load [C.Optional "server.conf"]
    --C.display conf
    --print "1"
    case module' of
        TK -> do
            life <- C.lookupDefault 86400 conf (T.pack "token.lifetime") :: IO Int
            return $ Token life
        
      {-DB -> do
          host <- C.lookupDefault "" conf (T.pack "database.host") :: IO String
          port <- C.lookupDefault "" conf (T.pack "database.port") :: IO String
          login <- C.lookupDefault "" conf (T.pack "database.login") :: IO String
          --port <- C.lookupDefault "" conf (T.pack "db_port") :: IO BC.ByteString
          password <- C.lookupDefault "" conf (T.pack "database.password") :: IO String
          name <- C.lookupDefault "" conf (T.pack "database.name") :: IO String
          return $ Database host port login password name-}
        DB -> do
            host <- C.lookupDefault "" conf (T.pack "database.host") :: IO BC.ByteString
            port <- C.lookupDefault "" conf (T.pack "database.port") :: IO BC.ByteString
            login <- C.lookupDefault "" conf (T.pack "database.login") :: IO BC.ByteString
          --port <- C.lookupDefault "" conf (T.pack "db_port") :: IO BC.ByteString
            password <- C.lookupDefault "" conf (T.pack "database.password") :: IO BC.ByteString
            name <- C.lookupDefault "" conf (T.pack "database.name") :: IO BC.ByteString
            return $ Database host port login password name
        LG -> do
            log' <- C.lookupDefault "Info" conf (T.pack "logger.priority") :: IO String 
            case log' of
                "Debug" -> return $ Log Debug
                "Info" -> return $ Log Info 
                "Warning" -> return $ Log Warning
                "Error" -> return $ Log Logger.Error
                _ -> return $ Log Info
        SR -> do
            port <- C.lookupDefault 8000 conf (T.pack "server.port") :: IO Int
            smbf <- C.lookupDefault 1048576 conf (T.pack "server.maximum_body_flush") :: IO Int
            return $ Server port (Just smbf)
      

newLogHandle :: ConfigModules -> Logger.Handle 
newLogHandle conf = Handle (log_priority conf) printLog


tstConfig :: IO ()
tstConfig = do
    conf <- newConfigHandle
    --n <- getDbConfig conf
    n <- getLgConfig conf
    let pr = log_priority n
    --print $ db_host n
    print pr
    print n




