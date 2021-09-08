{-# LANGUAGE OverloadedStrings #-}

module Config where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Configurator as C
import qualified Data.Text as T
import Data.Time.Clock ( NominalDiffTime )
import Logger (Priority(..))

data Modules
    = TK
    | DB
    | LG
    | SR
    | PL

data ConfigModules
    = Token
          { lifeTime :: Int
          }
    | Database
          { db_host :: BC.ByteString
          , db_port :: BC.ByteString
          , db_login :: BC.ByteString
          , db_password :: BC.ByteString
          , db_name :: BC.ByteString
          }
    | Log
          { log_priority :: Priority
          }
    | Server
          { server_port :: Int
          , server_maximum_body_flush :: Maybe Int
          }
    | PoolConf
          { num_stripes :: Int
          , idle_time :: NominalDiffTime
          , max_resources :: Int
          }
    deriving (Show)

newtype ConfigHandle =
    ConfigHandle
        { getConfig :: Modules -> IO ConfigModules
        }

getDbConfig, getTkConfig, getLgConfig, getSrConfig, getPlConfig ::
       ConfigHandle -> IO ConfigModules
getDbConfig = (`getConfig` DB)

getTkConfig = (`getConfig` TK)

getLgConfig = (`getConfig` LG)

getSrConfig = (`getConfig` SR)

getPlConfig = (`getConfig` PL)

newConfigHandle :: IO ConfigHandle
newConfigHandle = return $ ConfigHandle {getConfig = getconfig}

getconfig :: Modules -> IO ConfigModules
getconfig module' = do
    conf <- C.load [C.Optional "server.conf"]
    case module' of
        TK -> do
            life <-
                C.lookupDefault 86400 conf (T.pack "token.lifetime") :: IO Int
            return $ Token life
        DB -> do
            host <-
                C.lookupDefault "" conf (T.pack "database.host") :: IO BC.ByteString
            port <-
                C.lookupDefault "" conf (T.pack "database.port") :: IO BC.ByteString
            login <-
                C.lookupDefault "" conf (T.pack "database.login") :: IO BC.ByteString
            password <-
                C.lookupDefault "" conf (T.pack "database.password") :: IO BC.ByteString
            name <-
                C.lookupDefault "" conf (T.pack "database.name") :: IO BC.ByteString
            return $ Database host port login password name
        LG -> do
            log' <-
                C.lookupDefault "Info" conf (T.pack "logger.priority") :: IO String
            case log' of
                "Debug" -> return $ Log Debug
                "Info" -> return $ Log Info
                "Warning" -> return $ Log Warning
                "Error" -> return $ Log Logger.Error
                _ -> return $ Log Info
        SR -> do
            port <- C.lookupDefault 8000 conf (T.pack "server.port") :: IO Int
            smbf <-
                C.lookupDefault
                    1048576
                    conf
                    (T.pack "server.maximum_body_flush") :: IO Int
            return $ Server port (Just smbf)
        PL -> do
            num_sparks' <- C.lookupDefault 1 conf (T.pack "pool.num_stripes")
            idle_time' <-
                toEnum . (1000000000000 *) <$>
                C.lookupDefault 10 conf (T.pack "pool.idle_time")
            --let idle_time'' = toEnum idle_time'
            max_resources' <-
                C.lookupDefault 10 conf (T.pack "pool.max_resources")
            return $ PoolConf num_sparks' idle_time' max_resources'
