{-# LANGUAGE OverloadedStrings #-}

module Config where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Configurator as C
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime)
import Logger (Priority(..))
import Types (PoolParams(..), TokenLifeTime(TokenLifeTime))

data ConfigModules =
    ConfigModules
        { lifeTime :: TokenLifeTime
        , db_conf :: DatabaseConf
        , log_priority :: Priority
        , server_conf :: ServerConf
        , pool_params :: PoolParams
        }
    deriving (Show)

getConfig :: IO ConfigModules
getConfig = do
    conf <- C.load [C.Optional "server.conf"]
    life <-
        TokenLifeTime <$> C.lookupDefault 86400 conf (T.pack "token.lifetime") :: IO TokenLifeTime
    host <- C.lookupDefault "" conf (T.pack "database.host") :: IO BC.ByteString
    port <- C.lookupDefault "" conf (T.pack "database.port") :: IO BC.ByteString
    login <-
        C.lookupDefault "" conf (T.pack "database.login") :: IO BC.ByteString
    password <-
        C.lookupDefault "" conf (T.pack "database.password") :: IO BC.ByteString
    name <- C.lookupDefault "" conf (T.pack "database.name") :: IO BC.ByteString
    log' <- C.lookupDefault "Info" conf (T.pack "logger.priority") :: IO String
    serv_port <- C.lookupDefault 8000 conf (T.pack "server.port") :: IO Int
    smbf <-
        C.lookupDefault 1048576 conf (T.pack "server.maximum_body_flush") :: IO Int
    num_stripes' <- C.lookupDefault 1 conf (T.pack "pool.num_stripes")
    idle_time' <-
        toEnum . (1000000000000 *) <$>
        C.lookupDefault 10 conf (T.pack "pool.idle_time")
    max_resources' <- C.lookupDefault 10 conf (T.pack "pool.max_resources")
    return
        ConfigModules
            { lifeTime = life
            , pool_params =
                  PoolParams
                      { num_stripes = num_stripes'
                      , idle_time = idle_time'
                      , max_resources = max_resources'
                      }
            , log_priority =
                  case log' of
                      "Debug" -> Debug
                      "Info" -> Info
                      "Warning" -> Warning
                      "Error" -> Logger.Error
                      _ -> Info
            , server_conf =
                  ServerConf
                      { server_port = serv_port
                      , server_maximum_body_flush = Just smbf
                      }
            , db_conf =
                  DatabaseConf
                      { db_host = host
                      , db_port = port
                      , db_login = login
                      , db_password = password
                      , db_name = name
                      }
            }

data ServerConf =
    ServerConf
        { server_port :: Int
        , server_maximum_body_flush :: Maybe Int
        }
    deriving (Show)

data DatabaseConf =
    DatabaseConf
        { db_host :: BC.ByteString
        , db_port :: BC.ByteString
        , db_login :: BC.ByteString
        , db_password :: BC.ByteString
        , db_name :: BC.ByteString
        }
    deriving (Show)
