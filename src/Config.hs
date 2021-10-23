{-# LANGUAGE OverloadedStrings #-}

module Config where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Configurator as C
import Data.Time (NominalDiffTime)
import Logger (Priority(..))
import Types

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
    conf <- C.load [C.Optional "config/server.conf"]
    life <- TokenLifeTime <$> C.lookupDefault 86400 conf "token.lifetime"
    host <- C.lookupDefault "" conf "database.host"
    port <- C.lookupDefault "" conf "database.port"
    login <- C.lookupDefault "" conf "database.login"
    password <- C.lookupDefault "" conf "database.password"
    name <- C.lookupDefault "" conf "database.name"
    log' <- C.lookupDefault "Info" conf "logger.priority" :: IO String
    serv_port <- C.lookupDefault 8000 conf "server.port"
    smbf <- C.lookupDefault 1048576 conf "server.maximum_body_flush"
    num_stripes' <- C.lookupDefault 1 conf "pool.num_stripes"
    idle_time' <-
        toEnum . (1000000000000 *) <$> C.lookupDefault 10 conf "pool.idle_time"
    max_resources' <- C.lookupDefault 10 conf "pool.max_resources"
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

data PoolParams =
    PoolParams
        { num_stripes :: Int
        , idle_time :: NominalDiffTime
        , max_resources :: Int
        }
    deriving (Show)
