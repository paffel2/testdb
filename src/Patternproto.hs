{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
module Patternproto where
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Parse
import Network.HTTP.Types
import GHC.Generics
import Data.Aeson
import Testdatabase
import Control.Exception
import qualified Data.Text.Lazy.Encoding as EL
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.Encoding as E
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BC
import Types
import Data.Maybe
import Text.Read
import Control.Applicative
import HelpFunction
import Responses
import NewsAndCommentshandle
--import Categories
--import Users
import Drafts
import Logger
import Config
import Categorieshandle
import Usershandle





protoServ :: IO ()
protoServ = do
    hConfig <- newConfigHandle
    --let hLogger' = newLogHandle hConfig
    confLogger <- getLgConfig  hConfig
    confServer <- getSrConfig hConfig
    let hLogger = Handle (log_priority confLogger) printLog
    --logInfo (newLogHandle confLogger) "Serving..."
    logInfo hLogger "Serving"
    --runSettings  (setMaximumBodyFlush (Just 1048576) $ setPort 8000 defaultSettings) $ appProto hLogger
    runSettings (setMaximumBodyFlush (server_maximum_body_flush confServer) $ setPort (server_port confServer) defaultSettings) $ appProto hLogger

appProto :: Handle -> Application
appProto hLogger req respond 
    | pathHead == "news" = newsMethodBlock hLogger path pathElems req >>= respond
    | pathHead == "login" = login hLogger req >>= respond
    | pathHead == "registration" = registration hLogger req >>= respond
    | pathHead == "deleteUser" = deleteUser hLogger req >>= respond
    {- pathHead == "categories" = categoriesBlock path pathElems req >>= respond
    | pathHead == "drafts" = draftsBlock pathElems req >>= respond
    -- pathHead == "new_draft" = createDraft' req >>= respond -}
    | otherwise = badUrlRespond
    where
        path = BC.tail $ rawPathInfo req
        pathElems = BC.split '/' path
        pathElemC = length pathElems
        pathHead = head pathElems
        badUrlRespond = do
            --logError hLogger "Bad url"
            respond $ responseBadRequest "bad url"