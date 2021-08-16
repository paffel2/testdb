{-# LANGUAGE OverloadedStrings #-}
module Tags where
import Network.Wai
--import Network.Wai.Handler.Warp
--import Network.Wai.Parse
--import Network.HTTP.Types
--import GHC.Generics
import Data.Aeson
--import Testdatabase
--import Control.Exception
--import qualified Data.Text.Lazy.Encoding as EL
--import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.Encoding as E
--import qualified Data.Text as T
--import qualified Data.Text.IO as TIO
--import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BC
import Types
import Data.Maybe
--import Text.Read
--import Control.Applicative
--import HelpFunction
import Responses
--import NewsAndComments
--import Categories
--import Users
import Logger
import FromRequest
import Databaseoperations
import Data.Pool
import Database.PostgreSQL.Simple
import qualified Data.Text as T
--import Network.Wai.Test (request)
--import qualified GHC.Unicode as T
--import Foreign.Marshal (pooledMallocArray)
--import Database.PostgreSQL.Simple



sendTagsList :: Handle -> Pool Connection -> Request -> IO Response
sendTagsList hLogger pool req = do
    tags_list <- getTagsListFromDb hLogger pool page
    case tags_list of
        Left bs -> return $ responseBadRequest bs
        Right tl -> return $ responseOk $ encode tl
    where 
        page = fromMaybe Nothing (lookup "page" $ queryString req)

newTag :: Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response 
newTag hLogger pool token_lifetime req = do
    let token' = E.decodeUtf8 <$> takeToken req
    c_a <- checkAdmin' hLogger pool token_lifetime token'
    case c_a of 
        (False, bs) -> return $ responseBadRequest bs
        (True,_ ) -> do
            let tag_name_param = T.toLower . E.decodeUtf8 <$> fromMaybe Nothing (lookup "tag_name" $ queryString req)
            result <- createTagInDb hLogger pool tag_name_param 
            case result of
                Left bs -> return $ responseBadRequest bs
                Right n -> return $ responseOk $ LBS.fromStrict $ BC.pack $ show n

deleteTag :: Handle -> Pool Connection -> TokenLifeTime -> Request -> IO Response 
deleteTag hLogger pool token_lifetime req = do
    let token' = E.decodeUtf8 <$> takeToken req
    c_a <- checkAdmin' hLogger pool token_lifetime token'
    case c_a of
        (False, bs) -> return $ responseBadRequest bs
        (True,_ ) -> do
            let tag_name_param = T.toLower . E.decodeUtf8 <$> fromMaybe Nothing (lookup "tag_name" $ queryString req)
            result <- deleteTagFromDb hLogger pool tag_name_param 
            case result of
                Left bs -> return $ responseBadRequest bs
                Right bs -> return $ responseOk bs



tagsBlock :: Handle -> Pool Connection -> TokenLifeTime -> [BC.ByteString] -> Request -> IO Response 
tagsBlock hLogger pool token_lifetime pathElems req
    | pathElemsC == 1 = sendTagsList hLogger pool req
    | pathElemsC == 2 = 
        case last pathElems of
            "create_tag" -> newTag hLogger pool token_lifetime req
            "delete_tag" -> deleteTag hLogger pool token_lifetime req
            _ -> return $ responseBadRequest "bad request"
    | otherwise = return $ responseBadRequest "bad request"

    where pathElemsC = length pathElems
