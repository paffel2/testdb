{-# LANGUAGE OverloadedStrings #-}
module FromRequest where
import Network.Wai
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
--import Responses



takeToken :: Request -> Maybe BC.ByteString
takeToken req = fromMaybe Nothing (lookup "token" $ queryString req)

{-takeMainImage :: Request -> IO (Network.Wai.Parse.FileInfo c)
takeMainImage req = (parseRequestBodyEx noLimitParseRequestBodyOptions lbsBackEnd req) >>= foundParametr "main_image"
takeDraftId :: Request -> Maybe Int
takeDraftId req = readByteStringToInt $ fromMaybe "" (fromMaybe Nothing (lookup "draft_id" $ queryString req))

takeNewsId :: Request -> Maybe Int
takeNewsId req = readByteStringToInt $ fromMaybe "" (fromMaybe Nothing (lookup "news_id" $ queryString req))-}