{-# LANGUAGE OverloadedStrings #-}
module FromRequest where
import Network.Wai ( Request(queryString) )
import qualified Data.ByteString.Char8 as BC
import Data.Maybe ( fromMaybe )
import Network.Wai.Parse
    ( FileInfo(fileName, fileContentType, fileContent) )
import qualified Data.ByteString.Lazy as LBS
import Types ( Image(Image) )
import Database.PostgreSQL.Simple.Types ( Binary(Binary) )




takeToken :: Request -> Maybe BC.ByteString
takeToken req = fromMaybe Nothing (lookup "token" $ queryString req)
toImage :: FileInfo LBS.ByteString -> Image
toImage file_info = Image (fileName file_info) (fileContentType file_info) (Binary $ fileContent file_info)