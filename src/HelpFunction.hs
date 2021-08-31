{-# LANGUAGE OverloadedStrings #-}

module HelpFunction where

import Config (ConfigModules(db_host, db_login, db_name, db_password, db_port))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.String (IsString(fromString))
import Data.Time.Calendar (Day)
import Database.PostgreSQL.Simple.Types (Query)
import Network.Wai.Parse (FileInfo)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

myLookup :: Eq a => a -> [(a, b)] -> Maybe a
myLookup _key [] = Nothing
myLookup key ((x, _):xys)
    | key == x = Just key
    | otherwise = myLookup key xys

foundParametr :: B.ByteString -> [(B.ByteString, FileInfo c)] -> [FileInfo c]
foundParametr param ((p, c):xs) =
    if p == param
        then c : foundParametr param xs
        else foundParametr param xs
foundParametr _ [] = []

readByteStringToInt :: BC.ByteString -> Maybe Int
readByteStringToInt num = readMaybe $ BC.unpack num


readByteStringToInt' :: BC.ByteString -> Int 
readByteStringToInt' num = fromMaybe (-1) $ readByteStringToInt num

readByteStringListInt :: BC.ByteString -> Maybe [Int]
readByteStringListInt lst = readMaybe $ BC.unpack lst

readByteStringToDay :: BC.ByteString -> Maybe Day
readByteStringToDay bs = readMaybe $ BC.unpack bs

toQuery :: BC.ByteString -> Query
toQuery s = fromString $ BC.unpack s

tagsToQueryTagList :: BC.ByteString -> BC.ByteString
tagsToQueryTagList tagsString =
    BC.intercalate "," $ map (\x -> BC.concat ["'", x, "'"]) tagBSList
  where
    tagBSList = BC.split ' ' tagsString

dbAddress :: ConfigModules -> BC.ByteString
dbAddress confDb =
    BC.concat
        [ "host="
        , db_host confDb
        , " port="
        , db_port confDb
        , " user='"
        , db_login confDb
        , "' password='"
        , db_password confDb
        , "' dbname='"
        , db_name confDb
        , "'"
        ]
