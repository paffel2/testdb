{-# LANGUAGE OverloadedStrings #-}

module HelpFunction where

import Config (ConfigModules(db_host, db_login, db_name, db_password, db_port))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromMaybe)
import Data.String (IsString(fromString))
import Data.Time.Calendar (Day)
import Database.PostgreSQL.Simple.Types (Query)
import Network.Wai.Parse (FileInfo)
import Text.Read (readMaybe)
import System.Directory ( getDirectoryContents )
import Data.List ( sort )

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
dbServerAddress :: ConfigModules -> BC.ByteString 
dbServerAddress confDb = 
    BC.concat
        [ "host="
        , db_host confDb
        , " port="
        , db_port confDb
        , " user='"
        , db_login confDb
        , "' password='"
        , db_password confDb
        , "'" 
        ]


getFiles :: IO [FilePath]
getFiles = do
    list <- getDirectoryContents "sql/img"
    let cleanList = takeWhile (\x -> takeEnd 4 x == ".jpg" ) list
    return $ sort cleanList


takeEnd :: Int -> [a] -> [a]
takeEnd n x = reverse $ take n (reverse x) 
tstTakeEnd :: [Char]
tstTakeEnd = takeEnd 4 "img (1).jpg"

sumStrings :: String -> [String] -> [String]
sumStrings y (x:xs) = (y ++ x) : sumStrings y xs
sumStrings _ [] = [] 


tst :: [String]
tst = sumStrings "123" ["13","24"]

toPairList :: [a] -> [b] -> [(a, b)]
toPairList (x:xs) (y:ys) = (x,y) : toPairList xs ys
toPairList _ [] = []
toPairList [] _ = []