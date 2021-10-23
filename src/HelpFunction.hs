{-# LANGUAGE OverloadedStrings #-}

module HelpFunction where

import           Config                           (DatabaseConf (db_host, db_login, db_name, db_password, db_port))
import qualified Data.ByteString.Char8            as BC
import           Data.List                        (sort)
import           Data.String                      (IsString (fromString))
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as TIO
import           Data.Time.Calendar               (Day)
import           Database.PostgreSQL.Simple.Types (Query)
import           Network.Wai.Parse                (FileInfo)
import           System.Directory                 (getDirectoryContents)
import           Text.Read                        (readMaybe)
import           Types.Other                      (Id (Id))

myLookup :: Eq a => a -> [(a, b)] -> Maybe a
myLookup _key [] = Nothing
myLookup key ((x, _):xys)
    | key == x = Just key
    | otherwise = myLookup key xys

foundParametr :: BC.ByteString -> [(BC.ByteString, FileInfo c)] -> [FileInfo c]
foundParametr param ((p, c):xs) =
    if p == param
        then c : foundParametr param xs
        else foundParametr param xs
foundParametr _ [] = []

readByteStringToInt :: BC.ByteString -> Maybe Int
readByteStringToInt num = readMaybe $ BC.unpack num

readByteStringListInt :: BC.ByteString -> Maybe [Int]
readByteStringListInt lst = readMaybe $ BC.unpack lst

readByteStringToDay :: BC.ByteString -> Maybe Day
readByteStringToDay bs = readMaybe $ BC.unpack bs

readByteStringToId :: BC.ByteString -> Maybe Id
readByteStringToId num = Id <$> readByteStringToInt num

toQuery :: BC.ByteString -> Query
toQuery s = fromString $ BC.unpack s

dbAddress :: DatabaseConf -> BC.ByteString
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

dbServerAddress :: DatabaseConf -> BC.ByteString
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
    let cleanList = takeWhile (\x -> takeEnd 4 x == ".jpg") list
    return $ sort cleanList

takeEnd :: Int -> [a] -> [a]
takeEnd n x = reverse $ take n (reverse x)

getMaybeLine :: IO (Maybe T.Text)
getMaybeLine = do
    line <- TIO.getLine
    if T.null line
        then return Nothing
        else return $ Just line

saveHead :: [a] -> Maybe a
saveHead []    = Nothing
saveHead (x:_) = Just x
