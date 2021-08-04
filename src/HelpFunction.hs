{-# LANGUAGE OverloadedStrings #-}
module HelpFunction where
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Network.Wai.Parse
import qualified Data.Text as T
import Database.PostgreSQL.Simple.Types
import Data.Time.Calendar
import Text.Read
import Data.String
import Types
import Database.PostgreSQL.Simple
import qualified Data.ByteString.Lazy as LBS

myLookup :: Eq a => a -> [(a, b)] -> Maybe a
myLookup _key []          =  Nothing
myLookup  key ((x,_):xys)
    | key == x           =  Just key
    | otherwise         =  myLookup key xys


foundParametr :: B.ByteString -> [(B.ByteString, FileInfo c)] -> [FileInfo c]
foundParametr param ((p,c):xs) = if p == param then
                                                c : foundParametr param xs
                                                else foundParametr param xs
foundParametr _ [] = []



toTriple :: [FileInfo c] -> [([Char], [Char], c)]
toTriple = map
      (\ x
         -> (BC.unpack $ fileName x, BC.unpack $ fileContentType x,
             fileContent x))
toImage :: FileInfo LBS.ByteString -> Image'''
toImage file_info = Image''' (fileName file_info) (fileContentType file_info) (Binary $ fileContent file_info)

fstTriple :: (a, b, c) -> a
fstTriple (a,b,c) = a
sndTriple :: (a, b, c) -> b
sndTriple (a,b,c) = b
thrdTriple :: (a, b, c) -> c
thrdTriple (a,b,c) = c


splitOnPunctuationMark :: T.Text -> [T.Text]
splitOnPunctuationMark  = T.splitOn " " 


readByteStringToInt :: BC.ByteString -> Maybe Int
readByteStringToInt num = readMaybe $ BC.unpack num

readByteStringListInt :: BC.ByteString -> Maybe [Int]
readByteStringListInt lst = readMaybe $ BC.unpack lst


takePage :: Int -> [a] -> [a]
takePage p list = Prelude.take 10 $ Prelude.drop ((p-1)*10) list


readByteStringToDay :: BC.ByteString -> Maybe Day
readByteStringToDay bs = readMaybe $ BC.unpack bs



toQuery :: BC.ByteString -> Query 
toQuery s = fromString $ BC.unpack s