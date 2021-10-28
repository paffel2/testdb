{-# LANGUAGE OverloadedStrings #-}

module HelpFunctionTests where

import Config (DatabaseConf(DatabaseConf))
import qualified Data.ByteString.Char8 as BC
import Data.Time.Calendar (fromGregorian)
import HelpFunction
    ( dbAddress
    , dbServerAddress
    , foundParametr
    , myLookup
    , readByteStringListInt
    , readByteStringToDay
    , readByteStringToId
    , readByteStringToInt
    , saveHead
    , takeEnd
    )
import Network.Wai.Parse (FileInfo(FileInfo))
import Test.Hspec (describe, hspec, it, shouldBe)
import Types.Other (Id(Id))

functionsTests :: IO ()
functionsTests =
    hspec $ do
        describe "HelpFunctions" $ do
            describe "MyLookup" $ do
                it "find key" $
                    myLookup
                        ("key2" :: String)
                        [("key1", 1 :: Int), ("key2", 2), ("key3", 3)] `shouldBe`
                    Just "key2"
                it "not found key" $
                    myLookup
                        ("key4" :: String)
                        [("key1", 1 :: Int), ("key2", 2), ("key3", 3)] `shouldBe`
                    Nothing
                it "working with empty list" $
                    myLookup ("key2" :: String) [] `shouldBe` Nothing
            describe "foundParameter" $ do
                it "find parameter" $
                    foundParametr
                        "main_image"
                        [ ("other_image", FileInfo "1" "1" ("text" :: String))
                        , ("main_image", FileInfo "2" "2" "image")
                        ] `shouldBe`
                    [FileInfo "2" "2" "image"]
                it "parameter not found" $
                    foundParametr
                        "main_music"
                        [ ("other_image", FileInfo "1" "1" ("text" :: String))
                        , ("main_image", FileInfo "2" "2" "image")
                        ] `shouldBe`
                    []
                it "working with empty list" $
                    foundParametr
                        "main_music"
                        ([] :: [(BC.ByteString, FileInfo String)]) `shouldBe`
                    []
            describe "readByteStringToInt" $ do
                it "read int" $ readByteStringToInt "10" `shouldBe` Just 10
                it "read another" $
                    readByteStringToInt "ByteString" `shouldBe` Nothing
            describe "readByteStringListInt" $ do
                it "read int" $
                    readByteStringListInt "[1, 2, 3]" `shouldBe` Just [1, 2, 3]
                it "read another" $
                    readByteStringListInt "[1, a, 3]" `shouldBe` Nothing
            describe "dbAddress" $ do
                it "db addres from config" $
                    dbAddress
                        (DatabaseConf "localhost" "8000" "root" "admin" "DbName") `shouldBe`
                    "host=localhost port=8000 user='root' password='admin' dbname='DbName'"
            describe "dbServerAddress" $ do
                it "database's server addres from config" $
                    dbServerAddress
                        (DatabaseConf "localhost" "8000" "root" "admin" "DbName") `shouldBe`
                    "host=localhost port=8000 user='root' password='admin'"
            describe "takeEnd" $ do
                it "take last n elements" $
                    takeEnd 4 "img.jpg" `shouldBe` ".jpg"
                it "take last n elements from a string shorter than n" $
                    takeEnd 100 "img.jpg" `shouldBe` "img.jpg"
            describe "readByteStringToDay" $ do
                it "read day" $
                    readByteStringToDay "2021-09-01" `shouldBe`
                    Just (fromGregorian 2021 09 01)
                it "read another" $
                    readByteStringToDay "something" `shouldBe` Nothing
            describe "readByteStringToId" $ do
                it "read Id" $ readByteStringToId "3" `shouldBe` Just (Id 3)
                it "read another" $
                    readByteStringToId "something" `shouldBe` Nothing
            describe "saveHead" $ do
                it "return head of list" $ saveHead [1, 2, 3] `shouldBe` Just 1
                it "return nothing" $
                    saveHead [] `shouldBe` (Nothing :: Maybe Int)
