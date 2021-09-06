{-# LANGUAGE OverloadedStrings #-}

module HelpFunctionTests where

import Config (ConfigModules(Database))
import qualified Data.ByteString.Char8 as BC
import HelpFunction
    ( dbAddress
    , dbServerAddress
    , foundParametr
    , myLookup
    , readByteStringListInt
    , readByteStringToInt
    , takeEnd
    )
import Network.Wai.Parse (FileInfo(FileInfo))
import Test.Hspec (describe, hspec, it, shouldBe)

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
                        (Database "localhost" "8000" "root" "admin" "DbName") `shouldBe`
                    "host=localhost port=8000 user='root' password='admin' dbname='DbName'"
            describe "dbServerAddress" $ do
                it "database's server addres from config" $
                    dbServerAddress
                        (Database "localhost" "8000" "root" "admin" "DbName") `shouldBe`
                    "host=localhost port=8000 user='root' password='admin'"
            describe "takeEnd" $ do
                it "take last n elements" $
                    takeEnd 4 "img.jpg" `shouldBe` ".jpg"
                it "take last n elements from a string shorter than n" $
                    takeEnd 100 "img.jpg" `shouldBe` "img.jpg"
{-{-# LANGUAGE OverloadedStrings #-}

module APITests where

import Test.Hspec (describe, hspec, it, shouldBe)
import Vk.API (findRepeatNumber, updateListUsers)

apiTests :: IO ()
apiTests =
    hspec $ do
        describe "API" $ do
            describe "findRepeatNumber" $ do
                it "empty user list" $ findRepeatNumber [] 5 `shouldBe` 1
                it "user in the list" $ findRepeatNumber [(5, 3)] 5 `shouldBe` 3
                it "user is not in the list" $
                    findRepeatNumber [(5, 3)] 6 `shouldBe` 1
            describe "updateListUsers" $ do
                it "empty update" $ updateListUsers [] [] `shouldBe` []
                it "Nothing update" $ updateListUsers [] [Nothing] `shouldBe` []
                it "update empty list" $
                    updateListUsers [] [Just (5, 3)] `shouldBe` [(5, 3)]
                it "update number of repeats" $
                    updateListUsers [(5, 3)] [Just (5, 4)] `shouldBe` [(5, 4)]
                it "update empty list" $
                    updateListUsers [(5, 3)] [Just (4, 3)] `shouldBe`
                    [(5, 3), (4, 3)]-}
