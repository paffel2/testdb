{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module FromRequestTests where

import Database.PostgreSQL.Simple.Types (Binary(Binary))
import FromRequest (takeToken, toImage)
import Network.Wai.Internal (Request(Request, queryString))
import Network.Wai.Parse (FileInfo(FileInfo))
import Test.Hspec (describe, hspec, it, shouldBe)
import Types (Image(Image))

fromRequestTests :: IO ()
fromRequestTests =
    hspec $ do
        describe "FromRequest" $ do
            describe "takeToken" $ do
                it "take token from request" $
                    takeToken
                        (Request {queryString = [("token", Just "qwerty")]}) `shouldBe`
                    Just "qwerty"
            describe "toImage" $ do
                it "convert FileInfo to Image" $
                    toImage (FileInfo "image.jpg" "image/jpeg" "some binary") `shouldBe`
                    Image "image.jpg" "image/jpeg" (Binary "some binary")
